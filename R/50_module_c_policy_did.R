challenge_to_binary <- function(x) {
  txt <- tolower(trimws(as_label_text(x)))
  dplyr::case_when(
    txt %in% c("a slight challenge", "a moderate challenge", "a major challenge") ~ 1L,
    txt %in% c("not a challenge", "not applicable") ~ 0L,
    TRUE ~ NA_integer_
  )
}

text_match_binary <- function(x, pattern) {
  txt <- tolower(trimws(as.character(x)))
  out <- ifelse(is.na(txt) | txt == "", NA_integer_, as.integer(stringr::str_detect(txt, pattern)))
  as.integer(out)
}

weighted_rate <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(as.numeric(x[keep]), w[keep])
}

prepare_module_c_mechanism_data <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "lits")) {
  path <- select_existing_file(c(file.path(raw_dir, "lits_iv_dta", "lits_iv.dta")))
  if (is.na(path)) {
    return(tibble::tibble())
  }

  d <- haven::read_dta(
    path,
    col_select = c(
      country, region, int_gender, urbanity,
      q110b, q111b,
      q713, q714, q715a, q715b, q715c, q716, q717, q718a, q718b, q718c, q718d, q718e,
      weight_pop, weight
    )
  ) %>%
    dplyr::mutate(
      country = as.character(haven::as_factor(country)),
      region = as.character(haven::as_factor(region))
    ) %>%
    dplyr::filter(country == "Uzbekistan") %>%
    dplyr::mutate(
      father_level = map_education_level(as_label_text(q110b)),
      mother_level = map_education_level(as_label_text(q111b)),
      father_years = education_level_to_years(father_level),
      mother_years = education_level_to_years(mother_level),
      parent_years_schooling = rowMeans(cbind(father_years, mother_years), na.rm = TRUE),
      parent_years_schooling = dplyr::if_else(is.nan(parent_years_schooling), NA_real_, parent_years_schooling),
      gender = coerce_gender_label(as_label_text(int_gender)),
      urban = coerce_urban_binary(as_label_text(urbanity)),
      weight_final = clean_numeric(dplyr::coalesce(weight_pop, weight)),
      child_enrolled_pre_covid = yes_no_to_binary(as_label_text(q713)),
      in_mechanism_sample = child_enrolled_pre_covid == 1L,
      education_stopped_covid = yes_no_to_binary(as_label_text(q714)),
      switched_online = yes_no_to_binary(as_label_text(q715a)),
      school_closed_no_online = yes_no_to_binary(as_label_text(q715b)),
      switched_hybrid = yes_no_to_binary(as_label_text(q715c)),
      support_text = tolower(as_label_text(q716)),
      support_mother = text_match_binary(support_text, "mother"),
      support_father = text_match_binary(support_text, "father"),
      support_grand_relatives = text_match_binary(support_text, "grand|relative"),
      no_support_needed = text_match_binary(support_text, "no additional support was needed"),
      no_support_available = text_match_binary(support_text, "no-one could provide additional support"),
      device_text = tolower(as_label_text(q717)),
      shared_device = text_match_binary(device_text, "shared with other members"),
      dedicated_device = text_match_binary(device_text, "only used by this child|provided by the school"),
      challenge_internet = challenge_to_binary(q718a),
      challenge_device = challenge_to_binary(q718b),
      challenge_cost = challenge_to_binary(q718c),
      challenge_tech = challenge_to_binary(q718d),
      challenge_balance = challenge_to_binary(q718e),
      any_remote_challenge = dplyr::if_else(
        rowSums(!is.na(cbind(challenge_internet, challenge_device, challenge_cost, challenge_tech, challenge_balance))) > 0,
        as.integer(rowSums(cbind(challenge_internet, challenge_device, challenge_cost, challenge_tech, challenge_balance), na.rm = TRUE) > 0),
        NA_integer_
      )
    ) %>%
    dplyr::mutate(
      region = factor(region),
      gender = factor(gender),
      urban = as.integer(urban)
    )

  d
}

build_module_c_coverage <- function(df, vars) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(vars, function(v) {
    x <- df[[v]]
    n_non_missing <- sum(!is.na(x))
    tibble::tibble(
      variable = v,
      n_total = nrow(df),
      n_non_missing = n_non_missing,
      share_non_missing = n_non_missing / nrow(df),
      n_unique_non_missing = dplyr::n_distinct(x, na.rm = TRUE)
    )
  })
}

build_module_c_summary <- function(df) {
  outcome_map <- c(
    education_stopped_covid = "Stopped education due to COVID",
    switched_online = "Switched to online learning",
    school_closed_no_online = "School closed without online learning",
    switched_hybrid = "Switched to hybrid learning",
    support_mother = "Learning support from mother",
    support_father = "Learning support from father",
    support_grand_relatives = "Learning support from grandparents/relatives",
    no_support_needed = "No additional support needed",
    shared_device = "Child used shared device",
    any_remote_challenge = "Any remote-learning challenge",
    challenge_internet = "Internet challenge",
    challenge_device = "Device challenge",
    challenge_cost = "Cost challenge",
    challenge_tech = "Technology-learning challenge",
    challenge_balance = "Work/home-school balance challenge"
  )

  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  long <- df %>%
    dplyr::select(weight_final, parent_low_edu, dplyr::all_of(names(outcome_map))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(names(outcome_map)), names_to = "outcome", values_to = "value")

  overall <- long %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      estimate = weighted_rate(value, weight_final),
      n_non_missing = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(group = "overall", group_value = "all")

  by_parent <- long %>%
    dplyr::filter(!is.na(parent_low_edu)) %>%
    dplyr::group_by(outcome, parent_low_edu) %>%
    dplyr::summarise(
      estimate = weighted_rate(value, weight_final),
      n_non_missing = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      group = "parent_education",
      group_value = dplyr::if_else(parent_low_edu == 1L, "lower_or_equal_median_parent_edu", "above_median_parent_edu")
    ) %>%
    dplyr::select(-parent_low_edu)

  dplyr::bind_rows(overall, by_parent) %>%
    dplyr::mutate(
      outcome_label = unname(outcome_map[outcome]),
      .after = outcome
    ) %>%
    dplyr::select(outcome, outcome_label, group, group_value, estimate, n_non_missing)
}

apply_parent_education_split <- function(df, split_rule = "median") {
  if (nrow(df) == 0) {
    return(df %>% dplyr::mutate(parent_low_edu = NA_integer_, split_rule = split_rule, split_threshold = NA_real_))
  }

  split_threshold <- dplyr::case_when(
    split_rule == "median" ~ stats::median(df$parent_years_schooling, na.rm = TRUE),
    split_rule == "leq_11" ~ 11,
    split_rule == "leq_9" ~ 9,
    TRUE ~ NA_real_
  )

  if (is.na(split_threshold)) {
    stop("Unknown split_rule: ", split_rule)
  }

  df %>%
    dplyr::mutate(
      parent_low_edu = dplyr::case_when(
        is.na(parent_years_schooling) ~ NA_integer_,
        parent_years_schooling <= split_threshold ~ 1L,
        TRUE ~ 0L
      ),
      split_rule = split_rule,
      split_threshold = split_threshold
    )
}

fit_module_c_outcome_model <- function(df, outcome_var, use_weights = TRUE) {
  model_df <- df %>%
    dplyr::filter(
      !is.na(.data[[outcome_var]]),
      !is.na(parent_low_edu),
      !is.na(urban),
      !is.na(gender),
      !is.na(region)
    )

  if (use_weights) {
    model_df <- model_df %>%
      dplyr::filter(!is.na(weight_final), weight_final > 0)
  }

  if (nrow(model_df) < 80 || dplyr::n_distinct(model_df[[outcome_var]]) < 2) {
    return(NULL)
  }

  fml <- stats::as.formula(
    paste0(outcome_var, " ~ parent_low_edu + urban + gender + parent_low_edu:urban | region")
  )

  fit <- if (use_weights) {
    fixest::feglm(
      fml = fml,
      family = "logit",
      data = model_df,
      vcov = ~region,
      weights = ~weight_final
    )
  } else {
    fixest::feglm(
      fml = fml,
      family = "logit",
      data = model_df,
      vcov = ~region
    )
  }

  list(
    model = fit,
    formula = deparse(fml, width.cutoff = 500),
    n_used = nrow(model_df),
    use_weights = use_weights
  )
}

fit_module_c_mechanisms <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "lits")) {
  prepared <- prepare_module_c_mechanism_data(raw_dir)
  if (nrow(prepared) == 0) {
    message("LiTS IV data not available for Module C mechanisms. Returning empty output.")
    return(list(
      prepared_data = tibble::tibble(),
      base_sample_data = tibble::tibble(),
      analysis_data = tibble::tibble(),
      summary = tibble::tibble(),
      coverage = tibble::tibble(),
      sample_overview = tibble::tibble(),
      models = list(),
      formulae = tibble::tibble()
    ))
  }

  mech_df_base <- prepared %>% dplyr::filter(in_mechanism_sample)
  mech_df <- apply_parent_education_split(mech_df_base, split_rule = "median")
  median_parent_years <- unique(mech_df$split_threshold)[1]

  sample_overview <- tibble::tibble(
    step = c(
      "Uzbekistan LiTS IV respondents",
      "Respondents with child module eligibility info (q713 non-missing)",
      "Respondents with child enrolled pre-COVID (q713 = yes)",
      "Mechanism sample with non-missing parental schooling",
      "Parental schooling median threshold (years)",
      "Baseline low-parent-education group size",
      "Baseline higher-parent-education group size"
    ),
    n = c(
      nrow(prepared),
      sum(!is.na(prepared$child_enrolled_pre_covid)),
      sum(prepared$in_mechanism_sample, na.rm = TRUE),
      sum(mech_df$in_mechanism_sample & !is.na(mech_df$parent_years_schooling), na.rm = TRUE),
      median_parent_years,
      sum(mech_df$parent_low_edu == 1, na.rm = TRUE),
      sum(mech_df$parent_low_edu == 0, na.rm = TRUE)
    )
  )

  summary_tbl <- build_module_c_summary(mech_df)

  coverage_vars <- c(
    "parent_years_schooling", "parent_low_edu", "urban", "gender", "weight_final",
    "education_stopped_covid", "switched_online", "school_closed_no_online", "switched_hybrid",
    "support_mother", "support_father", "support_grand_relatives", "no_support_needed",
    "shared_device", "any_remote_challenge", "challenge_internet", "challenge_device",
    "challenge_cost", "challenge_tech", "challenge_balance"
  )
  coverage_tbl <- build_module_c_coverage(mech_df, coverage_vars)

  model_specs <- tibble::tibble(
    model_name = c(
      "m1_switched_online",
      "m2_school_closed_no_online",
      "m3_education_stopped_covid",
      "m4_any_remote_challenge"
    ),
    outcome = c(
      "switched_online",
      "school_closed_no_online",
      "education_stopped_covid",
      "any_remote_challenge"
    )
  )

  models <- list()
  formula_rows <- list()
  for (i in seq_len(nrow(model_specs))) {
    model_name <- model_specs$model_name[[i]]
    outcome <- model_specs$outcome[[i]]
    fit_obj <- fit_module_c_outcome_model(mech_df, outcome, use_weights = TRUE)
    if (is.null(fit_obj)) {
      next
    }
    models[[model_name]] <- fit_obj$model
    formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
      model = model_name,
      outcome = outcome,
      formula = fit_obj$formula,
      n_used = fit_obj$n_used
    )
  }

  scenario_specs <- tibble::tribble(
    ~scenario_id, ~split_rule, ~use_weights,
    "baseline_weighted_median", "median", TRUE,
    "unweighted_median", "median", FALSE,
    "weighted_leq11", "leq_11", TRUE,
    "weighted_leq9", "leq_9", TRUE
  )

  scenario_rows <- list()
  robust_rows <- list()
  row_id <- 0L
  for (s in seq_len(nrow(scenario_specs))) {
    sid <- scenario_specs$scenario_id[[s]]
    split_rule <- scenario_specs$split_rule[[s]]
    use_w <- scenario_specs$use_weights[[s]]
    df_s <- apply_parent_education_split(mech_df_base, split_rule = split_rule)
    split_threshold <- unique(df_s$split_threshold)[1]

    scenario_rows[[length(scenario_rows) + 1]] <- tibble::tibble(
      scenario_id = sid,
      split_rule = split_rule,
      split_threshold = split_threshold,
      use_weights = use_w,
      n_total = nrow(df_s),
      n_parent_non_missing = sum(!is.na(df_s$parent_years_schooling)),
      n_low_group = sum(df_s$parent_low_edu == 1, na.rm = TRUE),
      n_high_group = sum(df_s$parent_low_edu == 0, na.rm = TRUE)
    )

    for (i in seq_len(nrow(model_specs))) {
      model_name <- model_specs$model_name[[i]]
      outcome <- model_specs$outcome[[i]]
      fit_obj <- fit_module_c_outcome_model(df_s, outcome, use_weights = use_w)

      if (is.null(fit_obj)) {
        row_id <- row_id + 1L
        robust_rows[[row_id]] <- tibble::tibble(
          scenario_id = sid,
          split_rule = split_rule,
          split_threshold = split_threshold,
          use_weights = use_w,
          model = model_name,
          outcome = outcome,
          term = NA_character_,
          estimate = NA_real_,
          std.error = NA_real_,
          statistic = NA_real_,
          p.value = NA_real_,
          n_used = NA_integer_,
          status = "not_estimable"
        )
      } else {
        tid <- broom::tidy(fit_obj$model) %>%
          dplyr::mutate(
            scenario_id = sid,
            split_rule = split_rule,
            split_threshold = split_threshold,
            use_weights = use_w,
            model = model_name,
            outcome = outcome,
            n_used = fit_obj$n_used,
            status = "estimated",
            .before = 1
          )
        row_id <- row_id + 1L
        robust_rows[[row_id]] <- tid
      }
    }
  }

  robust_scenarios <- if (length(scenario_rows) == 0) tibble::tibble() else dplyr::bind_rows(scenario_rows)
  robust_coefs <- if (length(robust_rows) == 0) tibble::tibble() else dplyr::bind_rows(robust_rows)
  formulae_tbl <- if (length(formula_rows) == 0) tibble::tibble() else dplyr::bind_rows(formula_rows)

  list(
    prepared_data = prepared,
    base_sample_data = mech_df_base,
    analysis_data = mech_df,
    summary = summary_tbl,
    coverage = coverage_tbl,
    sample_overview = sample_overview,
    models = models,
    formulae = formulae_tbl,
    robustness_scenarios = robust_scenarios,
    robustness_coefficients = robust_coefs
  )
}
