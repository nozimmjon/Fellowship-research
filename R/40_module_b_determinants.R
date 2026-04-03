MODULE_B_PREDETERMINED_CONTROLS <- c("urban", "female")
MODULE_B_POTENTIALLY_ENDOGENOUS_CONTROLS <- c(
  "hh_income_proxy",
  "migration_exposure",
  "multigenerational_hh"
)

prepare_module_b_data <- function(df) {
  needed <- c(
    "wave_year", "own_years_schooling", "parent_years_schooling",
    "own_ed_level", "parent_ed_level", "region", "cohort", "sample_weight"
  )
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  out <- df %>%
    dplyr::mutate(
      wave_year = suppressWarnings(as.integer(wave_year)),
      wave_year_fe = factor(wave_year),
      own_years_schooling = suppressWarnings(as.numeric(own_years_schooling)),
      parent_years_schooling = suppressWarnings(as.numeric(parent_years_schooling)),
      sample_weight = suppressWarnings(as.numeric(sample_weight)),
      sample_weight = dplyr::if_else(is.na(sample_weight) | sample_weight <= 0, NA_real_, sample_weight),
      hh_income_proxy = suppressWarnings(as.numeric(hh_income_proxy)),
      migration_exposure = suppressWarnings(as.numeric(migration_exposure)),
      multigenerational_hh = suppressWarnings(as.numeric(multigenerational_hh)),
      urban = dplyr::case_when(
        urban %in% c(1L, "1", "urban", "Urban", TRUE) ~ 1,
        urban %in% c(0L, "0", "rural", "Rural", FALSE) ~ 0,
        TRUE ~ suppressWarnings(as.numeric(urban))
      ),
      gender = dplyr::case_when(
        tolower(as.character(gender)) %in% c("male", "m", "1") ~ "male",
        tolower(as.character(gender)) %in% c("female", "f", "2", "0") ~ "female",
        TRUE ~ NA_character_
      ),
      female = dplyr::if_else(gender == "female", 1, 0, missing = NA_real_),
      wave2022 = dplyr::if_else(wave_year == 2022L, 1, 0, missing = NA_real_),
      own_ed_level = normalize_ed_level(own_ed_level),
      parent_ed_level = normalize_ed_level(parent_ed_level),
      own_ed_score = match(own_ed_level, EDUCATION_LEVELS),
      parent_ed_score = match(parent_ed_level, EDUCATION_LEVELS),
      cohort = factor(cohort),
      region = factor(region),
      parent_ed_level = factor(parent_ed_level, levels = EDUCATION_LEVELS, ordered = TRUE)
    ) %>%
    dplyr::filter(
      !is.na(sample_weight),
      !is.na(region),
      !is.na(cohort),
      !is.na(wave_year_fe),
      !is.na(own_ed_score),
      !is.na(parent_ed_score)
    )

  if (nrow(out) == 0) {
    return(tibble::tibble())
  }

  out <- out %>%
    dplyr::group_by(wave_year_fe) %>%
    dplyr::mutate(
      own_rank = weighted_rank(own_ed_score, sample_weight),
      parent_rank = weighted_rank(parent_ed_score, sample_weight)
    ) %>%
    dplyr::ungroup()

  out <- out %>%
    dplyr::mutate(
      hh_income_proxy = dplyr::if_else(!is.na(hh_income_proxy) & hh_income_proxy > 0, log(hh_income_proxy), NA_real_)
    ) %>%
    dplyr::group_by(wave_year_fe) %>%
    dplyr::mutate(
      hh_income_proxy = dplyr::if_else(
        !is.na(hh_income_proxy) & stats::sd(hh_income_proxy, na.rm = TRUE) > 0,
        (hh_income_proxy - mean(hh_income_proxy, na.rm = TRUE)) / stats::sd(hh_income_proxy, na.rm = TRUE),
        hh_income_proxy
      )
    ) %>%
    dplyr::ungroup()

  out %>%
    dplyr::mutate(
      upward_any = as.integer(own_ed_score > parent_ed_score),
      persist_same = as.integer(own_ed_score == parent_ed_score),
      low_parent_sample = as.integer(parent_ed_score <= match("upper_secondary", EDUCATION_LEVELS))
    )
}

build_covariate_coverage <- function(df, covariates) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(covariates, function(v) {
    x <- df[[v]]
    non_missing <- sum(!is.na(x))
    uniq <- dplyr::n_distinct(x, na.rm = TRUE)
    tibble::tibble(
      covariate = v,
      n_total = nrow(df),
      n_non_missing = non_missing,
      share_non_missing = non_missing / nrow(df),
      n_unique_non_missing = uniq
    )
  })
}

is_covariate_informative <- function(
    df,
    covariate,
    min_non_missing_share = 0.2,
    min_wave_non_missing_share = 0.1,
    wave_var = "wave_year"
) {
  if (!(covariate %in% names(df))) {
    return(FALSE)
  }
  x <- df[[covariate]]
  non_missing_share <- sum(!is.na(x)) / nrow(df)
  unique_non_missing <- dplyr::n_distinct(x, na.rm = TRUE)
  if (!(non_missing_share >= min_non_missing_share && unique_non_missing >= 2)) {
    return(FALSE)
  }

  if (!(wave_var %in% names(df))) {
    return(TRUE)
  }

  wave_cover <- df %>%
    dplyr::group_by(.data[[wave_var]]) %>%
    dplyr::summarise(share = sum(!is.na(.data[[covariate]])) / dplyr::n(), .groups = "drop")

  if (nrow(wave_cover) == 0) {
    return(FALSE)
  }

  all(wave_cover$share >= min_wave_non_missing_share)
}

rhs_with_optional <- function(base_terms, optional_terms) {
  terms <- unique(c(base_terms, optional_terms))
  terms[!is.na(terms) & terms != ""]
}

build_fe_formula <- function(lhs, rhs_terms, fe_terms = c("region", "cohort", "wave_year_fe")) {
  rhs <- paste(rhs_terms, collapse = " + ")
  fe <- paste(fe_terms, collapse = " + ")
  stats::as.formula(paste0(lhs, " ~ ", rhs, " | ", fe))
}

fit_feols_weighted <- function(formula_obj, data_obj) {
  if (nrow(data_obj) < 100) {
    return(NULL)
  }
  tryCatch(
    fixest::feols(
      fml = formula_obj,
      data = data_obj,
      weights = ~sample_weight,
      vcov = ~region
    ),
    error = function(e) NULL
  )
}

formula_to_string <- function(formula_obj) {
  paste(deparse(formula_obj, width.cutoff = 500), collapse = "")
}

build_module_b_covariate_classification <- function(coverage_df, selected_endogenous) {
  tibble::tribble(
    ~covariate, ~control_class, ~rationale,
    "urban", "pre_determined", "Residence type is treated as a background location characteristic rather than an outcome of the respondent's schooling in the current design.",
    "female", "pre_determined", "Sex is fixed before the respondent's schooling outcome and is used only for descriptive adjustment.",
    "hh_income_proxy", "potentially_endogenous", "Current household income may partly reflect the respondent's own education and later-life selection, so it is not a clean pre-treatment control.",
    "migration_exposure", "potentially_endogenous", "Migration-linked exposure can reflect household responses that co-evolve with schooling and later labor-market opportunities.",
    "multigenerational_hh", "potentially_endogenous", "Current multigenerational co-residence may be shaped by education, marriage, fertility, and post-school household adjustment."
  ) %>%
    dplyr::left_join(coverage_df, by = "covariate") %>%
    dplyr::mutate(
      included_in_extended = covariate %in% selected_endogenous,
      included_in_demographic = covariate %in% MODULE_B_PREDETERMINED_CONTROLS
    )
}

build_module_b_specifications <- function(selected_endogenous) {
  tibble::tibble(
    specification = c("minimal", "demographic", "extended"),
    specification_label = c(
      "Minimal parental association + fixed effects",
      "Add standard demographics",
      "Add potentially endogenous household controls"
    ),
    control_terms = list(
      character(),
      MODULE_B_PREDETERMINED_CONTROLS,
      rhs_with_optional(MODULE_B_PREDETERMINED_CONTROLS, selected_endogenous)
    )
  )
}

build_module_b_formula_rows <- function(model_id, model_family, spec_row, formula_obj, model_obj) {
  tibble::tibble(
    model = model_id,
    model_family = model_family,
    specification = spec_row$specification[[1]],
    specification_label = spec_row$specification_label[[1]],
    control_terms = paste(spec_row$control_terms[[1]], collapse = " + "),
    formula = formula_to_string(formula_obj),
    n_used = if (is.null(model_obj)) NA_integer_ else as.integer(stats::nobs(model_obj))
  )
}

linear_combo_summary <- function(model_obj, weights) {
  if (is.null(model_obj) || length(weights) == 0) {
    return(tibble::tibble(
      estimate = NA_real_,
      std.error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }

  beta <- stats::coef(model_obj)
  vcv <- tryCatch(as.matrix(stats::vcov(model_obj)), error = function(e) NULL)
  if (is.null(vcv) || any(!(names(weights) %in% names(beta)))) {
    return(tibble::tibble(
      estimate = NA_real_,
      std.error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    ))
  }

  w <- rep(0, length(beta))
  names(w) <- names(beta)
  w[names(weights)] <- weights

  estimate <- sum(w * beta)
  variance <- as.numeric(t(w) %*% vcv %*% w)
  std_error <- if (is.na(variance) || variance < 0) NA_real_ else sqrt(variance)
  statistic <- if (is.na(std_error) || std_error == 0) NA_real_ else estimate / std_error
  p_value <- if (is.na(statistic)) NA_real_ else 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  ci_low <- if (is.na(std_error)) NA_real_ else estimate - stats::qnorm(0.975) * std_error
  ci_high <- if (is.na(std_error)) NA_real_ else estimate + stats::qnorm(0.975) * std_error

  tibble::tibble(
    estimate = estimate,
    std.error = std_error,
    ci_low = ci_low,
    ci_high = ci_high,
    statistic = statistic,
    p.value = p_value
  )
}

build_module_b_key_coefficient_comparison <- function(models, formulae) {
  if (length(models) == 0 || nrow(formulae) == 0) {
    return(tibble::tibble())
  }

  key_map <- tibble::tribble(
    ~model_family, ~term, ~term_label,
    "eq2_persistence_trend", "parent_rank", "Parent rank",
    "eq2_persistence_trend", "wave_year_fe::2016:parent_rank", "Parent rank x 2016",
    "eq2_persistence_trend", "wave_year_fe::2022:parent_rank", "Parent rank x 2022-23",
    "eq3_attainment_score", "parent_ed_score", "Parent education score",
    "eq5_persistence_heterogeneity", "parent_ed_score", "Parent education score",
    "eq5_persistence_heterogeneity", "parent_ed_score:wave2022", "Parent education score x 2022-23"
  )

  purrr::imap_dfr(models, function(model_obj, model_id) {
    tid <- broom::tidy(model_obj) %>%
      dplyr::mutate(model = model_id, .before = 1)
    meta <- formulae %>% dplyr::filter(model == model_id)
    tid %>%
      dplyr::left_join(meta, by = "model") %>%
      dplyr::semi_join(key_map, by = c("model_family", "term")) %>%
      dplyr::left_join(key_map, by = c("model_family", "term")) %>%
      dplyr::select(
        model, model_family, specification, specification_label, term, term_label,
        estimate, std.error, statistic, p.value, n_used
      )
  })
}

build_module_b_persistence_wave_profiles <- function(models, formulae) {
  relevant <- formulae %>%
    dplyr::filter(model_family == "eq2_persistence_trend")

  if (nrow(relevant) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(relevant$model, function(model_id) {
    model_obj <- models[[model_id]]
    meta <- relevant %>% dplyr::filter(model == model_id)
    if (is.null(model_obj) || nrow(meta) == 0) {
      return(tibble::tibble())
    }

    wave_weights <- list(
      `2010` = c(parent_rank = 1),
      `2016` = c(parent_rank = 1, "wave_year_fe::2016:parent_rank" = 1),
      `2022` = c(parent_rank = 1, "wave_year_fe::2022:parent_rank" = 1)
    )

    purrr::imap_dfr(wave_weights, function(wts, wave_label) {
      out <- linear_combo_summary(model_obj, wts)
      dplyr::bind_cols(
        meta %>% dplyr::select(model, model_family, specification, specification_label, n_used),
        tibble::tibble(
          wave_year = as.integer(wave_label),
          slope_type = "conditional_association"
        ),
        out
      )
    })
  })
}

build_module_b_wave_difference_tests <- function(models, formulae) {
  relevant <- formulae %>%
    dplyr::filter(model_family == "eq2_persistence_trend")

  if (nrow(relevant) == 0) {
    return(tibble::tibble())
  }

  test_defs <- list(
    `2010_to_2016` = c("wave_year_fe::2016:parent_rank" = 1),
    `2016_to_2022` = c("wave_year_fe::2022:parent_rank" = 1, "wave_year_fe::2016:parent_rank" = -1),
    `2010_to_2022` = c("wave_year_fe::2022:parent_rank" = 1)
  )

  purrr::map_dfr(relevant$model, function(model_id) {
    model_obj <- models[[model_id]]
    meta <- relevant %>% dplyr::filter(model == model_id)
    if (is.null(model_obj) || nrow(meta) == 0) {
      return(tibble::tibble())
    }

    purrr::imap_dfr(test_defs, function(wts, comparison_id) {
      waves <- switch(
        comparison_id,
        `2010_to_2016` = c(2010L, 2016L),
        `2016_to_2022` = c(2016L, 2022L),
        `2010_to_2022` = c(2010L, 2022L)
      )
      out <- linear_combo_summary(model_obj, wts)
      dplyr::bind_cols(
        meta %>% dplyr::select(model, model_family, specification, specification_label, n_used),
        tibble::tibble(
          comparison = comparison_id,
          base_wave = waves[[1]],
          comparison_wave = waves[[2]]
        ),
        out
      )
    })
  })
}

fit_module_b_models <- function(df) {
  mod_data <- prepare_module_b_data(df)
  if (nrow(mod_data) < 100) {
    message("Insufficient observations for Module B. Returning empty output.")
    return(list(
      models = list(),
      selected_covariates = character(),
      coverage = tibble::tibble(),
      covariate_classification = tibble::tibble(),
      specifications = tibble::tibble(),
      formulae = tibble::tibble(),
      key_coefficients = tibble::tibble(),
      persistence_wave_profiles = tibble::tibble(),
      wave_difference_tests = tibble::tibble()
    ))
  }

  candidate_covariates <- c(
    MODULE_B_PREDETERMINED_CONTROLS,
    MODULE_B_POTENTIALLY_ENDOGENOUS_CONTROLS
  )

  coverage <- build_covariate_coverage(mod_data, candidate_covariates)
  selected_endogenous <- MODULE_B_POTENTIALLY_ENDOGENOUS_CONTROLS[
    purrr::map_lgl(MODULE_B_POTENTIALLY_ENDOGENOUS_CONTROLS, ~ is_covariate_informative(mod_data, .x))
  ]
  specifications <- build_module_b_specifications(selected_endogenous)
  covariate_classification <- build_module_b_covariate_classification(coverage, selected_endogenous)

  formula_rows <- list()
  models <- list()

  for (s in seq_len(nrow(specifications))) {
    spec_row <- specifications[s, ]
    spec_id <- spec_row$specification[[1]]
    control_terms <- spec_row$control_terms[[1]]

    rhs_eq2 <- rhs_with_optional(
      base_terms = c("parent_rank", "i(wave_year_fe, parent_rank, ref = '2010')"),
      optional_terms = control_terms
    )
    f_eq2 <- build_fe_formula("own_rank", rhs_eq2)
    model_id <- paste0("eq2_persistence_trend_", spec_id)
    m_eq2 <- fit_feols_weighted(f_eq2, mod_data %>% dplyr::filter(!is.na(own_rank), !is.na(parent_rank)))
    if (!is.null(m_eq2)) {
      models[[model_id]] <- m_eq2
    }
    formula_rows[[length(formula_rows) + 1]] <- build_module_b_formula_rows(
      model_id, "eq2_persistence_trend", spec_row, f_eq2, m_eq2
    )

    rhs_eq3 <- rhs_with_optional(
      base_terms = c("parent_ed_score"),
      optional_terms = control_terms
    )
    f_eq3 <- build_fe_formula("own_ed_score", rhs_eq3)
    model_id <- paste0("eq3_attainment_score_", spec_id)
    m_eq3 <- fit_feols_weighted(f_eq3, mod_data)
    if (!is.null(m_eq3)) {
      models[[model_id]] <- m_eq3
    }
    formula_rows[[length(formula_rows) + 1]] <- build_module_b_formula_rows(
      model_id, "eq3_attainment_score", spec_row, f_eq3, m_eq3
    )

    rhs_eq4 <- rhs_with_optional(
      base_terms = c("i(parent_ed_level, ref = 'upper_secondary')"),
      optional_terms = control_terms
    )
    f_eq4 <- build_fe_formula("upward_any", rhs_eq4)
    model_id <- paste0("eq4_upward_full_lpm_", spec_id)
    m_eq4 <- fit_feols_weighted(f_eq4, mod_data)
    if (!is.null(m_eq4)) {
      models[[model_id]] <- m_eq4
    }
    formula_rows[[length(formula_rows) + 1]] <- build_module_b_formula_rows(
      model_id, "eq4_upward_full_lpm", spec_row, f_eq4, m_eq4
    )

    mod_data_low_parent <- mod_data %>% dplyr::filter(low_parent_sample == 1L)
    model_id <- paste0("eq4_upward_lowparent_lpm_", spec_id)
    m_eq4_low <- fit_feols_weighted(f_eq4, mod_data_low_parent)
    if (!is.null(m_eq4_low)) {
      models[[model_id]] <- m_eq4_low
    }
    formula_rows[[length(formula_rows) + 1]] <- build_module_b_formula_rows(
      model_id, "eq4_upward_lowparent_lpm", spec_row, f_eq4, m_eq4_low
    )

    rhs_eq5 <- rhs_with_optional(
      base_terms = c(
        "parent_ed_score",
        "urban",
        "female",
        "parent_ed_score:urban",
        "parent_ed_score:female",
        "parent_ed_score:wave2022"
      ),
      optional_terms = control_terms
    )
    f_eq5 <- build_fe_formula("persist_same", rhs_eq5)
    model_id <- paste0("eq5_persistence_heterogeneity_", spec_id)
    m_eq5 <- fit_feols_weighted(f_eq5, mod_data)
    if (!is.null(m_eq5)) {
      models[[model_id]] <- m_eq5
    }
    formula_rows[[length(formula_rows) + 1]] <- build_module_b_formula_rows(
      model_id, "eq5_persistence_heterogeneity", spec_row, f_eq5, m_eq5
    )
  }

  formulae <- dplyr::bind_rows(formula_rows)
  key_coefficients <- build_module_b_key_coefficient_comparison(models, formulae)
  persistence_wave_profiles <- build_module_b_persistence_wave_profiles(models, formulae)
  wave_difference_tests <- build_module_b_wave_difference_tests(models, formulae)

  list(
    models = models,
    selected_covariates = selected_endogenous,
    coverage = coverage,
    covariate_classification = covariate_classification,
    specifications = specifications %>%
      dplyr::mutate(control_terms = vapply(control_terms, function(x) paste(x, collapse = " + "), character(1))),
    formulae = formulae,
    key_coefficients = key_coefficients,
    persistence_wave_profiles = persistence_wave_profiles,
    wave_difference_tests = wave_difference_tests
  )
}
