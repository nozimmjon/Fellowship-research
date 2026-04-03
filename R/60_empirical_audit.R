coalesce_years_from_level <- function(years, level) {
  years_num <- clean_numeric(years)
  level_norm <- normalize_ed_level(level)
  fallback_years <- education_level_to_years(level_norm)
  dplyr::if_else(!is.na(years_num), years_num, fallback_years)
}

prepare_empirical_master_flags <- function(lits_harmonized) {
  if (is.null(lits_harmonized) || nrow(lits_harmonized) == 0) {
    return(tibble::tibble())
  }

  out <- lits_harmonized %>%
    dplyr::mutate(
      audit_row_id = dplyr::row_number(),
      wave_year = suppressWarnings(as.integer(wave_year)),
      age = suppressWarnings(as.numeric(age)),
      sample_weight = suppressWarnings(as.numeric(sample_weight)),
      sample_weight = dplyr::if_else(is.na(sample_weight) | sample_weight <= 0, NA_real_, sample_weight),
      own_ed_level = normalize_ed_level(own_ed_level),
      parent_ed_level = normalize_ed_level(parent_ed_level),
      father_ed_level = normalize_ed_level(father_ed_level),
      mother_ed_level = normalize_ed_level(mother_ed_level),
      own_years_schooling = clean_numeric(own_years_schooling),
      parent_years_schooling = clean_numeric(parent_years_schooling),
      father_years_schooling = coalesce_years_from_level(father_years_schooling, father_ed_level),
      mother_years_schooling = coalesce_years_from_level(mother_years_schooling, mother_ed_level),
      cohort = as.character(cohort),
      gender_group = coerce_gender_group(gender),
      urban_group = coerce_urban_group(urban),
      age_eligible = !is.na(age) & age >= ANALYSIS_SAMPLE$age_min & age <= ANALYSIS_SAMPLE$age_max,
      weight_valid = !is.na(sample_weight) & sample_weight > 0,
      own_category_observed = !is.na(own_ed_level),
      own_years_observed = !is.na(own_years_schooling),
      parent_category_observed = !is.na(parent_ed_level),
      parent_years_observed = !is.na(parent_years_schooling),
      father_observed = !is.na(father_ed_level) | !is.na(father_years_schooling),
      mother_observed = !is.na(mother_ed_level) | !is.na(mother_years_schooling),
      any_parent_observed = father_observed | mother_observed,
      both_parents_observed = father_observed & mother_observed,
      father_only_observed = father_observed & !mother_observed,
      mother_only_observed = mother_observed & !father_observed,
      neither_parent_observed = !father_observed & !mother_observed,
      rank_model_included = own_years_observed & parent_years_observed & weight_valid,
      category_model_included = own_category_observed & parent_category_observed & weight_valid
    )

  module_b_data <- prepare_module_b_data(out)
  module_b_ids <- if (nrow(module_b_data) == 0 || !("audit_row_id" %in% names(module_b_data))) integer() else module_b_data$audit_row_id

  out %>%
    dplyr::mutate(
      module_b_included = audit_row_id %in% module_b_ids
    ) %>%
    dplyr::select(
      audit_row_id, wave_year, age, cohort, region, gender_group, urban_group, sample_weight,
      age_eligible, weight_valid,
      own_category_observed, own_years_observed,
      father_observed, mother_observed, father_only_observed, mother_only_observed,
      both_parents_observed, neither_parent_observed,
      parent_category_observed, parent_years_observed,
      rank_model_included, category_model_included, module_b_included
    )
}

prepare_module_c_inclusion_flags <- function(module_c_model) {
  prepared <- module_c_model$prepared_data
  if (is.null(prepared) || nrow(prepared) == 0) {
    return(tibble::tibble())
  }

  split_threshold <- if (!is.null(module_c_model$analysis_data) && nrow(module_c_model$analysis_data) > 0) {
    unique(module_c_model$analysis_data$split_threshold)[1]
  } else {
    stats::median(prepared$parent_years_schooling[prepared$in_mechanism_sample], na.rm = TRUE)
  }

  out <- prepared %>%
    dplyr::mutate(
      module_c_row_id = dplyr::row_number(),
      wave_year = 2022L,
      region = as.character(region),
      urban = suppressWarnings(as.integer(urban)),
      weight_final = suppressWarnings(as.numeric(weight_final)),
      weight_valid = !is.na(weight_final) & weight_final > 0,
      child_module_eligibility_observed = !is.na(child_enrolled_pre_covid),
      child_enrolled_pre_covid = child_enrolled_pre_covid == 1L,
      parent_years_schooling = suppressWarnings(as.numeric(parent_years_schooling)),
      parent_schooling_observed = !is.na(parent_years_schooling),
      parent_low_edu = dplyr::case_when(
        is.na(parent_years_schooling) ~ NA_integer_,
        parent_years_schooling <= split_threshold ~ 1L,
        TRUE ~ 0L
      ),
      mechanism_sample_eligible = child_module_eligibility_observed & child_enrolled_pre_covid,
      mechanism_parent_non_missing = mechanism_sample_eligible & parent_schooling_observed
    )

  model_specs <- c(
    m1_switched_online = "switched_online",
    m2_school_closed_no_online = "school_closed_no_online",
    m3_education_stopped_covid = "education_stopped_covid",
    m4_any_remote_challenge = "any_remote_challenge"
  )

  for (model_name in names(model_specs)) {
    outcome_var <- model_specs[[model_name]]
    out[[paste0(model_name, "_included")]] <-
      out$mechanism_sample_eligible &
      !is.na(out[[outcome_var]]) &
      !is.na(out$parent_low_edu) &
      !is.na(out$urban) &
      !is.na(out$region) &
      out$weight_valid
  }

  out %>%
    dplyr::select(
      module_c_row_id, wave_year, region, urban, weight_valid,
      child_module_eligibility_observed, child_enrolled_pre_covid,
      mechanism_sample_eligible, parent_schooling_observed, mechanism_parent_non_missing,
      parent_years_schooling, parent_low_edu,
      dplyr::ends_with("_included")
    )
}

build_empirical_sample_flow <- function(master_flags, module_c_flags) {
  adult_flow <- if (nrow(master_flags) == 0) {
    tibble::tibble()
  } else {
    master_flags %>%
      dplyr::group_by(wave_year) %>%
      dplyr::summarise(
        adult_age_eligible_sample = sum(age_eligible),
        own_category_observed = sum(own_category_observed),
        own_years_observed = sum(own_years_observed),
        father_observed = sum(father_observed),
        mother_observed = sum(mother_observed),
        any_parent_observed = sum(parent_category_observed | parent_years_observed),
        both_parents_observed = sum(both_parents_observed),
        rank_model_included = sum(rank_model_included),
        category_model_included = sum(category_model_included),
        module_b_included = sum(module_b_included),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = -wave_year,
        names_to = "step",
        values_to = "n"
      ) %>%
      dplyr::mutate(sample_family = "adult_lits")
  }

  module_c_flow <- if (nrow(module_c_flags) == 0) {
    tibble::tibble()
  } else {
    module_c_flags %>%
      dplyr::summarise(
        `Uzbekistan LiTS IV respondents` = dplyr::n(),
        `Respondents with child module eligibility info` = sum(child_module_eligibility_observed, na.rm = TRUE),
        `Respondents with child enrolled pre-COVID` = sum(mechanism_sample_eligible, na.rm = TRUE),
        `Mechanism sample with non-missing parental schooling` = sum(mechanism_parent_non_missing, na.rm = TRUE),
        `Main online-switch model usable sample` = sum(m1_switched_online_included, na.rm = TRUE),
        `Main closure-without-online model usable sample` = sum(m2_school_closed_no_online_included, na.rm = TRUE),
        `Main stoppage model usable sample` = sum(m3_education_stopped_covid_included, na.rm = TRUE),
        `Main remote-challenge model usable sample` = sum(m4_any_remote_challenge_included, na.rm = TRUE)
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "step",
        values_to = "n"
      ) %>%
      dplyr::mutate(
        sample_family = "module_c_child",
        wave_year = 2022L,
        .before = 1
      )
  }

  dplyr::bind_rows(adult_flow, module_c_flow) %>%
    dplyr::select(sample_family, wave_year, step, n)
}

build_inclusion_composition <- function(master_flags) {
  if (is.null(master_flags) || nrow(master_flags) == 0) {
    return(tibble::tibble())
  }

  inclusion_vars <- c("rank_model_included", "category_model_included", "module_b_included")
  subgroup_vars <- c("gender_group", "cohort", "urban_group", "region")

  purrr::map_dfr(inclusion_vars, function(inclusion_var) {
    purrr::map_dfr(subgroup_vars, function(subgroup_var) {
      master_flags %>%
        dplyr::filter(!is.na(.data[[subgroup_var]]), .data[[subgroup_var]] != "") %>%
        dplyr::group_by(wave_year, subgroup_value = .data[[subgroup_var]], included = .data[[inclusion_var]]) %>%
        dplyr::summarise(
          n = dplyr::n(),
          weight_sum = sum(sample_weight, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::group_by(wave_year, included) %>%
        dplyr::mutate(
          share_within_status = n / sum(n),
          weight_total = sum(weight_sum, na.rm = TRUE),
          weighted_share_within_status = ifelse(weight_total > 0, weight_sum / weight_total, NA_real_)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          model_sample = inclusion_var,
          subgroup_type = subgroup_var,
          .before = 1
        ) %>%
        dplyr::select(-weight_total)
    })
  })
}

build_parent_availability <- function(master_flags) {
  if (is.null(master_flags) || nrow(master_flags) == 0) {
    return(tibble::tibble())
  }

  master_flags %>%
    dplyr::group_by(wave_year) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      father_only_n = sum(father_only_observed),
      mother_only_n = sum(mother_only_observed),
      both_observed_n = sum(both_parents_observed),
      neither_observed_n = sum(neither_parent_observed),
      any_parent_n = sum(parent_category_observed | parent_years_observed),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      father_only_share = father_only_n / n_total,
      mother_only_share = mother_only_n / n_total,
      both_observed_share = both_observed_n / n_total,
      neither_observed_share = neither_observed_n / n_total,
      any_parent_share = any_parent_n / n_total
    )
}

build_parent_variant_data <- function(lits_harmonized, variant) {
  out <- lits_harmonized %>%
    dplyr::mutate(
      father_ed_level = normalize_ed_level(father_ed_level),
      mother_ed_level = normalize_ed_level(mother_ed_level),
      father_years_schooling = coalesce_years_from_level(father_years_schooling, father_ed_level),
      mother_years_schooling = coalesce_years_from_level(mother_years_schooling, mother_ed_level)
    )

  if (variant == "baseline_max") {
    out <- out %>%
      dplyr::mutate(
        parent_ed_level = parent_max_level(father_ed_level, mother_ed_level),
        parent_years_schooling = education_level_to_years(parent_ed_level)
      )
  } else if (variant == "mean_parent_years") {
    out <- out %>%
      dplyr::mutate(
        parent_years_schooling = rowMeans(cbind(father_years_schooling, mother_years_schooling), na.rm = TRUE),
        parent_years_schooling = dplyr::if_else(is.nan(parent_years_schooling), NA_real_, parent_years_schooling),
        parent_ed_level = years_to_education_level(parent_years_schooling)
      )
  } else if (variant == "father_only") {
    out <- out %>%
      dplyr::mutate(
        parent_ed_level = father_ed_level,
        parent_years_schooling = father_years_schooling
      )
  } else if (variant == "mother_only") {
    out <- out %>%
      dplyr::mutate(
        parent_ed_level = mother_ed_level,
        parent_years_schooling = mother_years_schooling
      )
  } else if (variant == "both_parents_observed_only") {
    out <- out %>%
      dplyr::filter(
        (!is.na(father_ed_level) | !is.na(father_years_schooling)) &
          (!is.na(mother_ed_level) | !is.na(mother_years_schooling))
      ) %>%
      dplyr::mutate(
        parent_ed_level = parent_max_level(father_ed_level, mother_ed_level),
        parent_years_schooling = education_level_to_years(parent_ed_level)
      )
  } else {
    stop("Unknown parent-measure variant: ", variant)
  }

  out
}

build_parent_measure_robustness <- function(lits_harmonized) {
  if (is.null(lits_harmonized) || nrow(lits_harmonized) == 0) {
    return(tibble::tibble())
  }

  variants <- c("baseline_max", "mean_parent_years", "father_only", "mother_only", "both_parents_observed_only")

  purrr::map_dfr(variants, function(variant) {
    variant_data <- build_parent_variant_data(lits_harmonized, variant)
    metrics <- estimate_mobility_metrics(variant_data)$core_metrics
    metrics %>%
      dplyr::filter(
        subgroup_type == "overall",
        subgroup_value == "all",
        metric %in% c("rank_rank_slope", "upward_mobility_rate", "downward_mobility_rate", "persistence_probability")
      ) %>%
      dplyr::mutate(
        parent_measure_variant = variant,
        .before = 1
      )
  })
}

build_empirical_model_inventory <- function(module_b_models, module_c_model) {
  module_b_inventory <- if (is.null(module_b_models$formulae) || nrow(module_b_models$formulae) == 0) {
    tibble::tibble()
  } else {
    module_b_models$formulae %>%
      dplyr::mutate(
        module = "module_b",
        outcome = dplyr::case_when(
          model == "eq2_persistence_trend" ~ "own rank",
          model == "eq3_attainment_score" ~ "own education score",
          model %in% c("eq4_upward_full_lpm", "eq4_upward_lowparent_lpm") ~ "upward mobility indicator",
          model == "eq5_persistence_heterogeneity" ~ "same-category persistence indicator",
          TRUE ~ model
        ),
        estimator = "fixest::feols",
        family = "gaussian",
        link = "identity",
        weighted = TRUE,
        fixed_effects = "region + cohort + wave_year_fe",
        parameter_scale = dplyr::case_when(
          model == "eq2_persistence_trend" ~ "rank outcome / rank coefficient",
          model == "eq3_attainment_score" ~ "education-score points",
          TRUE ~ "linear-probability coefficient"
        ),
        interaction_structure = dplyr::case_when(
          model == "eq2_persistence_trend" ~ "parent_rank x wave",
          model == "eq5_persistence_heterogeneity" ~ "parent_ed_score x urban + female + wave2022",
          TRUE ~ "no parent-by-wave interaction"
        )
      ) %>%
      dplyr::select(module, model, outcome, estimator, family, link, weighted, fixed_effects, parameter_scale, interaction_structure, formula)
  }

  module_c_inventory <- if (is.null(module_c_model$formulae) || nrow(module_c_model$formulae) == 0) {
    tibble::tibble()
  } else {
    module_c_model$formulae %>%
      dplyr::mutate(
        module = "module_c",
        estimator = "fixest::feglm",
        family = "binomial",
        link = "logit",
        weighted = TRUE,
        fixed_effects = "region",
        parameter_scale = "log-odds coefficient",
        interaction_structure = "parent_low_edu x urban"
      ) %>%
      dplyr::select(module, model, outcome, estimator, family, link, weighted, fixed_effects, parameter_scale, interaction_structure, formula, n_used)
  }

  dplyr::bind_rows(module_b_inventory, module_c_inventory)
}

build_empirical_claim_audit <- function(master_flags, module_a_metrics, module_b_models, module_c_model) {
  rows <- list()
  row_id <- 0L

  if (nrow(master_flags) > 0) {
    completeness <- master_flags %>%
      dplyr::group_by(wave_year) %>%
      dplyr::summarise(
        own_category_n = sum(own_category_observed),
        own_years_n = sum(own_years_observed),
        parent_category_n = sum(parent_category_observed),
        parent_years_n = sum(parent_years_observed),
        rank_model_n = sum(rank_model_included),
        category_model_n = sum(category_model_included),
        .groups = "drop"
      )

    for (i in seq_len(nrow(completeness))) {
      wave <- completeness$wave_year[[i]]
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_a",
        check_id = paste0("completeness_alignment_", wave),
        status = if (completeness$own_years_n[[i]] > completeness$own_category_n[[i]] || completeness$parent_years_n[[i]] > completeness$parent_category_n[[i]]) "needs_review" else "ok",
        detail = paste0(
          "Wave ", wave,
          ": own category N=", completeness$own_category_n[[i]],
          ", own years N=", completeness$own_years_n[[i]],
          "; parent category N=", completeness$parent_category_n[[i]],
          ", parent years N=", completeness$parent_years_n[[i]], "."
        )
      )

      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_a",
        check_id = paste0("rank_vs_category_denominator_", wave),
        status = if (completeness$category_model_n[[i]] < completeness$rank_model_n[[i]]) "caution" else "ok",
        detail = paste0(
          "Wave ", wave,
          ": rank-model N=", completeness$rank_model_n[[i]],
          " versus category-model N=", completeness$category_model_n[[i]], "."
        )
      )
    }
  }

  if (!is.null(module_b_models$formulae) && nrow(module_b_models$formulae) > 0) {
    formulae <- module_b_models$formulae
    for (model_name in formulae$model) {
      formula_text <- formulae$formula[formulae$model == model_name][1]
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_b",
        check_id = paste0("formula_inventory_", model_name),
        status = "ok",
        detail = paste0(model_name, ": ", formula_text)
      )
    }
  }

  if (!is.null(module_b_models$models$eq2_persistence_trend)) {
    eq2_coef <- broom::tidy(module_b_models$models$eq2_persistence_trend) %>%
      dplyr::filter(term == "parent_rank")
    if (nrow(eq2_coef) > 0) {
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_b",
        check_id = "eq2_parent_rank_precision",
        status = if (eq2_coef$p.value[[1]] < 0.05) "ok" else "caution",
        detail = paste0(
          "Eq. 2 parent_rank estimate=", sprintf("%.3f", eq2_coef$estimate[[1]]),
          ", p-value=", sprintf("%.3f", eq2_coef$p.value[[1]]), "."
        )
      )
    }
  }

  if (!is.null(module_c_model$formulae) && nrow(module_c_model$formulae) > 0) {
    row_id <- row_id + 1L
    rows[[row_id]] <- tibble::tibble(
      module = "module_c",
      check_id = "main_estimator_scale",
      status = "caution",
      detail = "Main Module C models are weighted region-fixed-effects logits, so coefficients are on the log-odds scale."
    )
  }

  if (!is.null(module_c_model$robustness_coefficients) && nrow(module_c_model$robustness_coefficients) > 0) {
    stoppage_rows <- module_c_model$robustness_coefficients %>%
      dplyr::filter(model == "m3_education_stopped_covid", term == "parent_low_edu")
    if (nrow(stoppage_rows) > 0) {
      n_sig <- sum(!is.na(stoppage_rows$p.value) & stoppage_rows$p.value < 0.05)
      extreme_sep <- any(abs(stoppage_rows$estimate) > 10, na.rm = TRUE)
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_c",
        check_id = "stoppage_fragility",
        status = if (n_sig <= 1 || extreme_sep) "caution" else "ok",
        detail = paste0(
          "Stoppage robustness: ", n_sig, " of ", nrow(stoppage_rows),
          " parent_low_edu scenarios significant at 5%; extreme coefficient present=",
          ifelse(extreme_sep, "yes", "no"), "."
        )
      )
    }
  }

  if (!is.null(module_c_model$robustness_scenarios) && nrow(module_c_model$robustness_scenarios) > 0) {
    max_parent_row <- module_c_model$robustness_scenarios %>%
      dplyr::filter(scenario_id == "weighted_median_maxparent")

    if (nrow(max_parent_row) > 0) {
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_c",
        check_id = "parent_proxy_exception",
        status = if (max_parent_row$n_high_group[[1]] == 0) "caution" else "ok",
        detail = paste0(
          "Module C uses mean parent years for the low/high split; under the max-parent median split, threshold=",
          sprintf("%.1f", max_parent_row$split_threshold[[1]]),
          ", low-group N=", max_parent_row$n_low_group[[1]],
          ", high-group N=", max_parent_row$n_high_group[[1]], "."
        )
      )
    }
  }

  if (!is.null(module_c_model$sample_overview) && nrow(module_c_model$sample_overview) > 0 && !is.null(module_c_model$formulae) && nrow(module_c_model$formulae) > 0) {
    mech_parent_n <- module_c_model$sample_overview$n[module_c_model$sample_overview$step == "Mechanism sample with non-missing parental schooling"][1]
    main_n <- module_c_model$formulae$n_used[module_c_model$formulae$model == "m3_education_stopped_covid"][1]
    if (!is.na(mech_parent_n) && !is.na(main_n)) {
      row_id <- row_id + 1L
      rows[[row_id]] <- tibble::tibble(
        module = "module_c",
        check_id = "sample_flow_vs_model_n",
        status = if (main_n <= mech_parent_n) "ok" else "needs_review",
        detail = paste0(
          "Module C parental-schooling sample N=", mech_parent_n,
          "; main stoppage-model usable N=", main_n, "."
        )
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(rows)
}

build_empirical_audit_memo_lines <- function(audit_bundle) {
  master_flags <- audit_bundle$master_flags
  parent_availability <- audit_bundle$parent_availability
  claim_audit <- audit_bundle$claim_audit

  fmt_int <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ",", scientific = FALSE, trim = TRUE)

  lines <- c(
    "# Empirical Audit Memo",
    "",
    paste0("Generated on ", as.character(Sys.Date()), "."),
    "",
    "## Scope",
    "",
    "- This memo audits empirical sample accounting, denominator differences, model definitions, and parent-measure robustness before any manuscript rewrite.",
    "- The adult LiTS audit uses the harmonized age-eligible sample (ages 25-64). Module C is audited separately because its child-module sample is a different 2022 respondent subset.",
    ""
  )

  if (nrow(master_flags) > 0) {
    adult_counts <- master_flags %>%
      dplyr::group_by(wave_year) %>%
      dplyr::summarise(n_total = dplyr::n(), .groups = "drop")
    lines <- c(lines, "## Adult LiTS Sample", "")
    for (i in seq_len(nrow(adult_counts))) {
      lines <- c(lines, paste0("- Wave ", adult_counts$wave_year[[i]], ": ", fmt_int(adult_counts$n_total[[i]]), " adult respondents in the harmonized analytical sample."))
    }
    lines <- c(lines, "")
  }

  if (nrow(parent_availability) > 0) {
    lines <- c(lines, "## Parent Availability", "")
    for (i in seq_len(nrow(parent_availability))) {
      lines <- c(
        lines,
        paste0(
          "- Wave ", parent_availability$wave_year[[i]],
          ": both parents observed=", fmt_int(parent_availability$both_observed_n[[i]]),
          ", father only=", fmt_int(parent_availability$father_only_n[[i]]),
          ", mother only=", fmt_int(parent_availability$mother_only_n[[i]]),
          ", neither=", fmt_int(parent_availability$neither_observed_n[[i]]), "."
        )
      )
    }
    lines <- c(lines, "")
  }

  if (nrow(claim_audit) > 0) {
    lines <- c(lines, "## Main Checks", "")
    for (i in seq_len(nrow(claim_audit))) {
      lines <- c(lines, paste0("- [", claim_audit$status[[i]], "] ", claim_audit$detail[[i]]))
    }
  }

  lines
}

build_empirical_audit <- function(lits_harmonized, module_a_metrics, module_b_models, module_c_model) {
  master_flags <- prepare_empirical_master_flags(lits_harmonized)
  module_c_flags <- prepare_module_c_inclusion_flags(module_c_model)
  sample_flow <- build_empirical_sample_flow(master_flags, module_c_flags)
  inclusion_composition <- build_inclusion_composition(master_flags)
  parent_availability <- build_parent_availability(master_flags)
  parent_measure_robustness <- build_parent_measure_robustness(lits_harmonized)
  model_inventory <- build_empirical_model_inventory(module_b_models, module_c_model)
  claim_audit <- build_empirical_claim_audit(master_flags, module_a_metrics, module_b_models, module_c_model)

  audit_bundle <- list(
    master_flags = master_flags,
    module_c_flags = module_c_flags,
    sample_flow = sample_flow,
    inclusion_composition = inclusion_composition,
    parent_availability = parent_availability,
    parent_measure_robustness = parent_measure_robustness,
    model_inventory = model_inventory,
    claim_audit = claim_audit
  )

  audit_bundle$memo_lines <- build_empirical_audit_memo_lines(audit_bundle)
  audit_bundle
}
