safe_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(df) || ncol(df) == 0) {
    df <- data.frame(note = character())
  }
  readr::write_csv(df, path)
  path
}

safe_png_plot <- function(path, width = 1200, height = 800, res = 120, expr) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(filename = path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
  path
}

plot_national_rank_rank <- function(national_trends, path) {
  dat <- national_trends %>%
    dplyr::filter(metric == "rank_rank_slope") %>%
    dplyr::arrange(wave_year)

  safe_png_plot(path, expr = {
    if (nrow(dat) == 0) {
      graphics::plot.new()
      graphics::title("Rank-Rank Slope by Wave")
      graphics::text(0.5, 0.5, "No data available")
    } else {
      y_min <- if ("ci_low" %in% names(dat)) min(c(0, dat$ci_low), na.rm = TRUE) else min(c(0, dat$estimate), na.rm = TRUE)
      y_max <- if ("ci_high" %in% names(dat)) max(dat$ci_high, na.rm = TRUE) else max(dat$estimate, na.rm = TRUE)
      graphics::plot(
        dat$wave_year, dat$estimate,
        type = "b", pch = 19, lwd = 2, col = "#1f4e79",
        xlab = "Wave Year", ylab = "Rank-Rank Slope",
        main = "Rank-Rank Slope by Wave",
        ylim = c(y_min, y_max)
      )
      graphics::grid()
      if (all(c("ci_low", "ci_high") %in% names(dat))) {
        graphics::arrows(
          x0 = dat$wave_year,
          y0 = dat$ci_low,
          x1 = dat$wave_year,
          y1 = dat$ci_high,
          angle = 90,
          code = 3,
          length = 0.05,
          col = "#1f4e79"
        )
      }
    }
  })
}

plot_national_directional_rates <- function(national_trends, path) {
  dat <- national_trends %>%
    dplyr::filter(metric %in% c("upward_mobility_rate", "downward_mobility_rate", "persistence_probability")) %>%
    dplyr::arrange(metric, wave_year)

  metric_labels <- c(
    upward_mobility_rate = "Upward",
    downward_mobility_rate = "Downward",
    persistence_probability = "Persistence"
  )
  colors <- c(
    upward_mobility_rate = "#2a9d8f",
    downward_mobility_rate = "#e76f51",
    persistence_probability = "#264653"
  )

  safe_png_plot(path, expr = {
    if (nrow(dat) == 0) {
      graphics::plot.new()
      graphics::title("Directional Mobility Rates by Wave")
      graphics::text(0.5, 0.5, "No data available")
    } else {
      yr <- sort(unique(dat$wave_year))
      graphics::plot(
        yr, rep(NA_real_, length(yr)),
        type = "n",
        xlab = "Wave Year", ylab = "Rate",
        ylim = c(0, 1),
        main = "Directional Mobility Rates by Wave"
      )
      graphics::grid()
      for (m in names(metric_labels)) {
        dm <- dat %>% dplyr::filter(metric == m) %>% dplyr::arrange(wave_year)
        graphics::lines(dm$wave_year, dm$estimate, type = "b", pch = 19, lwd = 2, col = colors[[m]])
        if (all(c("ci_low", "ci_high") %in% names(dm))) {
          graphics::arrows(
            x0 = dm$wave_year,
            y0 = dm$ci_low,
            x1 = dm$wave_year,
            y1 = dm$ci_high,
            angle = 90,
            code = 3,
            length = 0.03,
            col = colors[[m]]
          )
        }
      }
      graphics::legend(
        "topright",
        legend = unname(metric_labels),
        col = unname(colors),
        pch = 19,
        lty = 1,
        bty = "n"
      )
    }
  })
}

plot_subgroup_upward <- function(subgroup_trends, subgroup_name, path) {
  dat <- subgroup_trends %>%
    dplyr::filter(subgroup_type == subgroup_name, metric == "upward_mobility_rate") %>%
    dplyr::arrange(subgroup_value, wave_year)

  safe_png_plot(path, expr = {
    if (nrow(dat) == 0) {
      graphics::plot.new()
      graphics::title(paste("Upward Mobility by", subgroup_name))
      graphics::text(0.5, 0.5, "No data available")
    } else {
      yr <- sort(unique(dat$wave_year))
      graphics::plot(
        yr, rep(NA_real_, length(yr)),
        type = "n",
        xlab = "Wave Year", ylab = "Upward Mobility Rate",
        ylim = c(0, max(dat$estimate, na.rm = TRUE) * 1.1),
        main = paste("Upward Mobility by", subgroup_name)
      )
      graphics::grid()
      groups <- unique(dat$subgroup_value)
      cols <- grDevices::hcl.colors(length(groups), "Dark 3")
      for (i in seq_along(groups)) {
        g <- groups[[i]]
        dg <- dat %>% dplyr::filter(subgroup_value == g)
        graphics::lines(dg$wave_year, dg$estimate, type = "b", pch = 19, lwd = 2, col = cols[[i]])
        if (all(c("ci_low", "ci_high") %in% names(dg))) {
          graphics::arrows(
            x0 = dg$wave_year,
            y0 = dg$ci_low,
            x1 = dg$wave_year,
            y1 = dg$ci_high,
            angle = 90,
            code = 3,
            length = 0.03,
            col = cols[[i]]
          )
        }
      }
      graphics::legend("topright", legend = groups, col = cols, pch = 19, lty = 1, bty = "n")
    }
  })
}

save_module_a_outputs <- function(module_a_metrics) {
  summary_path <- file.path(PROJ_PATHS$tables, "module_a_summary_metrics.csv")
  subgroup_path <- file.path(PROJ_PATHS$tables, "module_a_subgroup_metrics.csv")
  upward_path <- file.path(PROJ_PATHS$tables, "module_a_upward_mobility.csv")
  transition_path <- file.path(PROJ_PATHS$tables, "module_a_transition_matrix.csv")
  persistence_parent_path <- file.path(PROJ_PATHS$tables, "module_a_persistence_by_parent.csv")
  measure_spec_path <- file.path(PROJ_PATHS$metadata, "mobility_measure_set.csv")
  variable_lock_path <- file.path(PROJ_PATHS$metadata, "mobility_variable_lock.csv")

  core_metrics <- module_a_metrics$core_metrics
  subgroup_metrics <- module_a_metrics$subgroup_metrics
  transition_matrix <- module_a_metrics$transition_matrix
  persistence_by_parent <- module_a_metrics$persistence_by_parent
  measure_spec <- module_a_metrics$measure_spec
  variable_lock <- module_a_metrics$variable_lock

  if (is.null(core_metrics)) core_metrics <- tibble::tibble()
  if (is.null(subgroup_metrics)) subgroup_metrics <- tibble::tibble()
  if (is.null(transition_matrix)) transition_matrix <- tibble::tibble()
  if (is.null(persistence_by_parent)) persistence_by_parent <- tibble::tibble()
  if (is.null(measure_spec)) measure_spec <- tibble::tibble()
  if (is.null(variable_lock)) variable_lock <- tibble::tibble()

  upward_legacy <- dplyr::bind_rows(core_metrics, subgroup_metrics) %>%
    dplyr::filter(metric == "upward_mobility_rate")

  safe_write_csv(core_metrics, summary_path)
  safe_write_csv(subgroup_metrics, subgroup_path)
  safe_write_csv(upward_legacy, upward_path)
  safe_write_csv(transition_matrix, transition_path)
  safe_write_csv(persistence_by_parent, persistence_parent_path)
  safe_write_csv(measure_spec, measure_spec_path)
  safe_write_csv(variable_lock, variable_lock_path)

  c(
    summary_path,
    subgroup_path,
    upward_path,
    transition_path,
    persistence_parent_path,
    measure_spec_path,
    variable_lock_path
  )
}

save_module_a_tier_a_outputs <- function(module_a_tier_a) {
  national_path <- file.path(PROJ_PATHS$tables, "tier_a_national_trends.csv")
  subgroup_path <- file.path(PROJ_PATHS$tables, "tier_a_subgroup_trends.csv")
  region_path <- file.path(PROJ_PATHS$tables, "tier_a_region_trends.csv")
  transition_path <- file.path(PROJ_PATHS$tables, "tier_a_transition_summary.csv")
  sample_path <- file.path(PROJ_PATHS$tables, "tier_a_sample_by_wave.csv")
  completeness_path <- file.path(PROJ_PATHS$tables, "tier_a_data_completeness.csv")

  rank_rank_fig <- file.path(PROJ_PATHS$figures, "tier_a_rank_rank_by_wave.png")
  directional_fig <- file.path(PROJ_PATHS$figures, "tier_a_directional_rates_by_wave.png")
  urban_fig <- file.path(PROJ_PATHS$figures, "tier_a_upward_by_urban_rural.png")
  gender_fig <- file.path(PROJ_PATHS$figures, "tier_a_upward_by_gender.png")

  national_trends <- module_a_tier_a$national_trends
  subgroup_trends <- module_a_tier_a$subgroup_trends
  region_trends <- module_a_tier_a$region_trends
  transition_summary <- module_a_tier_a$transition_summary
  sample_by_wave <- module_a_tier_a$sample_by_wave
  data_completeness <- module_a_tier_a$data_completeness

  if (is.null(national_trends)) national_trends <- tibble::tibble()
  if (is.null(subgroup_trends)) subgroup_trends <- tibble::tibble()
  if (is.null(region_trends)) region_trends <- tibble::tibble()
  if (is.null(transition_summary)) transition_summary <- tibble::tibble()
  if (is.null(sample_by_wave)) sample_by_wave <- tibble::tibble()
  if (is.null(data_completeness)) data_completeness <- tibble::tibble()

  safe_write_csv(national_trends, national_path)
  safe_write_csv(subgroup_trends, subgroup_path)
  safe_write_csv(region_trends, region_path)
  safe_write_csv(transition_summary, transition_path)
  safe_write_csv(sample_by_wave, sample_path)
  safe_write_csv(data_completeness, completeness_path)

  plot_national_rank_rank(national_trends, rank_rank_fig)
  plot_national_directional_rates(national_trends, directional_fig)
  plot_subgroup_upward(subgroup_trends, "urban_rural", urban_fig)
  plot_subgroup_upward(subgroup_trends, "gender", gender_fig)

  c(
    national_path,
    subgroup_path,
    region_path,
    transition_path,
    sample_path,
    completeness_path,
    rank_rank_fig,
    directional_fig,
    urban_fig,
    gender_fig
  )
}

save_module_b_outputs <- function(model_list) {
  coef_path <- file.path(PROJ_PATHS$tables, "module_b_model_coefficients.csv")
  coverage_path <- file.path(PROJ_PATHS$tables, "module_b_covariate_coverage.csv")
  selected_covariates_path <- file.path(PROJ_PATHS$tables, "module_b_selected_covariates.csv")
  classification_path <- file.path(PROJ_PATHS$tables, "module_b_covariate_classification.csv")
  specifications_path <- file.path(PROJ_PATHS$tables, "module_b_specifications.csv")
  formulae_path <- file.path(PROJ_PATHS$tables, "module_b_formulae.csv")
  key_coef_path <- file.path(PROJ_PATHS$tables, "module_b_key_coefficient_comparison.csv")
  wave_profile_path <- file.path(PROJ_PATHS$tables, "module_b_persistence_wave_profiles.csv")
  wave_test_path <- file.path(PROJ_PATHS$tables, "module_b_wave_difference_tests.csv")
  model_obj_path <- file.path(PROJ_PATHS$models, "module_b_models.rds")

  if (length(model_list) == 0 || is.null(model_list$models) || length(model_list$models) == 0) {
    empty_coef <- tibble::tibble(
      model = character(),
      term = character(),
      estimate = double(),
      std.error = double(),
      statistic = double(),
      p.value = double()
    )
    empty_cov <- tibble::tibble(
      covariate = character(),
      n_total = integer(),
      n_non_missing = integer(),
      share_non_missing = double(),
      n_unique_non_missing = integer()
    )
    empty_selected <- tibble::tibble(covariate = character())
    empty_formulae <- tibble::tibble(model = character(), formula = character())
    safe_write_csv(empty_coef, coef_path)
    safe_write_csv(empty_cov, coverage_path)
    safe_write_csv(empty_selected, selected_covariates_path)
    safe_write_csv(tibble::tibble(), classification_path)
    safe_write_csv(tibble::tibble(), specifications_path)
    safe_write_csv(empty_formulae, formulae_path)
    safe_write_csv(tibble::tibble(), key_coef_path)
    safe_write_csv(tibble::tibble(), wave_profile_path)
    safe_write_csv(tibble::tibble(), wave_test_path)
    saveRDS(model_list, model_obj_path)
    return(c(
      coef_path, coverage_path, selected_covariates_path, classification_path,
      specifications_path, formulae_path, key_coef_path, wave_profile_path,
      wave_test_path, model_obj_path
    ))
  }

  coef_df <- purrr::imap_dfr(model_list$models, function(model, model_name) {
    broom::tidy(model) %>%
      dplyr::mutate(model = model_name, .before = 1)
  })

  coverage_df <- model_list$coverage
  selected_df <- tibble::tibble(covariate = model_list$selected_covariates)
  classification_df <- model_list$covariate_classification
  specifications_df <- model_list$specifications
  formulae_df <- model_list$formulae
  key_coef_df <- model_list$key_coefficients
  wave_profile_df <- model_list$persistence_wave_profiles
  wave_test_df <- model_list$wave_difference_tests

  if (is.null(coverage_df)) coverage_df <- tibble::tibble()
  if (is.null(selected_df)) selected_df <- tibble::tibble(covariate = character())
  if (is.null(classification_df)) classification_df <- tibble::tibble()
  if (is.null(specifications_df)) specifications_df <- tibble::tibble()
  if (is.null(formulae_df)) formulae_df <- tibble::tibble()
  if (is.null(key_coef_df)) key_coef_df <- tibble::tibble()
  if (is.null(wave_profile_df)) wave_profile_df <- tibble::tibble()
  if (is.null(wave_test_df)) wave_test_df <- tibble::tibble()

  if (nrow(coef_df) > 0 && nrow(formulae_df) > 0) {
    coef_df <- coef_df %>%
      dplyr::left_join(
        formulae_df %>%
          dplyr::select(model, model_family, specification, specification_label, n_used),
        by = "model"
      )
  }

  safe_write_csv(coef_df, coef_path)
  safe_write_csv(coverage_df, coverage_path)
  safe_write_csv(selected_df, selected_covariates_path)
  safe_write_csv(classification_df, classification_path)
  safe_write_csv(specifications_df, specifications_path)
  safe_write_csv(formulae_df, formulae_path)
  safe_write_csv(key_coef_df, key_coef_path)
  safe_write_csv(wave_profile_df, wave_profile_path)
  safe_write_csv(wave_test_df, wave_test_path)
  saveRDS(model_list, model_obj_path)

  c(
    coef_path, coverage_path, selected_covariates_path, classification_path,
    specifications_path, formulae_path, key_coef_path, wave_profile_path,
    wave_test_path, model_obj_path
  )
}

save_module_c_outputs <- function(model) {
  summary_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_summary.csv")
  coverage_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_coverage.csv")
  sample_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_sample.csv")
  formulae_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_formulae.csv")
  coef_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_coefficients.csv")
  robust_scenarios_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_robustness_scenarios.csv")
  robust_coef_path <- file.path(PROJ_PATHS$tables, "module_c_mechanism_robustness_coefficients.csv")
  model_obj_path <- file.path(PROJ_PATHS$models, "module_c_mechanism_models.rds")

  if (is.null(model) || length(model) == 0) {
    safe_write_csv(tibble::tibble(), summary_path)
    safe_write_csv(tibble::tibble(), coverage_path)
    safe_write_csv(tibble::tibble(), sample_path)
    safe_write_csv(tibble::tibble(), formulae_path)
    safe_write_csv(tibble::tibble(), coef_path)
    safe_write_csv(tibble::tibble(), robust_scenarios_path)
    safe_write_csv(tibble::tibble(), robust_coef_path)
    saveRDS(model, model_obj_path)
    return(c(summary_path, coverage_path, sample_path, formulae_path, coef_path, robust_scenarios_path, robust_coef_path, model_obj_path))
  }

  summary_df <- model$summary
  coverage_df <- model$coverage
  sample_df <- model$sample_overview
  formulae_df <- model$formulae
  robust_scenarios_df <- model$robustness_scenarios
  robust_coef_df <- model$robustness_coefficients

  if (is.null(summary_df)) summary_df <- tibble::tibble()
  if (is.null(coverage_df)) coverage_df <- tibble::tibble()
  if (is.null(sample_df)) sample_df <- tibble::tibble()
  if (is.null(formulae_df)) formulae_df <- tibble::tibble()
  if (is.null(robust_scenarios_df)) robust_scenarios_df <- tibble::tibble()
  if (is.null(robust_coef_df)) robust_coef_df <- tibble::tibble()

  if (!is.null(model$models) && length(model$models) > 0) {
    coef_df <- purrr::imap_dfr(model$models, function(m, nm) {
      broom::tidy(m) %>% dplyr::mutate(model = nm, .before = 1)
    })
  } else {
    coef_df <- tibble::tibble(
      model = character(),
      term = character(),
      estimate = double(),
      std.error = double(),
      statistic = double(),
      p.value = double()
    )
  }

  safe_write_csv(summary_df, summary_path)
  safe_write_csv(coverage_df, coverage_path)
  safe_write_csv(sample_df, sample_path)
  safe_write_csv(formulae_df, formulae_path)
  safe_write_csv(coef_df, coef_path)
  safe_write_csv(robust_scenarios_df, robust_scenarios_path)
  safe_write_csv(robust_coef_df, robust_coef_path)
  saveRDS(model, model_obj_path)

  c(summary_path, coverage_path, sample_path, formulae_path, coef_path, robust_scenarios_path, robust_coef_path, model_obj_path)
}

save_empirical_audit_outputs <- function(audit_bundle) {
  master_flags_path <- file.path(PROJ_PATHS$tables, "empirical_master_inclusion_flags.csv")
  module_c_flags_path <- file.path(PROJ_PATHS$tables, "empirical_module_c_inclusion_flags.csv")
  sample_flow_path <- file.path(PROJ_PATHS$tables, "empirical_sample_flow.csv")
  inclusion_comp_path <- file.path(PROJ_PATHS$tables, "empirical_inclusion_composition.csv")
  parent_availability_path <- file.path(PROJ_PATHS$tables, "empirical_parent_availability.csv")
  parent_robustness_path <- file.path(PROJ_PATHS$tables, "empirical_parent_measure_robustness.csv")
  parent_missingness_path <- file.path(PROJ_PATHS$tables, "empirical_parent_missingness_by_wave.csv")
  parent_missingness_compare_path <- file.path(PROJ_PATHS$tables, "empirical_parent_missingness_observables.csv")
  parent_missingness_sensitivity_path <- file.path(PROJ_PATHS$tables, "empirical_parent_missingness_sensitivity.csv")
  parent_measure_map_path <- file.path(PROJ_PATHS$tables, "empirical_parent_measure_map.csv")
  rank_change_tests_path <- file.path(PROJ_PATHS$tables, "empirical_rank_rank_change_tests.csv")
  subgroup_trend_path <- file.path(PROJ_PATHS$tables, "empirical_subgroup_trend_checks.csv")
  trend_comparison_path <- file.path(PROJ_PATHS$tables, "empirical_trend_comparison.csv")
  claim_audit_path <- file.path(PROJ_PATHS$tables, "empirical_claim_audit.csv")
  model_inventory_path <- file.path(PROJ_PATHS$tables, "empirical_model_inventory.csv")
  memo_path <- file.path(PROJ_PATHS$outputs, "publication", "empirical_audit_memo.md")

  safe_write_csv(audit_bundle$master_flags, master_flags_path)
  safe_write_csv(audit_bundle$module_c_flags, module_c_flags_path)
  safe_write_csv(audit_bundle$sample_flow, sample_flow_path)
  safe_write_csv(audit_bundle$inclusion_composition, inclusion_comp_path)
  safe_write_csv(audit_bundle$parent_availability, parent_availability_path)
  safe_write_csv(audit_bundle$parent_measure_robustness, parent_robustness_path)
  safe_write_csv(audit_bundle$parent_missingness_by_wave, parent_missingness_path)
  safe_write_csv(audit_bundle$parent_missingness_observables, parent_missingness_compare_path)
  safe_write_csv(audit_bundle$parent_missingness_sensitivity, parent_missingness_sensitivity_path)
  safe_write_csv(audit_bundle$parent_measure_map, parent_measure_map_path)
  safe_write_csv(audit_bundle$rank_rank_change_tests, rank_change_tests_path)
  safe_write_csv(audit_bundle$subgroup_trend_checks, subgroup_trend_path)
  safe_write_csv(audit_bundle$trend_comparison, trend_comparison_path)
  safe_write_csv(audit_bundle$claim_audit, claim_audit_path)
  safe_write_csv(audit_bundle$model_inventory, model_inventory_path)

  dir.create(dirname(memo_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(audit_bundle$memo_lines, con = memo_path, useBytes = TRUE)

  c(
    master_flags_path,
    module_c_flags_path,
    sample_flow_path,
    inclusion_comp_path,
    parent_availability_path,
    parent_robustness_path,
    parent_missingness_path,
    parent_missingness_compare_path,
    parent_missingness_sensitivity_path,
    parent_measure_map_path,
    rank_change_tests_path,
    subgroup_trend_path,
    trend_comparison_path,
    claim_audit_path,
    model_inventory_path,
    memo_path
  )
}
