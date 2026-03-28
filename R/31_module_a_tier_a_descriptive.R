build_tier_a_descriptive <- function(module_a_metrics, lits_harmonized) {
  empty <- list(
    sample_by_wave = tibble::tibble(),
    national_trends = tibble::tibble(),
    subgroup_trends = tibble::tibble(),
    region_trends = tibble::tibble(),
    transition_summary = tibble::tibble(),
    data_completeness = tibble::tibble()
  )

  if (is.null(module_a_metrics) || length(module_a_metrics) == 0) {
    return(empty)
  }

  core <- module_a_metrics$core_metrics
  subgroup <- module_a_metrics$subgroup_metrics
  transition <- module_a_metrics$transition_matrix

  if (is.null(core)) core <- tibble::tibble()
  if (is.null(subgroup)) subgroup <- tibble::tibble()
  if (is.null(transition)) transition <- tibble::tibble()

  sample_by_wave <- tibble::tibble()
  completeness <- tibble::tibble()
  if (!is.null(lits_harmonized) && nrow(lits_harmonized) > 0) {
    sample_by_wave <- lits_harmonized %>%
      dplyr::count(wave_year, name = "n_total") %>%
      dplyr::arrange(wave_year)

    completeness <- lits_harmonized %>%
      dplyr::group_by(wave_year) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        age_non_na = sum(!is.na(age)),
        own_ed_non_na = sum(!is.na(own_ed_level)),
        parent_ed_non_na = sum(!is.na(parent_ed_level)),
        own_years_non_na = sum(!is.na(own_years_schooling)),
        parent_years_non_na = sum(!is.na(parent_years_schooling)),
        region_non_na = sum(!is.na(region)),
        urban_non_na = sum(!is.na(urban)),
        gender_non_na = sum(!is.na(gender)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(wave_year)
  }

  national_trends <- core %>%
    dplyr::filter(status == "ok") %>%
    dplyr::select(dplyr::any_of(c("wave_year", "metric", "estimate", "std.error", "ci_low", "ci_high", "effective_n", "n", "status"))) %>%
    dplyr::arrange(metric, wave_year)

  subgroup_trends <- subgroup %>%
    dplyr::filter(
      status == "ok",
      subgroup_type %in% c("urban_rural", "gender", "cohort"),
      metric %in% c("rank_rank_slope", "upward_mobility_rate", "downward_mobility_rate", "persistence_probability")
    ) %>%
    dplyr::select(dplyr::any_of(c("subgroup_type", "subgroup_value", "wave_year", "metric", "estimate", "std.error", "ci_low", "ci_high", "effective_n", "n", "status"))) %>%
    dplyr::arrange(subgroup_type, subgroup_value, metric, wave_year)

  region_trends <- subgroup %>%
    dplyr::filter(
      status == "ok",
      subgroup_type == "region",
      metric %in% c("rank_rank_slope", "upward_mobility_rate")
    ) %>%
    dplyr::select(dplyr::any_of(c("subgroup_value", "wave_year", "metric", "estimate", "std.error", "ci_low", "ci_high", "effective_n", "n", "status"))) %>%
    dplyr::rename(region = subgroup_value) %>%
    dplyr::arrange(region, metric, wave_year)

  transition_summary <- transition %>%
    dplyr::select(wave_year, parent_ed_level, own_ed_level, n, n_parent_total, share, status) %>%
    dplyr::arrange(wave_year, parent_ed_level, own_ed_level)

  list(
    sample_by_wave = sample_by_wave,
    national_trends = national_trends,
    subgroup_trends = subgroup_trends,
    region_trends = region_trends,
    transition_summary = transition_summary,
    data_completeness = completeness
  )
}
