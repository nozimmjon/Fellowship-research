build_mobility_measure_spec <- function() {
  tibble::tribble(
    ~metric_order, ~metric_id, ~metric_label, ~definition, ~estimation_unit, ~subgroup_splits, ~min_n, ~core_set,
    1L, "rank_rank_slope", "Years-of-schooling rank-rank slope", "OLS slope of respondent schooling rank on parental schooling rank.", "wave", "overall + urban/rural + gender + region + cohort", 30L, TRUE,
    2L, "transition_matrix_share", "Transition matrix share", "Conditional share of child education category within each parental education category.", "wave x parent_education x child_education", "overall", 30L, TRUE,
    3L, "upward_mobility_rate", "Upward mobility rate", "Share with higher education category than parent.", "wave", "overall + urban/rural + gender + region + cohort", 30L, TRUE,
    4L, "downward_mobility_rate", "Downward mobility rate", "Share with lower education category than parent.", "wave", "overall + urban/rural + gender + region + cohort", 30L, TRUE,
    5L, "persistence_probability", "Persistence probability", "Share with same education category as parent.", "wave", "overall + urban/rural + gender + region + cohort + parent_education", 30L, TRUE
  )
}

build_mobility_variable_lock <- function() {
  tibble::tribble(
    ~construct, ~standard_variable, ~type, ~required_for_module_a, ~coding_rule, ~allowed_values_or_units, ~missing_rule,
    "Survey wave", "wave_year", "integer", "yes", "Parse from source metadata; keep one of 2010, 2016, 2022, 2023.", "year", "Drop from wave-specific outputs if missing.",
    "Respondent education years", "own_years_schooling", "double", "yes (rank-rank)", "Numeric years of schooling completed by respondent.", "years", "Exclude from rank-rank if missing.",
    "Parental education years", "parent_years_schooling", "double", "yes (rank-rank)", "Numeric years of schooling for parent proxy variable.", "years", "Exclude from rank-rank if missing.",
    "Respondent education category", "own_ed_level", "character", "yes", "Map to ordered categories defined in EDUCATION_LEVELS.", "no_formal|primary|lower_secondary|upper_secondary|post_secondary_non_tertiary|tertiary", "Exclude from category-based metrics if missing.",
    "Parental education category", "parent_ed_level", "character", "yes", "Map to same ordered categories as respondent education.", "no_formal|primary|lower_secondary|upper_secondary|post_secondary_non_tertiary|tertiary", "Exclude from category-based metrics if missing.",
    "Region", "region", "character", "no (subgroup)", "Administrative region label harmonized across waves.", "region label", "If missing, skip region subgroup.",
    "Urban/rural", "urban", "integer/character", "no (subgroup)", "Map to binary urban/rural indicator then label.", "urban|rural", "If missing, skip urban/rural subgroup.",
    "Gender", "gender", "character", "no (subgroup)", "Map to male/female when available.", "male|female", "If missing, skip gender subgroup.",
    "Birth cohort band", "cohort", "character", "no (subgroup)", "Fixed bands: 25-34, 35-44, 45-54, 55-64.", "25-34|35-44|45-54|55-64", "If missing, skip cohort subgroup."
  )
}

write_mobility_measure_spec <- function(
  path = file.path(PROJ_PATHS$metadata, "mobility_measure_set.csv"),
  overwrite = TRUE
) {
  ensure_project_dirs()
  if (file.exists(path) && !overwrite) {
    return(path)
  }
  readr::write_csv(build_mobility_measure_spec(), path)
  path
}

write_mobility_variable_lock <- function(
  path = file.path(PROJ_PATHS$metadata, "mobility_variable_lock.csv"),
  overwrite = TRUE
) {
  ensure_project_dirs()
  if (file.exists(path) && !overwrite) {
    return(path)
  }
  readr::write_csv(build_mobility_variable_lock(), path)
  path
}
