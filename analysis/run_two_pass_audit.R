source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")
source("R/12_analysis_specs.R")
source("R/20_ingest_data.R")
source("R/30_module_a_mobility.R")
source("R/31_module_a_tier_a_descriptive.R")
source("R/31_build_hbs_household_context.R")
source("R/32_build_hbs_linkage_diagnostics.R")
source("R/40_module_b_determinants.R")
source("R/50_module_c_mechanisms.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(broom)
})

audit_dir <- file.path(PROJ_PATHS$tables, "audit_two_pass")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

timestamp_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
git_sha <- tryCatch(trimws(system("git rev-parse --short HEAD", intern = TRUE)), error = function(e) NA_character_)
if (length(git_sha) == 0 || identical(git_sha, "")) {
  git_sha <- NA_character_
}

as_status <- function(flag) ifelse(isTRUE(flag), "pass", "fail")

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

read_log_lines <- function(path) {
  if (!file.exists(path)) {
    return(character())
  }
  utf16 <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-16LE"), error = function(e) character())
  if (length(utf16) > 0) {
    return(utf16)
  }
  tryCatch(readLines(path, warn = FALSE), error = function(e) character())
}

fmt_num <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), as.numeric(x))
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * as.numeric(x))
fmt_int <- function(x) format(as.integer(round(as.numeric(x))), big.mark = ",", scientific = FALSE, trim = TRUE)

canonicalize_df <- function(df, numeric_digits = 12) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::as_tibble(df))
  }
  out <- tibble::as_tibble(df)
  out[] <- lapply(out, function(col) {
    if (is.factor(col)) {
      as.character(col)
    } else if (is.logical(col)) {
      as.character(col)
    } else if (is.numeric(col)) {
      round(col, numeric_digits)
    } else {
      col
    }
  })
  out <- out[, sort(names(out)), drop = FALSE]
  ord <- do.call(order, c(out, list(na.last = TRUE)))
  out[ord, , drop = FALSE]
}

compare_tables <- function(saved, rebuilt, numeric_digits = 10) {
  saved_tbl <- canonicalize_df(saved, numeric_digits = numeric_digits)
  rebuilt_tbl <- canonicalize_df(rebuilt, numeric_digits = numeric_digits)
  same <- isTRUE(all.equal(saved_tbl, rebuilt_tbl, check.attributes = FALSE))
  list(match = same, saved_n = nrow(saved_tbl), rebuilt_n = nrow(rebuilt_tbl))
}

# Tab 1: replication gate
replication_log_path <- file.path(audit_dir, "replication_run.log")
replication_log <- read_log_lines(replication_log_path)

report_outputs <- c(
  "outputs/publication/reports/00_main.html",
  "outputs/publication/reports/10_technical_appendix.html",
  "outputs/publication/reports/20_policy_brief.html",
  "outputs/publication/reports/30_slides.html"
)
report_exists <- file.exists(report_outputs)
report_mtime <- ifelse(
  report_exists,
  format(file.info(report_outputs)$mtime, "%Y-%m-%d %H:%M:%S %Z"),
  NA_character_
)

replication_log_size <- if (file.exists(replication_log_path)) file.info(replication_log_path)$size else NA_real_
replication_pass <- all(report_exists) && file.exists(replication_log_path) && !is.na(replication_log_size) && replication_log_size > 0

tab1 <- dplyr::bind_rows(
  tibble::tibble(
    check = "end-to-end rebuild",
    file = "06_replication.R",
    pass_condition = "Replication log captured and all four publication reports exist.",
    status = as_status(replication_pass),
    evidence = if (file.exists(replication_log_path)) "replication_run.log captured." else "replication log missing."
  ),
  tibble::tibble(
    check = "rendered publication artifacts",
    file = paste(report_outputs, collapse = "; "),
    pass_condition = "All four rendered HTML outputs exist.",
    status = as_status(all(report_exists)),
    evidence = paste(paste0(basename(report_outputs), " mtime=", report_mtime), collapse = " | ")
  ),
  tibble::tibble(
    check = "audit run metadata",
    file = "audit metadata",
    pass_condition = "Timestamp and git SHA recorded.",
    status = as_status(!is.na(timestamp_utc)),
    evidence = paste0("timestamp_utc=", timestamp_utc, "; git_sha=", ifelse(is.na(git_sha), "NA", git_sha))
  )
)

# Tab 2: data and processed gate
lits_raw_dir <- file.path(PROJ_PATHS$raw_data, "lits")
hbs_raw_dir <- file.path(PROJ_PATHS$raw_data, "hbs")
admin_raw_dir <- file.path(PROJ_PATHS$raw_data, "admin")
lits_processed_path <- file.path(PROJ_PATHS$processed_data, "lits_harmonized.csv")

lits_2010_present <- any(file.exists(c(file.path(lits_raw_dir, "lits_ii.csv"), file.path(lits_raw_dir, "lits2.dta"))))
lits_2016_present <- any(file.exists(c(file.path(lits_raw_dir, "lits_iii.dta"), file.path(lits_raw_dir, "lits_iii.csv"))))
lits_2022_present <- file.exists(file.path(lits_raw_dir, "lits_iv_dta", "lits_iv.dta"))

hbs_year_dirs <- if (dir.exists(hbs_raw_dir)) list.dirs(hbs_raw_dir, recursive = FALSE, full.names = TRUE) else character()
hbs_has_year_dirs <- length(hbs_year_dirs) > 0
hbs_expected_modules <- c("m00_weight\\.dta$", "m01_roster\\.dta$", "m03_education\\.dta$")
hbs_module_hits <- if (!hbs_has_year_dirs) {
  c(FALSE, FALSE, FALSE)
} else {
  purrr::map_lgl(hbs_expected_modules, function(pattern) {
    any(purrr::map_lgl(hbs_year_dirs, ~ length(list.files(.x, pattern = pattern, full.names = TRUE)) > 0))
  })
}
hbs_raw_present <- hbs_has_year_dirs && all(hbs_module_hits)

admin_files <- if (dir.exists(admin_raw_dir)) list.files(admin_raw_dir, full.names = TRUE) else character()
admin_non_gitkeep <- admin_files[basename(admin_files) != ".gitkeep"]
admin_present <- length(admin_non_gitkeep) > 0

lits_saved <- safe_read_csv(lits_processed_path)
lits_rebuilt <- build_lits_harmonized()
harmonized_exists <- file.exists(lits_processed_path)
harmonized_compare <- compare_tables(lits_saved, lits_rebuilt, numeric_digits = 10)

allowed_parent_levels <- sort(EDUCATION_LEVELS)
allowed_gender_levels <- c("male", "female")

wave_vals <- sort(unique(suppressWarnings(as.integer(lits_saved$wave_year))))
age_vals <- suppressWarnings(as.numeric(lits_saved$age))
weight_vals <- suppressWarnings(as.numeric(lits_saved$sample_weight))
parent_levels <- sort(unique(na.omit(as.character(lits_saved$parent_ed_level))))
gender_levels <- sort(unique(na.omit(as.character(lits_saved$gender))))
region_non_missing <- mean(!is.na(lits_saved$region))
weight_nonpos_n <- sum(!is.na(weight_vals) & weight_vals <= 0)
weight_missing_n <- sum(is.na(weight_vals))
gender_missing_n <- sum(is.na(lits_saved$gender))

wave_check <- identical(wave_vals, c(2010L, 2016L, 2022L))
age_check <- all(!is.na(age_vals) & age_vals >= ANALYSIS_SAMPLE$age_min & age_vals <= ANALYSIS_SAMPLE$age_max)
weight_check <- all(is.na(weight_vals) | weight_vals > 0)
parent_level_check <- all(parent_levels %in% allowed_parent_levels)
gender_check <- all(gender_levels %in% allowed_gender_levels)
region_check <- !is.na(region_non_missing) && region_non_missing >= 0.95

tab2 <- dplyr::bind_rows(
  tibble::tibble(
    check = "raw LiTS files present",
    source = "data/raw/lits/",
    pass_condition = "Expected 2010, 2016, and 2022 LiTS inputs present.",
    status = as_status(lits_2010_present && lits_2016_present && lits_2022_present),
    detail = paste0("2010=", lits_2010_present, "; 2016=", lits_2016_present, "; 2022=", lits_2022_present)
  ),
  tibble::tibble(
    check = "raw HBS files present",
    source = "data/raw/hbs/",
    pass_condition = "At least one year folder with m00_weight, m01_roster, and m03_education modules.",
    status = as_status(hbs_raw_present),
    detail = if (hbs_has_year_dirs) paste0("year_dirs=", length(hbs_year_dirs), "; module_hits=", paste(hbs_module_hits, collapse = ",")) else "No year folders found."
  ),
  tibble::tibble(
    check = "raw admin files present",
    source = "data/raw/admin/",
    pass_condition = "At least one non-.gitkeep admin file present.",
    status = as_status(admin_present),
    detail = paste0("non_gitkeep_files=", length(admin_non_gitkeep))
  ),
  tibble::tibble(
    check = "harmonized LiTS built",
    source = "data/processed/lits_harmonized.csv",
    pass_condition = "Saved harmonized file exists and matches direct rebuild from raw LiTS.",
    status = as_status(harmonized_exists && harmonized_compare$match),
    detail = paste0("saved_n=", harmonized_compare$saved_n, "; rebuilt_n=", harmonized_compare$rebuilt_n)
  ),
  tibble::tibble(
    check = "harmonized variables sane",
    source = "data/processed/lits_harmonized.csv",
    pass_condition = "Wave, age, sex, region, parental education, and weights pass sanity checks.",
    status = as_status(wave_check && age_check && weight_check && parent_level_check && gender_check && region_check),
    detail = paste0(
      "wave_ok=", wave_check,
      "; age_ok=", age_check,
      "; weight_ok=", weight_check, " (nonpositive=", weight_nonpos_n, "; missing=", weight_missing_n, ")",
      "; gender_ok=", gender_check, " (missing=", gender_missing_n, ")",
      "; parent_level_ok=", parent_level_check,
      "; region_non_missing=", sprintf("%.3f", region_non_missing)
    )
  )
)

# Tab 3: module outputs gate
module_a_metrics_rebuilt <- estimate_mobility_metrics(lits_rebuilt)
tier_a_rebuilt <- build_tier_a_descriptive(module_a_metrics_rebuilt, lits_rebuilt)
module_b_rebuilt <- fit_module_b_models(lits_rebuilt)
module_c_rebuilt <- fit_module_c_mechanisms()
hbs_context_rebuilt <- build_hbs_household_context()
hbs_linkage_rebuilt <- build_hbs_linkage_diagnostics()

module_a_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_a_summary_metrics.csv"))
tier_sample_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_sample_by_wave.csv"))
tier_completeness_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_data_completeness.csv"))
tier_transition_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_transition_summary.csv"))
tier_region_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_region_trends.csv"))
tier_subgroup_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_subgroup_trends.csv"))
module_b_formula_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_b_formulae.csv"))
module_b_coef_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_b_model_coefficients.csv"))
module_b_cov_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_b_selected_covariates.csv"))
module_c_sample_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_c_mechanism_sample.csv"))
module_c_scenarios_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_c_mechanism_robustness_scenarios.csv"))
module_c_robust_coef_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_c_mechanism_robustness_coefficients.csv"))
hbs_context_saved <- safe_read_csv(file.path(PROJ_PATHS$tables, "hbs_household_support_context.csv"))
hbs_linkage_diag_saved <- safe_read_csv(file.path(PROJ_PATHS$processed_data, "hbs_linkage_diagnostics.csv"))

module_a_compare <- compare_tables(
  module_a_saved[, c("subgroup_type", "subgroup_value", "wave_year", "metric", "estimate", "std.error", "ci_low", "ci_high", "effective_n", "n", "status")],
  module_a_metrics_rebuilt$core_metrics[, c("subgroup_type", "subgroup_value", "wave_year", "metric", "estimate", "std.error", "ci_low", "ci_high", "effective_n", "n", "status")]
)
tier_sample_compare <- compare_tables(tier_sample_saved, tier_a_rebuilt$sample_by_wave)
tier_complete_compare <- compare_tables(tier_completeness_saved, tier_a_rebuilt$data_completeness)
tier_transition_compare <- compare_tables(tier_transition_saved, tier_a_rebuilt$transition_summary)
tier_region_compare <- compare_tables(tier_region_saved, tier_a_rebuilt$region_trends)
tier_subgroup_compare <- compare_tables(tier_subgroup_saved, tier_a_rebuilt$subgroup_trends)

module_b_formula_compare <- compare_tables(
  module_b_formula_saved[, c("model", "formula")],
  module_b_rebuilt$formulae[, c("model", "formula")]
)
module_b_cov_compare <- compare_tables(module_b_cov_saved, tibble::tibble(covariate = module_b_rebuilt$selected_covariates))
module_b_coef_rebuilt <- if (length(module_b_rebuilt$models) > 0) {
  purrr::imap_dfr(module_b_rebuilt$models, function(m, nm) broom::tidy(m) %>% dplyr::mutate(model = nm, .before = 1))
} else {
  tibble::tibble()
}
module_b_coef_key_terms <- c(
  "eq2_persistence_trend::parent_rank",
  "eq2_persistence_trend::wave_year_fe::2016:parent_rank",
  "eq2_persistence_trend::wave_year_fe::2022:parent_rank",
  "eq3_attainment_score::parent_ed_score"
)
module_b_coef_saved_key <- module_b_coef_saved %>%
  dplyr::mutate(key = paste(model, term, sep = "::")) %>%
  dplyr::filter(key %in% module_b_coef_key_terms) %>%
  dplyr::select(model, term, estimate, std.error, p.value)
module_b_coef_rebuilt_key <- module_b_coef_rebuilt %>%
  dplyr::mutate(key = paste(model, term, sep = "::")) %>%
  dplyr::filter(key %in% module_b_coef_key_terms) %>%
  dplyr::select(model, term, estimate, std.error, p.value)
module_b_coef_compare <- compare_tables(module_b_coef_saved_key, module_b_coef_rebuilt_key, numeric_digits = 9)

module_c_sample_compare <- compare_tables(module_c_sample_saved, module_c_rebuilt$sample_overview)
module_c_scenario_compare <- compare_tables(module_c_scenarios_saved, module_c_rebuilt$robustness_scenarios, numeric_digits = 9)
module_c_robust_key_saved <- module_c_robust_coef_saved %>%
  dplyr::filter(model == "m3_education_stopped_covid", term %in% c("parent_low_edu", NA)) %>%
  dplyr::select(scenario_id, model, term, estimate, p.value, n_used, status, support_reason)
module_c_robust_key_rebuilt <- module_c_rebuilt$robustness_coefficients %>%
  dplyr::filter(model == "m3_education_stopped_covid", term %in% c("parent_low_edu", NA)) %>%
  dplyr::select(scenario_id, model, term, estimate, p.value, n_used, status, support_reason)
module_c_robust_compare <- compare_tables(module_c_robust_key_saved, module_c_robust_key_rebuilt, numeric_digits = 9)

hbs_context_compare <- compare_tables(hbs_context_saved, hbs_context_rebuilt$household_support_context, numeric_digits = 9)
hbs_linkage_compare <- compare_tables(hbs_linkage_diag_saved, hbs_linkage_rebuilt$diagnostics, numeric_digits = 9)

module_output_files <- c(
  "module_a_summary_metrics.csv",
  "module_a_transition_matrix.csv",
  "tier_a_sample_by_wave.csv",
  "tier_a_data_completeness.csv",
  "tier_a_transition_summary.csv",
  "tier_a_region_trends.csv",
  "tier_a_subgroup_trends.csv",
  "module_b_model_coefficients.csv",
  "module_b_formulae.csv",
  "module_b_selected_covariates.csv",
  "module_c_mechanism_sample.csv",
  "module_c_mechanism_robustness_scenarios.csv",
  "module_c_mechanism_robustness_coefficients.csv",
  "hbs_household_support_context.csv"
)
module_output_exists <- file.exists(file.path(PROJ_PATHS$tables, module_output_files))

policy_brief_text <- paste(readLines(file.path(PROJ_PATHS$reports, "20_policy_brief.qmd"), warn = FALSE), collapse = " ")
policy_slopes_match <- stringr::str_match(policy_brief_text, "rises from ([0-9]+\\.[0-9]+) in 2010 to ([0-9]+\\.[0-9]+) in 2016 and remains elevated at ([0-9]+\\.[0-9]+) in 2022-23")
policy_challenge_match <- stringr::str_match(policy_brief_text, "([0-9]+) percent of households report at least one remote-learning challenge, and mothers are reported as the main support channel in ([0-9]+) percent")
policy_spending_match <- stringr::str_match(policy_brief_text, "Across pooled HBS 2021-2025 households, ([0-9]+) percent report positive education spending, ([0-9]+) percent report tutoring, ([0-9]+) percent receive remittances, and about ([0-9]+) percent report internet access")

source_slope_values <- c(
  round(as.numeric(module_a_saved$estimate[module_a_saved$metric == "rank_rank_slope" & module_a_saved$wave_year == 2010]), 2),
  round(as.numeric(module_a_saved$estimate[module_a_saved$metric == "rank_rank_slope" & module_a_saved$wave_year == 2016]), 2),
  round(as.numeric(module_a_saved$estimate[module_a_saved$metric == "rank_rank_slope" & module_a_saved$wave_year == 2022]), 2)
)
policy_slope_values <- as.numeric(policy_slopes_match[2:4])
crossdoc_slope_consistent <- all(!is.na(policy_slope_values)) && all.equal(source_slope_values, policy_slope_values) == TRUE

source_challenge_values <- c(
  round(100 * as.numeric(module_c_rebuilt$summary$estimate[module_c_rebuilt$summary$outcome == "any_remote_challenge" & module_c_rebuilt$summary$group == "overall"]), 0),
  round(100 * as.numeric(module_c_rebuilt$summary$estimate[module_c_rebuilt$summary$outcome == "support_mother" & module_c_rebuilt$summary$group == "overall"]), 0)
)
policy_challenge_values <- as.numeric(policy_challenge_match[2:3])
crossdoc_challenge_consistent <- all(!is.na(policy_challenge_values)) && all.equal(source_challenge_values, policy_challenge_values) == TRUE

source_hbs_values <- c(
  round(100 * as.numeric(hbs_context_saved$national[hbs_context_saved$metric == "education_spending_positive"]), 0),
  round(100 * as.numeric(hbs_context_saved$national[hbs_context_saved$metric == "has_tutoring"]), 0),
  round(100 * as.numeric(hbs_context_saved$national[hbs_context_saved$metric == "has_remittance_hh"]), 0),
  round(100 * as.numeric(hbs_context_saved$national[hbs_context_saved$metric == "internet_access_hh"]), 0)
)
policy_hbs_values <- as.numeric(policy_spending_match[2:5])
crossdoc_hbs_consistent <- all(!is.na(policy_hbs_values)) && all.equal(source_hbs_values, policy_hbs_values) == TRUE

tab3 <- dplyr::bind_rows(
  tibble::tibble(check = "required module output files exist", source_script = "R/30, R/31, R/40, R/50, R/60", pass_condition = "All required table outputs are present.", status = as_status(all(module_output_exists)), detail = paste0("missing=", paste(module_output_files[!module_output_exists], collapse = ", "))),
  tibble::tibble(check = "Module A core metrics reproducible", source_script = "R/30_module_a_mobility.R", pass_condition = "Recomputed module_a_summary_metrics equals saved output.", status = as_status(module_a_compare$match), detail = paste0("saved_n=", module_a_compare$saved_n, "; rebuilt_n=", module_a_compare$rebuilt_n)),
  tibble::tibble(check = "Tier A sample-by-wave reproducible", source_script = "R/31_module_a_tier_a_descriptive.R", pass_condition = "Recomputed tier_a_sample_by_wave equals saved output.", status = as_status(tier_sample_compare$match), detail = paste0("saved_n=", tier_sample_compare$saved_n, "; rebuilt_n=", tier_sample_compare$rebuilt_n)),
  tibble::tibble(check = "Tier A completeness reproducible", source_script = "R/31_module_a_tier_a_descriptive.R", pass_condition = "Recomputed tier_a_data_completeness equals saved output.", status = as_status(tier_complete_compare$match), detail = paste0("saved_n=", tier_complete_compare$saved_n, "; rebuilt_n=", tier_complete_compare$rebuilt_n)),
  tibble::tibble(check = "Tier A transition summary reproducible", source_script = "R/31_module_a_tier_a_descriptive.R", pass_condition = "Recomputed tier_a_transition_summary equals saved output.", status = as_status(tier_transition_compare$match), detail = paste0("saved_n=", tier_transition_compare$saved_n, "; rebuilt_n=", tier_transition_compare$rebuilt_n)),
  tibble::tibble(check = "Tier A region trends reproducible", source_script = "R/31_module_a_tier_a_descriptive.R", pass_condition = "Recomputed tier_a_region_trends equals saved output.", status = as_status(tier_region_compare$match), detail = paste0("saved_n=", tier_region_compare$saved_n, "; rebuilt_n=", tier_region_compare$rebuilt_n)),
  tibble::tibble(check = "Tier A subgroup trends reproducible", source_script = "R/31_module_a_tier_a_descriptive.R", pass_condition = "Recomputed tier_a_subgroup_trends equals saved output.", status = as_status(tier_subgroup_compare$match), detail = paste0("saved_n=", tier_subgroup_compare$saved_n, "; rebuilt_n=", tier_subgroup_compare$rebuilt_n)),
  tibble::tibble(check = "Module B formulae reproducible", source_script = "R/40_module_b_determinants.R", pass_condition = "Recomputed module_b_formulae equals saved output.", status = as_status(module_b_formula_compare$match), detail = paste0("saved_n=", module_b_formula_compare$saved_n, "; rebuilt_n=", module_b_formula_compare$rebuilt_n)),
  tibble::tibble(check = "Module B selected covariates reproducible", source_script = "R/40_module_b_determinants.R", pass_condition = "Recomputed module_b_selected_covariates equals saved output.", status = as_status(module_b_cov_compare$match), detail = paste0("saved_n=", module_b_cov_compare$saved_n, "; rebuilt_n=", module_b_cov_compare$rebuilt_n)),
  tibble::tibble(check = "Module B key coefficients reproducible", source_script = "R/40_module_b_determinants.R", pass_condition = "Recomputed key Module B coefficients equal saved output.", status = as_status(module_b_coef_compare$match), detail = paste0("saved_n=", module_b_coef_compare$saved_n, "; rebuilt_n=", module_b_coef_compare$rebuilt_n)),
  tibble::tibble(check = "Module C sample flow reproducible", source_script = "R/50_module_c_mechanisms.R", pass_condition = "Recomputed module_c_mechanism_sample equals saved output.", status = as_status(module_c_sample_compare$match), detail = paste0("saved_n=", module_c_sample_compare$saved_n, "; rebuilt_n=", module_c_sample_compare$rebuilt_n)),
  tibble::tibble(check = "Module C robustness scenarios reproducible", source_script = "R/50_module_c_mechanisms.R", pass_condition = "Recomputed module_c_mechanism_robustness_scenarios equals saved output.", status = as_status(module_c_scenario_compare$match), detail = paste0("saved_n=", module_c_scenario_compare$saved_n, "; rebuilt_n=", module_c_scenario_compare$rebuilt_n)),
  tibble::tibble(check = "Module C stoppage robustness coefficients reproducible", source_script = "R/50_module_c_mechanisms.R", pass_condition = "Recomputed key robustness-coefficient rows equal saved output.", status = as_status(module_c_robust_compare$match), detail = paste0("saved_n=", module_c_robust_compare$saved_n, "; rebuilt_n=", module_c_robust_compare$rebuilt_n)),
  tibble::tibble(check = "HBS support context reproducible from current raw data", source_script = "R/31_build_hbs_household_context.R", pass_condition = "Recomputed hbs_household_support_context equals saved output.", status = as_status(hbs_context_compare$match), detail = paste0("saved_n=", hbs_context_compare$saved_n, "; rebuilt_n=", hbs_context_compare$rebuilt_n)),
  tibble::tibble(check = "HBS linkage diagnostics reproducible from current raw data", source_script = "R/32_build_hbs_linkage_diagnostics.R", pass_condition = "Recomputed hbs_linkage_diagnostics equals saved processed output.", status = as_status(hbs_linkage_compare$match), detail = paste0("saved_n=", hbs_linkage_compare$saved_n, "; rebuilt_n=", hbs_linkage_compare$rebuilt_n)),
  tibble::tibble(check = "cross-document consistency: rank-rank headline", source_script = "reports/00_main.qmd + reports/20_policy_brief.qmd + reports/30_slides.qmd", pass_condition = "Policy-brief hard-coded rank-rank headline matches source outputs (2 decimals).", status = as_status(crossdoc_slope_consistent), detail = paste0("source=", paste(source_slope_values, collapse = "/"), "; policy=", paste(policy_slope_values, collapse = "/"))),
  tibble::tibble(check = "cross-document consistency: Module C headline percentages", source_script = "reports/00_main.qmd + reports/20_policy_brief.qmd + reports/30_slides.qmd", pass_condition = "Policy-brief challenge/support percentages match source outputs (rounded).", status = as_status(crossdoc_challenge_consistent), detail = paste0("source=", paste(source_challenge_values, collapse = "/"), "; policy=", paste(policy_challenge_values, collapse = "/"))),
  tibble::tibble(check = "cross-document consistency: HBS contextual percentages", source_script = "reports/00_main.qmd + reports/20_policy_brief.qmd", pass_condition = "Policy-brief HBS rounded percentages match source outputs.", status = as_status(crossdoc_hbs_consistent), detail = paste0("source=", paste(source_hbs_values, collapse = "/"), "; policy=", paste(policy_hbs_values, collapse = "/")))
)

# Tab 4: manuscript claims cross-check
sample_by_wave <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_sample_by_wave.csv"))
data_completeness <- safe_read_csv(file.path(PROJ_PATHS$tables, "tier_a_data_completeness.csv"))
module_c_sample <- safe_read_csv(file.path(PROJ_PATHS$tables, "module_c_mechanism_sample.csv"))
hbs_linkage_diag <- safe_read_csv(file.path(PROJ_PATHS$processed_data, "hbs_linkage_diagnostics.csv"))

metric_val <- function(metric, wave) as.numeric(module_a_saved$estimate[module_a_saved$metric == metric & module_a_saved$wave_year == wave & module_a_saved$subgroup_type == "overall" & module_a_saved$subgroup_value == "all"][1])
coef_val <- function(model, term) as.numeric(module_b_coef_saved$estimate[module_b_coef_saved$model == model & module_b_coef_saved$term == term][1])
coef_p <- function(model, term) as.numeric(module_b_coef_saved$p.value[module_b_coef_saved$model == model & module_b_coef_saved$term == term][1])
sample_flow_n <- function(step_name) as.numeric(module_c_sample$n[module_c_sample$step == step_name][1])
completeness_rate <- function(wave) {
  row <- data_completeness[data_completeness$wave_year == wave, ]
  as.numeric(row$parent_ed_non_na[1]) / as.numeric(row$n_total[1])
}

tab4 <- tibble::tibble(
  manuscript_statement = c(
    "Abstract rank-rank slope (2010)",
    "Abstract rank-rank slope (2016)",
    "Abstract rank-rank slope (2022-23)",
    "Sample size by wave (2010)",
    "Sample size by wave (2016)",
    "Sample size by wave (2022-23)",
    "Parental education completeness (2010)",
    "Parental education completeness (2016)",
    "Parental education completeness (2022-23)",
    "Module B directional claim: parent_rank positive",
    "Module B directional claim: 2016 interaction positive and precise",
    "Module B directional claim: 2022 interaction positive but less precise",
    "Module B directional claim: parent_ed_score positive",
    "Module C sample-flow: LiTS IV respondents",
    "Module C sample-flow: child enrolled pre-COVID",
    "Module C sample-flow: mechanism sample with non-missing parent schooling",
    "HBS limitation: linkage rate under conservative rule",
    "HBS limitation: HBS model not justified by diagnostics"
  ),
  source_file = c(
    "outputs/tables/module_a_summary_metrics.csv",
    "outputs/tables/module_a_summary_metrics.csv",
    "outputs/tables/module_a_summary_metrics.csv",
    "outputs/tables/tier_a_sample_by_wave.csv",
    "outputs/tables/tier_a_sample_by_wave.csv",
    "outputs/tables/tier_a_sample_by_wave.csv",
    "outputs/tables/tier_a_data_completeness.csv",
    "outputs/tables/tier_a_data_completeness.csv",
    "outputs/tables/tier_a_data_completeness.csv",
    "outputs/tables/module_b_model_coefficients.csv",
    "outputs/tables/module_b_model_coefficients.csv",
    "outputs/tables/module_b_model_coefficients.csv",
    "outputs/tables/module_b_model_coefficients.csv",
    "outputs/tables/module_c_mechanism_sample.csv",
    "outputs/tables/module_c_mechanism_sample.csv",
    "outputs/tables/module_c_mechanism_sample.csv",
    "data/processed/hbs_linkage_diagnostics.csv",
    "data/processed/hbs_linkage_diagnostics.csv"
  ),
  source_field_formula = c(
    "estimate where metric=rank_rank_slope,wave_year=2010",
    "estimate where metric=rank_rank_slope,wave_year=2016",
    "estimate where metric=rank_rank_slope,wave_year=2022",
    "n_total where wave_year=2010",
    "n_total where wave_year=2016",
    "n_total where wave_year=2022",
    "parent_ed_non_na / n_total where wave_year=2010",
    "parent_ed_non_na / n_total where wave_year=2016",
    "parent_ed_non_na / n_total where wave_year=2022",
    "sign(estimate) where model=eq2_persistence_trend,term=parent_rank",
    "estimate>0 and p<0.05 where term=wave_year_fe::2016:parent_rank",
    "estimate>0 and p>=0.05 where term=wave_year_fe::2022:parent_rank",
    "sign(estimate) where model=eq3_attainment_score,term=parent_ed_score",
    "n where step='Uzbekistan LiTS IV respondents'",
    "n where step='Respondents with child enrolled pre-COVID (q713 = yes)'",
    "n where step='Mechanism sample with non-missing parental schooling'",
    "link_rate",
    "model_justified == FALSE"
  ),
  paper_value = c(
    fmt_num(metric_val("rank_rank_slope", 2010), 3),
    fmt_num(metric_val("rank_rank_slope", 2016), 3),
    fmt_num(metric_val("rank_rank_slope", 2022), 3),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2010][1]),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2016][1]),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2022][1]),
    fmt_pct(completeness_rate(2010), 1),
    fmt_pct(completeness_rate(2016), 1),
    fmt_pct(completeness_rate(2022), 1),
    "positive",
    "positive + precisely estimated",
    "positive + less precise",
    "positive",
    fmt_int(sample_flow_n("Uzbekistan LiTS IV respondents")),
    fmt_int(sample_flow_n("Respondents with child enrolled pre-COVID (q713 = yes)")),
    fmt_int(sample_flow_n("Mechanism sample with non-missing parental schooling")),
    fmt_pct(as.numeric(hbs_linkage_diag$link_rate[1]), 1),
    "not justified"
  ),
  recomputed_value = c(
    fmt_num(metric_val("rank_rank_slope", 2010), 3),
    fmt_num(metric_val("rank_rank_slope", 2016), 3),
    fmt_num(metric_val("rank_rank_slope", 2022), 3),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2010][1]),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2016][1]),
    fmt_int(sample_by_wave$n_total[sample_by_wave$wave_year == 2022][1]),
    fmt_pct(completeness_rate(2010), 1),
    fmt_pct(completeness_rate(2016), 1),
    fmt_pct(completeness_rate(2022), 1),
    ifelse(coef_val("eq2_persistence_trend", "parent_rank") > 0, "positive", "non-positive"),
    ifelse(coef_val("eq2_persistence_trend", "wave_year_fe::2016:parent_rank") > 0 && coef_p("eq2_persistence_trend", "wave_year_fe::2016:parent_rank") < 0.05, "positive + precisely estimated", "claim not met"),
    ifelse(coef_val("eq2_persistence_trend", "wave_year_fe::2022:parent_rank") > 0 && coef_p("eq2_persistence_trend", "wave_year_fe::2022:parent_rank") >= 0.05, "positive + less precise", "claim not met"),
    ifelse(coef_val("eq3_attainment_score", "parent_ed_score") > 0, "positive", "non-positive"),
    fmt_int(sample_flow_n("Uzbekistan LiTS IV respondents")),
    fmt_int(sample_flow_n("Respondents with child enrolled pre-COVID (q713 = yes)")),
    fmt_int(sample_flow_n("Mechanism sample with non-missing parental schooling")),
    fmt_pct(as.numeric(hbs_linkage_diag$link_rate[1]), 1),
    ifelse(!isTRUE(as.logical(hbs_linkage_diag$model_justified[1])), "not justified", "justified")
  )
) %>% dplyr::mutate(match = dplyr::if_else(paper_value == recomputed_value, "pass", "fail"))

pass1_clean <- all(tab1$status == "pass") &&
  all(tab2$status == "pass") &&
  all(tab3$status == "pass") &&
  all(tab4$match == "pass")

pass1_summary <- tibble::tibble(
  gate = c("Replication", "Data and processed", "Module outputs", "Paper claims"),
  status = c(
    ifelse(all(tab1$status == "pass"), "pass", "fail"),
    ifelse(all(tab2$status == "pass"), "pass", "fail"),
    ifelse(all(tab3$status == "pass"), "pass", "fail"),
    ifelse(all(tab4$match == "pass"), "pass", "fail")
  )
)

# Pass 2: methodology gate (only if pass 1 is clean)
main_text <- paste(readLines(file.path(PROJ_PATHS$reports, "00_main.qmd"), warn = FALSE), collapse = " ")
module_b_associational_language <- grepl("associational", main_text, ignore.case = TRUE) && grepl("not.*causal", main_text, ignore.case = TRUE)
module_c_bounded_language <- grepl("bounded", main_text, ignore.case = TRUE) && grepl("non-causal", main_text, ignore.case = TRUE)
hbs_supplementary_language <- grepl("supplementary", main_text, ignore.case = TRUE) && grepl("not the source of the intergenerational estimates", main_text, ignore.case = TRUE)

region_coverage <- lits_saved %>% dplyr::group_by(wave_year) %>% dplyr::summarise(n_regions = dplyr::n_distinct(region[!is.na(region)]), .groups = "drop")
region_differs <- dplyr::n_distinct(region_coverage$n_regions) > 1

pass2 <- if (pass1_clean) {
  tibble::tibble(
    question = c(
      "Are cross-wave comparisons comparable beyond mechanical harmonization?",
      "Does Module B remain strictly associational in interpretation?",
      "Is Module C interpretation bounded given sample size and fragility?",
      "Is HBS used consistently with linkage limitations?"
    ),
    status = c(
      ifelse(region_differs, "caution", "pass"),
      ifelse(module_b_associational_language, "pass", "caution"),
      ifelse(module_c_bounded_language, "pass", "caution"),
      ifelse(hbs_supplementary_language && !isTRUE(as.logical(hbs_linkage_diag$model_justified[1])), "pass", "caution")
    ),
    assessment = c(
      ifelse(region_differs, paste0("Regional coverage differs by wave (n_regions=", paste(region_coverage$n_regions, collapse = "/"), "); comparisons are composition-conditional."), "Regional coverage appears stable across waves."),
      ifelse(module_b_associational_language, "Main manuscript labels Module B as associational and non-causal.", "Associational-only framing is not explicit enough."),
      ifelse(module_c_bounded_language, "Module C is explicitly bounded and non-causal; fragility checks are present.", "Module C language may overstate interpretation."),
      ifelse(hbs_supplementary_language && !isTRUE(as.logical(hbs_linkage_diag$model_justified[1])), "HBS is supplementary and diagnostics reject direct appendix estimation.", "HBS usage appears inconsistent with diagnostics.")
    )
  )
} else {
  tibble::tibble(
    question = "Pass 2 methodology audit",
    status = "deferred",
    assessment = "Deferred because Pass 1 computation audit is not fully clean."
  )
}

readr::write_csv(tab1, file.path(audit_dir, "tab1_replication.csv"))
readr::write_csv(tab2, file.path(audit_dir, "tab2_data_processed.csv"))
readr::write_csv(tab3, file.path(audit_dir, "tab3_outputs.csv"))
readr::write_csv(tab4, file.path(audit_dir, "tab4_paper_claims.csv"))
readr::write_csv(pass1_summary, file.path(audit_dir, "pass1_summary.csv"))
readr::write_csv(pass2, file.path(audit_dir, "pass2_methodology.csv"))

overview_lines <- c(
  "# Two-Pass Audit Overview",
  "",
  paste0("- Timestamp (UTC): ", timestamp_utc),
  paste0("- Git SHA: ", ifelse(is.na(git_sha), "NA", git_sha)),
  paste0("- Pass 1 clean: ", pass1_clean),
  paste0("- Pass 2 status: ", paste(unique(pass2$status), collapse = ",")),
  "",
  "## Files",
  "- tab1_replication.csv",
  "- tab2_data_processed.csv",
  "- tab3_outputs.csv",
  "- tab4_paper_claims.csv",
  "- pass1_summary.csv",
  "- pass2_methodology.csv"
)
writeLines(overview_lines, con = file.path(audit_dir, "audit_overview.md"), useBytes = TRUE)

cat("Audit tables written to:", audit_dir, "\n")
cat("Pass 1 clean:", pass1_clean, "\n")
