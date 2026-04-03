source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")
source("R/12_analysis_specs.R")
source("R/20_ingest_data.R")
source("R/30_module_a_mobility.R")
source("R/31_module_a_tier_a_descriptive.R")
source("R/40_module_b_determinants.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})

out_dir <- file.path(PROJ_PATHS$tables, "audit_two_pass")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

orig <- read_csv(file.path(PROJ_PATHS$processed_data, "lits_harmonized.csv"), show_col_types = FALSE) %>%
  mutate(row_id = row_number())

bad <- orig %>%
  filter(!is.na(sample_weight), sample_weight <= 0) %>%
  select(
    row_id, wave_year, country, region, age, gender, urban, sample_weight,
    own_ed_level, parent_ed_level, own_years_schooling, parent_years_schooling, cohort
  )

ma <- prepare_mobility_data(orig)
mb <- prepare_module_b_data(orig)

ma_rank_ids <- ma %>%
  filter(!is.na(own_years_schooling), !is.na(parent_years_schooling), !is.na(sample_weight), sample_weight > 0) %>%
  pull(row_id)
ma_cat_ids <- ma %>%
  filter(!is.na(own_ed_rank), !is.na(parent_ed_rank), !is.na(sample_weight), sample_weight > 0) %>%
  pull(row_id)
ma_trans_ids <- ma %>%
  filter(!is.na(own_ed_level), !is.na(parent_ed_level), !is.na(sample_weight), sample_weight > 0) %>%
  pull(row_id)
mb_ids <- if ("row_id" %in% names(mb)) pull(mb, row_id) else integer()

entry <- bad %>%
  transmute(
    row_id,
    wave_year,
    region,
    age,
    sample_weight,
    module_a_rank_sample = row_id %in% ma_rank_ids,
    module_a_category_sample = row_id %in% ma_cat_ids,
    module_a_transition_sample = row_id %in% ma_trans_ids,
    module_b_sample = row_id %in% mb_ids,
    module_c_sample = FALSE
  )

trimmed <- orig %>% filter(is.na(sample_weight) | sample_weight > 0)

module_a_orig <- estimate_mobility_metrics(orig)
module_a_trim <- estimate_mobility_metrics(trimmed)
tier_orig <- build_tier_a_descriptive(module_a_orig, orig)
tier_trim <- build_tier_a_descriptive(module_a_trim, trimmed)
module_b_orig <- fit_module_b_models(orig)
module_b_trim <- fit_module_b_models(trimmed)

canon <- function(df, digits = 10) {
  if (is.null(df) || nrow(df) == 0) {
    return(as_tibble(df))
  }
  out <- as_tibble(df)
  out[] <- lapply(out, function(col) {
    if (is.factor(col)) {
      as.character(col)
    } else if (is.logical(col)) {
      as.character(col)
    } else if (is.numeric(col)) {
      round(col, digits)
    } else {
      col
    }
  })
  out <- out[, sort(names(out)), drop = FALSE]
  out[do.call(order, c(out, list(na.last = TRUE))), , drop = FALSE]
}

same_tbl <- function(a, b) isTRUE(all.equal(canon(a), canon(b), check.attributes = FALSE))

coef_tbl <- function(model_list) {
  if (length(model_list$models) == 0) {
    return(tibble())
  }
  purrr::imap_dfr(model_list$models, function(m, nm) {
    broom::tidy(m) %>% mutate(model = nm, .before = 1)
  })
}

coef_orig <- coef_tbl(module_b_orig)
coef_trim <- coef_tbl(module_b_trim)
coef_keys <- c(
  "eq2_persistence_trend::parent_rank",
  "eq2_persistence_trend::wave_year_fe::2016:parent_rank",
  "eq2_persistence_trend::wave_year_fe::2022:parent_rank",
  "eq3_attainment_score::parent_ed_score"
)
coef_orig_key <- coef_orig %>%
  mutate(key = paste(model, term, sep = "::")) %>%
  filter(key %in% coef_keys) %>%
  select(model, term, estimate, std.error, p.value)
coef_trim_key <- coef_trim %>%
  mutate(key = paste(model, term, sep = "::")) %>%
  filter(key %in% coef_keys) %>%
  select(model, term, estimate, std.error, p.value)

sample_stats <- function(df) {
  wave_levels <- c(2010L, 2016L, 2022L)
  cohort_levels <- c("25-34", "35-44", "45-54", "55-64")
  one <- lapply(wave_levels, function(w) {
    wave_rows <- df %>% filter(wave_year == w)
    cohort_counts <- table(factor(wave_rows$cohort, levels = cohort_levels))
    cohort_shares <- if (nrow(wave_rows) > 0) 100 * as.numeric(cohort_counts) / nrow(wave_rows) else rep(NA_real_, length(cohort_levels))
    tibble(
      wave_year = w,
      n_total = nrow(wave_rows),
      female_share = mean(wave_rows$gender == "female", na.rm = TRUE),
      urban_share = mean(as.numeric(wave_rows$urban) == 1, na.rm = TRUE),
      cohort_comp = paste(sprintf("%.1f", cohort_shares), collapse = " / ")
    )
  })
  bind_rows(one)
}

comp_orig <- sample_stats(orig)
comp_trim <- sample_stats(trimmed)
comp_delta <- comp_orig %>%
  rename(
    n_total_orig = n_total,
    female_share_orig = female_share,
    urban_share_orig = urban_share,
    cohort_comp_orig = cohort_comp
  ) %>%
  left_join(
    comp_trim %>%
      rename(
        n_total_trim = n_total,
        female_share_trim = female_share,
        urban_share_trim = urban_share,
        cohort_comp_trim = cohort_comp
      ),
    by = "wave_year"
  ) %>%
  mutate(
    n_total_delta = n_total_trim - n_total_orig,
    female_share_delta = female_share_trim - female_share_orig,
    urban_share_delta = urban_share_trim - urban_share_orig,
    cohort_comp_changed = cohort_comp_trim != cohort_comp_orig
  )

impact <- bind_rows(
  tibble(
    component = "tier_a_sample_by_wave.csv",
    changed_if_removed = !same_tbl(tier_orig$sample_by_wave, tier_trim$sample_by_wave),
    note = "Used in manuscript sample composition table and text."
  ),
  tibble(
    component = "tier_a_data_completeness.csv",
    changed_if_removed = !same_tbl(tier_orig$data_completeness, tier_trim$data_completeness),
    note = "Used in manuscript parental-completeness text/table."
  ),
  tibble(
    component = "module_a_summary_metrics.csv",
    changed_if_removed = !same_tbl(module_a_orig$core_metrics, module_a_trim$core_metrics),
    note = "Feeds rank-rank and directional headline metrics."
  ),
  tibble(
    component = "tier_a_transition_summary.csv",
    changed_if_removed = !same_tbl(tier_orig$transition_summary, tier_trim$transition_summary),
    note = "Feeds transition-structure table."
  ),
  tibble(
    component = "tier_a_region_trends.csv",
    changed_if_removed = !same_tbl(tier_orig$region_trends, tier_trim$region_trends),
    note = "Feeds region rank-rank figure."
  ),
  tibble(
    component = "tier_a_subgroup_trends.csv",
    changed_if_removed = !same_tbl(tier_orig$subgroup_trends, tier_trim$subgroup_trends),
    note = "Feeds subgroup figure(s)."
  ),
  tibble(
    component = "module_b_formulae.csv",
    changed_if_removed = !same_tbl(module_b_orig$formulae, module_b_trim$formulae),
    note = "Model equation inventory."
  ),
  tibble(
    component = "module_b_key_coefficients",
    changed_if_removed = !same_tbl(coef_orig_key, coef_trim_key),
    note = "Main manuscript pooled-correlates claims."
  )
)

write_csv(bad, file.path(out_dir, "nonpositive_weight_rows.csv"))
write_csv(entry, file.path(out_dir, "nonpositive_weight_module_entry.csv"))
write_csv(impact, file.path(out_dir, "nonpositive_weight_removal_impact.csv"))
write_csv(comp_delta, file.path(out_dir, "nonpositive_weight_sample_table_delta.csv"))

cat("Wrote nonpositive-weight diagnostics to:", out_dir, "\n")
