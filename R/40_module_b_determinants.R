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

  # Within-wave weighted ranks for Eq. 1/Eq. 2 persistence modeling.
  out <- out %>%
    dplyr::group_by(wave_year_fe) %>%
    dplyr::mutate(
      own_rank = weighted_rank(own_ed_score, sample_weight),
      parent_rank = weighted_rank(parent_ed_score, sample_weight)
    ) %>%
    dplyr::ungroup()

  # Normalize nominal income by wave for comparability.
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

  out <- out %>%
    dplyr::mutate(
      upward_any = as.integer(own_ed_score > parent_ed_score),
      persist_same = as.integer(own_ed_score == parent_ed_score),
      low_parent_sample = as.integer(parent_ed_score <= match("upper_secondary", EDUCATION_LEVELS))
    )

  out
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

fit_module_b_models <- function(df) {
  mod_data <- prepare_module_b_data(df)
  if (nrow(mod_data) < 100) {
    message("Insufficient observations for Module B. Returning empty output.")
    return(list(
      models = list(),
      selected_covariates = character(),
      coverage = tibble::tibble(),
      formulae = tibble::tibble()
    ))
  }

  candidate_covariates <- c(
    "urban",
    "female",
    "hh_income_proxy",
    "migration_exposure",
    "multigenerational_hh"
  )

  coverage <- build_covariate_coverage(mod_data, candidate_covariates)
  selected_optional <- candidate_covariates[purrr::map_lgl(candidate_covariates, ~ is_covariate_informative(mod_data, .x))]

  formula_rows <- list()
  models <- list()

  # Eq. 2: pooled persistence with wave interaction terms.
  rhs_eq2 <- rhs_with_optional(
    base_terms = c("parent_rank", "i(wave_year_fe, parent_rank, ref = '2010')", "urban", "female"),
    optional_terms = selected_optional[selected_optional %in% c("hh_income_proxy", "migration_exposure", "multigenerational_hh")]
  )
  f_eq2 <- build_fe_formula("own_rank", rhs_eq2)
  m_eq2 <- fit_feols_weighted(f_eq2, mod_data %>% dplyr::filter(!is.na(own_rank), !is.na(parent_rank)))
  if (!is.null(m_eq2)) {
    models$eq2_persistence_trend <- m_eq2
  }
  formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
    model = "eq2_persistence_trend",
    formula = deparse(f_eq2, width.cutoff = 500)
  )

  # Eq. 3: attainment score model (main correlates model).
  rhs_eq3 <- rhs_with_optional(
    base_terms = c("parent_ed_score", "urban", "female"),
    optional_terms = selected_optional[selected_optional %in% c("hh_income_proxy", "migration_exposure", "multigenerational_hh")]
  )
  f_eq3 <- build_fe_formula("own_ed_score", rhs_eq3)
  m_eq3 <- fit_feols_weighted(f_eq3, mod_data)
  if (!is.null(m_eq3)) {
    models$eq3_attainment_score <- m_eq3
  }
  formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
    model = "eq3_attainment_score",
    formula = deparse(f_eq3, width.cutoff = 500)
  )

  # Eq. 4A: upward mobility model on full sample.
  rhs_eq4 <- rhs_with_optional(
    base_terms = c("i(parent_ed_level, ref = 'upper_secondary')", "urban", "female"),
    optional_terms = selected_optional[selected_optional %in% c("hh_income_proxy", "migration_exposure", "multigenerational_hh")]
  )
  f_eq4 <- build_fe_formula("upward_any", rhs_eq4)
  m_eq4 <- fit_feols_weighted(f_eq4, mod_data)
  if (!is.null(m_eq4)) {
    models$eq4_upward_full_lpm <- m_eq4
  }
  formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
    model = "eq4_upward_full_lpm",
    formula = deparse(f_eq4, width.cutoff = 500)
  )

  # Eq. 4B: upward mobility on low-parent sample only.
  mod_data_low_parent <- mod_data %>% dplyr::filter(low_parent_sample == 1L)
  m_eq4_low <- fit_feols_weighted(f_eq4, mod_data_low_parent)
  if (!is.null(m_eq4_low)) {
    models$eq4_upward_lowparent_lpm <- m_eq4_low
  }
  formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
    model = "eq4_upward_lowparent_lpm",
    formula = deparse(f_eq4, width.cutoff = 500)
  )

  # Eq. 5: persistence heterogeneity interactions.
  rhs_eq5 <- rhs_with_optional(
    base_terms = c(
      "parent_ed_score",
      "parent_ed_score:urban",
      "parent_ed_score:female",
      "parent_ed_score:wave2022",
      "urban",
      "female"
    ),
    optional_terms = selected_optional[selected_optional %in% c("hh_income_proxy", "migration_exposure", "multigenerational_hh")]
  )
  f_eq5 <- build_fe_formula("persist_same", rhs_eq5)
  m_eq5 <- fit_feols_weighted(f_eq5, mod_data)
  if (!is.null(m_eq5)) {
    models$eq5_persistence_heterogeneity <- m_eq5
  }
  formula_rows[[length(formula_rows) + 1]] <- tibble::tibble(
    model = "eq5_persistence_heterogeneity",
    formula = deparse(f_eq5, width.cutoff = 500)
  )

  formulae <- dplyr::bind_rows(formula_rows)

  list(
    models = models,
    selected_covariates = selected_optional,
    coverage = coverage,
    formulae = formulae
  )
}
