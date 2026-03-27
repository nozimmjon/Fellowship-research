prepare_module_b_data <- function(df) {
  needed <- c("own_years_schooling", "parent_years_schooling", "region", "cohort")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  out <- df %>%
    dplyr::mutate(
      own_years_schooling = suppressWarnings(as.numeric(own_years_schooling)),
      parent_years_schooling = suppressWarnings(as.numeric(parent_years_schooling)),
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
      wave_year = suppressWarnings(as.integer(wave_year)),
      cohort = factor(cohort),
      region = factor(region),
      wave_year_fe = factor(wave_year)
    ) %>%
    dplyr::filter(!is.na(own_years_schooling), !is.na(parent_years_schooling))

  # Income is wave-specific nominal value; normalize within wave to improve comparability.
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

is_covariate_informative <- function(df, covariate, min_non_missing_share = 0.2) {
  if (!(covariate %in% names(df))) {
    return(FALSE)
  }
  x <- df[[covariate]]
  non_missing_share <- sum(!is.na(x)) / nrow(df)
  unique_non_missing <- dplyr::n_distinct(x, na.rm = TRUE)
  non_missing_share >= min_non_missing_share && unique_non_missing >= 2
}

build_module_b_formula <- function(lhs, rhs_terms, fe_terms = c("region", "cohort", "wave_year_fe")) {
  rhs <- paste(rhs_terms, collapse = " + ")
  fe <- paste(fe_terms, collapse = " + ")
  stats::as.formula(paste0(lhs, " ~ ", rhs, " | ", fe))
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
    "parent_years_schooling",
    "urban",
    "gender",
    "hh_income_proxy",
    "migration_exposure",
    "multigenerational_hh"
  )

  coverage <- build_covariate_coverage(mod_data, candidate_covariates)
  selected_covariates <- candidate_covariates[purrr::map_lgl(candidate_covariates, ~ is_covariate_informative(mod_data, .x))]

  # Always enforce parental schooling as core predictor.
  if (!("parent_years_schooling" %in% selected_covariates)) {
    selected_covariates <- unique(c("parent_years_schooling", selected_covariates))
  }

  rhs_for_ols <- selected_covariates
  f_ols <- build_module_b_formula("own_years_schooling", rhs_for_ols)

  model_ols <- fixest::feols(
    fml = f_ols,
    data = mod_data,
    vcov = ~region
  )

  mod_data <- mod_data %>%
    dplyr::mutate(
      upward_tertiary = as.integer(own_years_schooling >= 15 & parent_years_schooling <= 9)
    )

  can_fit_logit <- dplyr::n_distinct(mod_data$upward_tertiary, na.rm = TRUE) >= 2
  model_logit <- NULL
  f_logit <- NULL
  if (can_fit_logit) {
    rhs_for_logit <- selected_covariates
    f_logit <- build_module_b_formula("upward_tertiary", rhs_for_logit)
    model_logit <- fixest::feglm(
      fml = f_logit,
      family = "logit",
      data = mod_data,
      vcov = ~region
    )
  }

  formulae <- tibble::tibble(
    model = c("ols", "logit"),
    formula = c(
      deparse(f_ols, width.cutoff = 500),
      if (is.null(f_logit)) NA_character_ else deparse(f_logit, width.cutoff = 500)
    )
  )

  models <- list(ols = model_ols)
  if (!is.null(model_logit)) {
    models$logit <- model_logit
  }

  list(
    models = models,
    selected_covariates = selected_covariates,
    coverage = coverage,
    formulae = formulae
  )
}
