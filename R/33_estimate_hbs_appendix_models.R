build_hbs_appendix_model <- function(linkage_results) {
  diagnostics <- linkage_results$diagnostics
  linked_sample <- linkage_results$linked_adult_sample

  if (is.null(diagnostics) || nrow(diagnostics) == 0) {
    return(list(
      status = tibble::tibble(
        model = "hbs_coresident_attainment",
        model_run = FALSE,
        note = "HBS linkage diagnostics were not available, so the supplemental appendix model was not estimated."
      ),
      coefficients = tibble::tibble(),
      formula = tibble::tibble(model = "hbs_coresident_attainment", formula = "not estimated"),
      model = NULL
    ))
  }

  model_allowed <- isTRUE(diagnostics$model_justified[[1]])
  model_sample <- linked_sample %>%
    dplyr::filter(
      !is.na(own_education_years),
      !is.na(parent_proxy_years),
      !is.na(female),
      !is.na(urban),
      !is.na(cohort),
      !is.na(province)
    )

  if (!model_allowed || nrow(model_sample) < 250) {
    note_text <- if (nrow(model_sample) < 250) {
      "The linked co-resident HBS sample is too small after requiring complete model inputs, so no supplemental appendix model is reported."
    } else {
      "The linkage diagnostics indicate that the co-resident HBS sample is too selective for a supplemental intergenerational appendix model."
    }

    return(list(
      status = tibble::tibble(
        model = "hbs_coresident_attainment",
        model_run = FALSE,
        note = note_text
      ),
      coefficients = tibble::tibble(),
      formula = tibble::tibble(
        model = "hbs_coresident_attainment",
        formula = "own_education_years ~ parent_proxy_years + female + urban + cohort | province"
      ),
      model = NULL
    ))
  }

  model_fit <- fixest::feols(
    own_education_years ~ parent_proxy_years + female + urban + i(cohort) | province,
    data = model_sample,
    weights = ~ sample_weight
  )

  coef_df <- broom::tidy(model_fit) %>%
    dplyr::mutate(model = "hbs_coresident_attainment", .before = 1)

  list(
    status = tibble::tibble(
      model = "hbs_coresident_attainment",
      model_run = TRUE,
      note = "Supplemental HBS model estimated on a selective co-resident adult-child sample; results are supportive and not a substitute for the core LiTS evidence."
    ),
    coefficients = coef_df,
    formula = tibble::tibble(
      model = "hbs_coresident_attainment",
      formula = "own_education_years ~ parent_proxy_years + female + urban + cohort | province"
    ),
    model = model_fit
  )
}

write_hbs_appendix_model_outputs <- function(
  model_results,
  status_path = file.path(PROJ_PATHS$tables, "hbs_appendix_model_status.csv"),
  coefficients_path = file.path(PROJ_PATHS$tables, "hbs_appendix_model_coefficients.csv"),
  formula_path = file.path(PROJ_PATHS$tables, "hbs_appendix_model_formula.csv"),
  model_rds_path = file.path(PROJ_PATHS$models, "hbs_appendix_model.rds")
) {
  status_df <- model_results$status
  coef_df <- model_results$coefficients
  formula_df <- model_results$formula

  if (is.null(status_df)) status_df <- tibble::tibble()
  if (is.null(coef_df)) coef_df <- tibble::tibble()
  if (is.null(formula_df)) formula_df <- tibble::tibble()

  safe_write_csv(status_df, status_path)
  safe_write_csv(coef_df, coefficients_path)
  safe_write_csv(formula_df, formula_path)
  saveRDS(model_results$model, model_rds_path)

  c(status_path, coefficients_path, formula_path, model_rds_path)
}
