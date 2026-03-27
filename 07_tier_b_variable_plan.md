# 07 Tier B Variable Plan

## Objective
Tighten correlates modeling to avoid unstable specifications and include only informative covariates.

## Core Tier B Covariates (active now)
- `parent_years_schooling`
- `urban`
- `gender`
- Region fixed effects
- Cohort fixed effects

## Conditional Covariates (included only if informative)
- `hh_income_proxy`
- `migration_exposure`
- `multigenerational_hh`

Inclusion rule:
- non-missing share at least 20%
- at least 2 distinct non-missing values

## Outputs for transparency
- `outputs/tables/module_b_covariate_coverage.csv`
- `outputs/tables/module_b_selected_covariates.csv`
- `outputs/tables/module_b_formulae.csv`
- `outputs/tables/module_b_model_coefficients.csv`

## Notes
- The model code now auto-selects covariates based on observed data quality.
- This prevents fake precision from plugging all-missing variables with constants.
