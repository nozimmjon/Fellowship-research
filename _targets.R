source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")
source("R/10_data_inventory.R")
source("R/12_analysis_specs.R")
source("R/20_ingest_data.R")
source("R/30_module_a_mobility.R")
source("R/31_module_a_tier_a_descriptive.R")
source("R/31_build_hbs_household_context.R")
source("R/32_build_hbs_linkage_diagnostics.R")
source("R/33_estimate_hbs_appendix_models.R")
source("R/40_module_b_determinants.R")
source("R/50_module_c_mechanisms.R")
source("R/60_empirical_audit.R")
source("R/90_reporting_helpers.R")

targets::tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "readr",
    "purrr",
    "stringr",
    "janitor",
    "broom"
  ),
  format = "rds"
)

list(
  targets::tar_target(
    data_inventory_csv,
    write_data_inventory_template(),
    format = "file"
  ),
  targets::tar_target(
    variable_dictionary_csv,
    write_variable_dictionary_template(),
    format = "file"
  ),
  targets::tar_target(
    mobility_measure_spec_csv,
    write_mobility_measure_spec(),
    format = "file"
  ),
  targets::tar_target(
    mobility_variable_lock_csv,
    write_mobility_variable_lock(),
    format = "file"
  ),
  targets::tar_target(
    lits_harmonized,
    build_lits_harmonized()
  ),
  targets::tar_target(
    lits_harmonized_csv,
    write_lits_harmonized(lits_harmonized),
    format = "file"
  ),
  targets::tar_target(
    module_a_metrics,
    estimate_mobility_metrics(lits_harmonized)
  ),
  targets::tar_target(
    module_a_files,
    save_module_a_outputs(module_a_metrics),
    format = "file"
  ),
  targets::tar_target(
    module_a_tier_a,
    build_tier_a_descriptive(module_a_metrics, lits_harmonized)
  ),
  targets::tar_target(
    module_a_tier_a_files,
    save_module_a_tier_a_outputs(module_a_tier_a),
    format = "file"
  ),
  targets::tar_target(
    module_b_models,
    fit_module_b_models(lits_harmonized)
  ),
  targets::tar_target(
    module_b_files,
    save_module_b_outputs(module_b_models),
    format = "file"
  ),
  targets::tar_target(
    policy_panel,
    build_policy_panel()
  ),
  targets::tar_target(
    module_c_model,
    fit_module_c_mechanisms()
  ),
  targets::tar_target(
    module_c_files,
    save_module_c_outputs(module_c_model),
    format = "file"
  ),
  targets::tar_target(
    empirical_audit,
    build_empirical_audit(lits_harmonized, module_a_metrics, module_b_models, module_c_model)
  ),
  targets::tar_target(
    empirical_audit_files,
    save_empirical_audit_outputs(empirical_audit),
    format = "file"
  ),
  targets::tar_target(
    hbs_household_context,
    build_hbs_household_context()
  ),
  targets::tar_target(
    hbs_household_context_files,
    write_hbs_household_context_outputs(hbs_household_context),
    format = "file"
  ),
  targets::tar_target(
    hbs_linkage_results,
    build_hbs_linkage_diagnostics()
  ),
  targets::tar_target(
    hbs_linkage_files,
    write_hbs_linkage_outputs(hbs_linkage_results),
    format = "file"
  ),
  targets::tar_target(
    hbs_appendix_model,
    build_hbs_appendix_model(hbs_linkage_results)
  ),
  targets::tar_target(
    hbs_appendix_model_files,
    write_hbs_appendix_model_outputs(hbs_appendix_model),
    format = "file"
  )
)
