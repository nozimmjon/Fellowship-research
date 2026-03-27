# Audit and Crosswalk Exports

This folder provides GitHub-readable exports of:
- `data/metadata/02_data_audit.xlsx`
- `data/metadata/03_variable_crosswalk.xlsx`

## Key Share Files
- `share_key_variable_map.csv`: construct-by-wave variable map with example variable names.
- `share_comparability_matrix.csv`: comparability status by construct across 2010, 2016, 2022-23.
- `share_audit_status_summary.csv`: status checks summary by source and field.

## Full Sheet Exports
- `02_data_audit__admin_checks.csv`
- `02_data_audit__admin_file_list.csv`
- `02_data_audit__hbs_checks.csv`
- `02_data_audit__hbs_edu_consistency.csv`
- `02_data_audit__hbs_key_modules.csv`
- `02_data_audit__hbs_modules_by_year.csv`
- `02_data_audit__inventory_files.csv`
- `02_data_audit__inventory_text.csv`
- `02_data_audit__lits_checks.csv`
- `02_data_audit__lits_expected_waves.csv`
- `02_data_audit__lits_file_inventory.csv`
- `02_data_audit__status_counts.csv`
- `02_data_audit__summary_checks.csv`
- `03_variable_crosswalk__admin_construct_checks.csv`
- `03_variable_crosswalk__hbs_construct_checks.csv`
- `03_variable_crosswalk__lits_construct_crosswalk.csv`
- `03_variable_crosswalk__lits_construct_long.csv`
- `03_variable_crosswalk__lits_wave_overlap.csv`
- `share_audit_status_summary.csv`
- `share_comparability_matrix.csv`
- `share_key_variable_map.csv`

## Regeneration
Run: `source("R/14_export_audit_share_files.R"); export_audit_share_files()`
