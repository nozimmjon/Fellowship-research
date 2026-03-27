sanitize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

export_workbook_sheets <- function(xlsx_path, out_dir, workbook_tag) {
  sheets <- readxl::excel_sheets(xlsx_path)
  out_files <- character(0)

  for (sheet in sheets) {
    dat <- readxl::read_excel(xlsx_path, sheet = sheet)
    out_file <- file.path(
      out_dir,
      paste0(workbook_tag, "__", sanitize_name(sheet), ".csv")
    )
    readr::write_csv(dat, out_file)
    out_files <- c(out_files, out_file)
  }

  out_files
}

build_share_bundle <- function(out_dir) {
  crosswalk_long <- readr::read_csv(
    file.path(out_dir, "03_variable_crosswalk__lits_construct_long.csv"),
    show_col_types = FALSE
  )
  crosswalk_matrix <- readr::read_csv(
    file.path(out_dir, "03_variable_crosswalk__lits_construct_crosswalk.csv"),
    show_col_types = FALSE
  )
  summary_checks <- readr::read_csv(
    file.path(out_dir, "02_data_audit__summary_checks.csv"),
    show_col_types = FALSE
  )

  variable_map <- crosswalk_long |>
    dplyr::transmute(
      construct = field,
      source_dataset = "LiTS",
      wave = wave,
      search_pattern = pattern,
      variable_present = present,
      example_variables = examples
    ) |>
    dplyr::arrange(construct, wave)

  comparability <- crosswalk_matrix |>
    dplyr::transmute(
      construct = field,
      var_2010 = `2010`,
      var_2016 = `2016`,
      var_2022_23 = `2022-23`,
      present_2010 = `2010_present`,
      present_2016 = `2016_present`,
      present_2022_23 = `2022-23_present`,
      comparability_status = status,
      comparability_notes = notes
    ) |>
    dplyr::arrange(construct)

  audit_status <- summary_checks |>
    dplyr::arrange(source, field)

  readr::write_csv(variable_map, file.path(out_dir, "share_key_variable_map.csv"))
  readr::write_csv(comparability, file.path(out_dir, "share_comparability_matrix.csv"))
  readr::write_csv(audit_status, file.path(out_dir, "share_audit_status_summary.csv"))

  c(
    file.path(out_dir, "share_key_variable_map.csv"),
    file.path(out_dir, "share_comparability_matrix.csv"),
    file.path(out_dir, "share_audit_status_summary.csv")
  )
}

write_share_index <- function(out_dir) {
  files <- list.files(out_dir, pattern = "\\.csv$", full.names = FALSE)
  key_files <- c(
    "share_key_variable_map.csv",
    "share_comparability_matrix.csv",
    "share_audit_status_summary.csv"
  )

  index_path <- file.path(out_dir, "README_audit_exports.md")
  lines <- c(
    "# Audit and Crosswalk Exports",
    "",
    "This folder provides GitHub-readable exports of:",
    "- `data/metadata/02_data_audit.xlsx`",
    "- `data/metadata/03_variable_crosswalk.xlsx`",
    "",
    "## Key Share Files",
    "- `share_key_variable_map.csv`: construct-by-wave variable map with example variable names.",
    "- `share_comparability_matrix.csv`: comparability status by construct across 2010, 2016, 2022-23.",
    "- `share_audit_status_summary.csv`: status checks summary by source and field.",
    "",
    "## Full Sheet Exports",
    paste0("- `", sort(files), "`"),
    "",
    "## Regeneration",
    "Run: `source(\"R/14_export_audit_share_files.R\"); export_audit_share_files()`"
  )

  writeLines(lines, con = index_path, useBytes = TRUE)
  index_path
}

export_audit_share_files <- function(
  audit_xlsx = file.path(PROJ_PATHS$metadata, "02_data_audit.xlsx"),
  crosswalk_xlsx = file.path(PROJ_PATHS$metadata, "03_variable_crosswalk.xlsx"),
  out_dir = file.path(PROJ_PATHS$metadata, "exports")
) {
  ensure_project_dirs()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  stopifnot(file.exists(audit_xlsx))
  stopifnot(file.exists(crosswalk_xlsx))

  out_files <- c(
    export_workbook_sheets(audit_xlsx, out_dir, "02_data_audit"),
    export_workbook_sheets(crosswalk_xlsx, out_dir, "03_variable_crosswalk"),
    build_share_bundle(out_dir),
    write_share_index(out_dir)
  )

  invisible(out_files)
}
