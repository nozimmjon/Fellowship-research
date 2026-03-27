options(stringsAsFactors = FALSE)

find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    markers <- c("_targets.R", "FellowshipResearch.Rproj")
    if (all(file.exists(file.path(path, markers)))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Project root not found. Open FellowshipResearch.Rproj or run from project directory.")
    }
    path <- parent
  }
}

PROJECT_ROOT <- find_project_root()

PROJ_PATHS <- list(
  raw_data = file.path(PROJECT_ROOT, "data", "raw"),
  processed_data = file.path(PROJECT_ROOT, "data", "processed"),
  metadata = file.path(PROJECT_ROOT, "data", "metadata"),
  outputs = file.path(PROJECT_ROOT, "outputs"),
  tables = file.path(PROJECT_ROOT, "outputs", "tables"),
  figures = file.path(PROJECT_ROOT, "outputs", "figures"),
  models = file.path(PROJECT_ROOT, "outputs", "models"),
  reports = file.path(PROJECT_ROOT, "reports"),
  scripts = file.path(PROJECT_ROOT, "R"),
  r_libs = file.path(PROJECT_ROOT, "r_libs")
)

ANALYSIS_SAMPLE <- list(
  age_min = 25L,
  age_max = 64L
)

EDUCATION_LEVELS <- c(
  "no_formal",
  "primary",
  "lower_secondary",
  "upper_secondary",
  "post_secondary_non_tertiary",
  "tertiary"
)

UZB_REGIONS <- c(
  "Andijan",
  "Bukhara",
  "Fergana",
  "Jizzakh",
  "Karakalpakstan",
  "Khorezm",
  "Namangan",
  "Navoiy",
  "Qashqadaryo",
  "Samarkand",
  "Sirdaryo",
  "Surkhandarya",
  "Tashkent",
  "Tashkent City"
)

ensure_project_dirs <- function() {
  dirs <- unlist(PROJ_PATHS, use.names = FALSE)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(TRUE)
}

activate_local_lib <- function() {
  lib_path <- PROJ_PATHS$r_libs
  if (!dir.exists(lib_path)) {
    dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
  }
  .libPaths(c(lib_path, .libPaths()))
  invisible(.libPaths())
}
