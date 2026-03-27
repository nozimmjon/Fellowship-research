required_packages <- c(
  "targets",
  "tarchetypes",
  "dplyr",
  "tidyr",
  "readr",
  "purrr",
  "stringr",
  "janitor",
  "haven",
  "readxl",
  "fixest",
  "broom",
  "modelsummary",
  "gt",
  "lubridate"
)

missing_packages <- function(pkgs = required_packages) {
  pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
}

check_required_packages <- function(pkgs = required_packages) {
  missing <- missing_packages(pkgs)
  if (length(missing) > 0) {
    stop(
      paste0(
        "Missing packages: ",
        paste(missing, collapse = ", "),
        ". Run install_missing_packages() first."
      )
    )
  }
  invisible(TRUE)
}

install_missing_packages <- function(pkgs = required_packages) {
  missing <- missing_packages(pkgs)
  if (length(missing) == 0) {
    message("All required packages are already installed.")
    return(invisible(character(0)))
  }
  install.packages(missing)
  invisible(missing)
}

load_required_packages <- function(pkgs = required_packages) {
  check_required_packages(pkgs)
  invisible(lapply(pkgs, library, character.only = TRUE))
}
