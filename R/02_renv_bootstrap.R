bootstrap_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }

  if (!file.exists(file.path(PROJECT_ROOT, "renv.lock"))) {
    renv::init(bare = TRUE)
  } else {
    renv::restore(prompt = FALSE)
  }

  invisible(TRUE)
}
