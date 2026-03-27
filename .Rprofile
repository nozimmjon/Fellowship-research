local_lib <- file.path(getwd(), "r_libs")
if (!dir.exists(local_lib)) {
  dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(normalizePath(local_lib, winslash = "/", mustWork = FALSE), .libPaths()))

options(repos = c(CRAN = "https://cloud.r-project.org"))
