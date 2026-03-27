source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")

check_required_packages()
ensure_project_dirs()

message("Running targets pipeline...")
targets::tar_make(callr_function = NULL)

resolve_quarto <- function() {
  q <- Sys.which("quarto")
  if (!nzchar(q)) {
    candidates <- c(
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe",
      "C:/Program Files/Quarto/bin/quarto.exe"
    )
    hits <- candidates[file.exists(candidates)]
    if (length(hits) > 0) {
      q <- hits[[1]]
    }
  }
  if (!nzchar(q)) {
    stop("Quarto executable not found. Install Quarto or add it to PATH.")
  }
  q
}

render_report <- function(quarto_bin, input_file) {
  message("Rendering: ", input_file)
  out <- system2(
    quarto_bin,
    args = c("render", input_file),
    stdout = TRUE,
    stderr = TRUE
  )
  cat(paste(out, collapse = "\n"), "\n")
  invisible(TRUE)
}

quarto_bin <- resolve_quarto()
reports_to_render <- c(
  "reports/00_main.qmd",
  "reports/10_technical_appendix.qmd",
  "reports/20_policy_brief.qmd",
  "reports/30_slides.qmd"
)

invisible(lapply(reports_to_render, function(f) render_report(quarto_bin, f)))

message("Replication completed.")
