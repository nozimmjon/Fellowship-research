read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Required file is missing: %s", path))
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

fmt_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

fmt_pct <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f percent"), 100 * as.numeric(x))
}

fmt_p <- function(x) {
  x <- as.numeric(x)
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x < 0.001) {
    return("<0.001")
  }
  sprintf("%.3f", x)
}

load_main_manuscript_context <- function(project_root = "..", env = parent.frame()) {
  project_file <- function(...) {
    file.path(project_root, ...)
  }

  module_a_summary <- read_csv_safe(project_file("outputs", "tables", "module_a_summary_metrics.csv"))
  module_b_coef <- read_csv_safe(project_file("outputs", "tables", "module_b_model_coefficients.csv"))
  module_c_sample <- read_csv_safe(project_file("outputs", "tables", "module_c_mechanism_sample.csv"))
  module_c_summary <- read_csv_safe(project_file("outputs", "tables", "module_c_mechanism_summary.csv"))
  module_c_coef <- read_csv_safe(project_file("outputs", "tables", "module_c_mechanism_coefficients.csv"))

  metric_row <- function(metric, wave_year) {
    out <- module_a_summary[
      module_a_summary$subgroup_type == "overall" &
        module_a_summary$subgroup_value == "all" &
        module_a_summary$metric == metric &
        module_a_summary$wave_year == wave_year,
    ]
    if (nrow(out) == 0) {
      stop(sprintf("Metric not found: %s / %s", metric, wave_year))
    }
    out[1, ]
  }

  metric_est <- function(metric, wave_year) {
    as.numeric(metric_row(metric, wave_year)$estimate)
  }

  metric_n <- function(metric, wave_year) {
    as.integer(metric_row(metric, wave_year)$n)
  }

  metric_ci_low <- function(metric, wave_year) {
    as.numeric(metric_row(metric, wave_year)$ci_low)
  }

  metric_ci_high <- function(metric, wave_year) {
    as.numeric(metric_row(metric, wave_year)$ci_high)
  }

  metric_ci <- function(metric, wave_year, digits = 3) {
    low <- metric_ci_low(metric, wave_year)
    high <- metric_ci_high(metric, wave_year)
    if (is.na(low) || is.na(high)) {
      return(NA_character_)
    }
    paste0("[", fmt_num(low, digits = digits), ", ", fmt_num(high, digits = digits), "]")
  }

  metric_effective_n <- function(metric, wave_year) {
    as.numeric(metric_row(metric, wave_year)$effective_n)
  }

  coef_row <- function(model, term) {
    out <- module_b_coef[module_b_coef$model == model & module_b_coef$term == term, ]
    if (nrow(out) == 0) {
      stop(sprintf("Coefficient not found: %s / %s", model, term))
    }
    out[1, ]
  }

  coef_est <- function(model, term) {
    as.numeric(coef_row(model, term)$estimate)
  }

  coef_p <- function(model, term) {
    as.numeric(coef_row(model, term)$p.value)
  }

  mech_summary_row <- function(outcome, group = "overall", group_value = "all") {
    out <- module_c_summary[
      module_c_summary$outcome == outcome &
        module_c_summary$group == group &
        module_c_summary$group_value == group_value,
    ]
    if (nrow(out) == 0) {
      stop(sprintf("Mechanism summary not found: %s / %s / %s", outcome, group, group_value))
    }
    out[1, ]
  }

  mech_est <- function(outcome, group = "overall", group_value = "all") {
    as.numeric(mech_summary_row(outcome, group, group_value)$estimate)
  }

  mech_coef_row <- function(model, term) {
    out <- module_c_coef[module_c_coef$model == model & module_c_coef$term == term, ]
    if (nrow(out) == 0) {
      stop(sprintf("Mechanism coefficient not found: %s / %s", model, term))
    }
    out[1, ]
  }

  mech_coef_est <- function(model, term) {
    as.numeric(mech_coef_row(model, term)$estimate)
  }

  mech_coef_p <- function(model, term) {
    as.numeric(mech_coef_row(model, term)$p.value)
  }

  sample_n <- function(step_pattern) {
    idx <- grep(step_pattern, module_c_sample$step)
    if (length(idx) == 0) {
      stop(sprintf("Mechanism sample step not found: %s", step_pattern))
    }
    as.integer(module_c_sample$n[idx[1]])
  }

  list2env(
    list(
      coef_est = coef_est,
      coef_p = coef_p,
      coef_row = coef_row,
      fmt_num = fmt_num,
      fmt_p = fmt_p,
      fmt_pct = fmt_pct,
      mech_coef_est = mech_coef_est,
      mech_coef_p = mech_coef_p,
      mech_coef_row = mech_coef_row,
      mech_est = mech_est,
      mech_summary_row = mech_summary_row,
      metric_est = metric_est,
      metric_ci = metric_ci,
      metric_ci_low = metric_ci_low,
      metric_ci_high = metric_ci_high,
      metric_effective_n = metric_effective_n,
      metric_n = metric_n,
      metric_row = metric_row,
      module_a_summary = module_a_summary,
      module_b_coef = module_b_coef,
      module_c_coef = module_c_coef,
      module_c_sample = module_c_sample,
      module_c_summary = module_c_summary,
      project_file = project_file,
      sample_n = sample_n
    ),
    envir = env
  )

  invisible(TRUE)
}
