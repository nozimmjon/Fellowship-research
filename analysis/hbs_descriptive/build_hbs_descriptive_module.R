suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(tidyr)
  library(janitor)
})

source(file.path("R", "00_config.R"))
activate_local_lib()
source(file.path("R", "20_ingest_data.R"))
source(file.path("R", "31_build_hbs_household_context.R"))
source(file.path("R", "90_reporting_helpers.R"))

MODULE_PATHS <- list(
  analysis = file.path(PROJECT_ROOT, "analysis", "hbs_descriptive"),
  outputs = file.path(PROJECT_ROOT, "outputs", "hbs_descriptive"),
  tables = file.path(PROJECT_ROOT, "outputs", "hbs_descriptive", "tables"),
  figures = file.path(PROJECT_ROOT, "outputs", "hbs_descriptive", "figures"),
  report = file.path(PROJECT_ROOT, "reports", "41_hbs_descriptive_note.qmd"),
  progress = file.path(PROJECT_ROOT, "HBS_DESCRIPTIVE_PROGRESS.md"),
  readme = file.path(PROJECT_ROOT, "outputs", "hbs_descriptive", "README.md")
)

HBS_YOUNG_COHORTS <- list(
  `18_24` = c(18L, 24L),
  `25_30` = c(25L, 30L),
  `18_30_combined` = c(18L, 30L)
)

HBS_REGION_PLOT_METRICS <- c("education_spending_positive", "internet_access_hh")

metric_specs <- tibble::tribble(
  ~metric, ~row_label, ~var, ~denominator_var, ~denominator_label,
  "has_enrolled_member", "Households with enrolled members", "has_enrolled_member", NA_character_, "all households",
  "education_spending_positive", "Households with positive education spending", "education_spending_positive", NA_character_, "all households",
  "has_tutoring", "Households with tutoring", "has_tutoring", NA_character_, "all households",
  "has_emigrant_hh", "Households with emigrant member", "has_emigrant_hh", NA_character_, "all households",
  "has_remittance_hh", "Households receiving remittances", "has_remittance_hh", NA_character_, "all households",
  "remittance_for_education", "Remittance households using remittances for education", "remittance_for_education", "has_remittance_hh", "remittance households",
  "internet_access_hh", "Households with internet access", "internet_access_hh", NA_character_, "all households"
)

cohort_specs <- tibble::tribble(
  ~metric, ~row_label, ~var, ~denominator_label,
  "currently_enrolled", "Individuals currently enrolled", "currently_enrolled", "all people in age group",
  "tertiary_enrolled_proxy", "Individuals with tertiary enrollment proxy", "tertiary_enrolled_proxy", "all people in age group",
  "tertiary_completed_proxy", "Individuals with tertiary completion proxy", "tertiary_completed_proxy", "all people in age group",
  "household_enrolled_member", "Lives in household with an enrolled member", "has_enrolled_member", "all people in age group",
  "household_education_spending", "Lives in household with positive education spending", "education_spending_positive", "all people in age group",
  "household_tutoring", "Lives in household with tutoring", "has_tutoring", "all people in age group",
  "household_remittance", "Lives in remittance-receiving household", "has_remittance_hh", "all people in age group",
  "household_internet", "Lives in household with internet access", "internet_access_hh", "all people in age group",
  "urban_share", "Urban", "urban", "all people in age group",
  "rural_share", "Rural", "urban", "all people in age group"
)

ensure_module_dirs <- function() {
  for (path in unlist(MODULE_PATHS[c("analysis", "outputs", "tables", "figures")], use.names = FALSE)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

write_md_lines <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
  path
}

progress_state <- new.env(parent = emptyenv())
progress_state$entries <- list()

write_progress_log <- function() {
  lines <- c(
    "# HBS Descriptive Progress",
    "",
    "This log tracks the supplementary HBS descriptive companion layer built on top of the frozen LiTS paper.",
    ""
  )

  if (length(progress_state$entries) == 0) {
    lines <- c(lines, "No stages recorded yet.")
  } else {
    for (idx in seq_along(progress_state$entries)) {
      entry <- progress_state$entries[[idx]]
      lines <- c(
        lines,
        paste0("## Stage ", idx, ": ", entry$stage),
        "",
        "**Completed stage**",
        "",
        paste0("- ", entry$stage),
        "",
        "**Key findings**",
        "",
        paste0("- ", entry$key_findings),
        "",
        "**Blockers**",
        "",
        paste0("- ", entry$blockers),
        "",
        "**Next action**",
        "",
        paste0("- ", entry$next_action),
        ""
      )
    }
  }

  write_md_lines(MODULE_PATHS$progress, lines)
}

record_progress <- function(stage, key_findings, blockers = "None at this stage.", next_action) {
  progress_state$entries[[length(progress_state$entries) + 1L]] <- list(
    stage = stage,
    key_findings = key_findings,
    blockers = blockers,
    next_action = next_action
  )
  write_progress_log()
}

weighted_share_safe <- function(x, w, mask = NULL) {
  x <- suppressWarnings(as.numeric(x))
  w <- suppressWarnings(as.numeric(w))
  if (is.null(mask)) {
    mask <- rep(TRUE, length(x))
  }
  keep <- mask & !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep])
}

value_available_years <- function(df, var, denominator_var = NA_character_) {
  if (!var %in% names(df)) {
    return(NA_character_)
  }
  keep <- !is.na(df[[var]])
  if (!is.na(denominator_var) && denominator_var %in% names(df)) {
    keep <- keep & !is.na(df[[denominator_var]])
  }
  years <- sort(unique(df$year[keep]))
  if (length(years) == 0) {
    return(NA_character_)
  }
  paste(years, collapse = ", ")
}

admin_region_label <- function(x) {
  region_std <- harmonize_uzbekistan_region(x)
  case_when(
    is.na(region_std) ~ NA_character_,
    region_std == "Andijan region" ~ "Andijan region",
    region_std == "Bukhara region" ~ "Bukhara region",
    region_std == "Fergana region" ~ "Fergana region",
    region_std == "Jizzakh region" ~ "Jizzakh region",
    region_std == "Karakalpakstan republic" ~ "Republic of Karakalpakstan",
    region_std == "Kashkadarya region" ~ "Kashkadarya region",
    region_std == "Khorezm region" ~ "Khorezm region",
    region_std == "Namangan region" ~ "Namangan region",
    region_std == "Navoi region" ~ "Navoi region",
    region_std == "Samarkand region" ~ "Samarkand region",
    region_std == "Sirdarya region" ~ "Syrdarya region",
    region_std == "Surkhandarya region" ~ "Surkhandarya region",
    region_std == "Tashkent city" ~ "Tashkent city",
    region_std == "Tashkent region" ~ "Tashkent region",
    TRUE ~ trimws(as.character(x))
  )
}

hbs_region_to_admin <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x == "Andijan" ~ "Andijan region",
    x == "Bukhara" ~ "Bukhara region",
    x == "Fergana" ~ "Fergana region",
    x == "Jizzakh" ~ "Jizzakh region",
    x == "Karakalpakstan" ~ "Republic of Karakalpakstan",
    x == "Kashkadarya" ~ "Kashkadarya region",
    x == "Khorezm" ~ "Khorezm region",
    x == "Namangan" ~ "Namangan region",
    x == "Navoi" ~ "Navoi region",
    x == "Samarkand" ~ "Samarkand region",
    x == "Syrdarya" ~ "Syrdarya region",
    x == "Surkhandarya" ~ "Surkhandarya region",
    x == "Tashkent City" ~ "Tashkent city",
    x == "Tashkent Region" ~ "Tashkent region",
    TRUE ~ admin_region_label(x)
  )
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

read_hbs_household_context_input <- function() {
  context_path <- file.path(PROJ_PATHS$processed_data, "hbs_household_context.csv")
  if (file.exists(context_path)) {
    return(read_csv_safe(context_path))
  }
  build_hbs_household_context()$household_context
}

refresh_education_household_flags <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  folders <- hbs_year_folders(raw_dir)
  if (length(folders) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(folders, function(folder) {
    path <- hbs_find_module_path(folder, "m03_education\\.dta$")
    if (is.na(path)) {
      return(tibble::tibble())
    }

    x <- haven::read_dta(path)
    cost_vars <- intersect(
      c(
        "edu_cost_total",
        "edu_cost_tuition",
        "edu_cost_uniform",
        "edu_cost_textbook",
        "edu_cost_supply",
        "edu_cost_meal",
        "edu_cost_equipment",
        "edu_cost_gift",
        "edu_cost_other",
        "edu_cost_inkind",
        "edu_cost_service",
        "edu_tutor_fee",
        "edu_cost_tutor"
      ),
      names(x)
    )

    cost_positive <- rep(NA_integer_, nrow(x))
    if (length(cost_vars) > 0) {
      cost_mat <- as.data.frame(lapply(cost_vars, function(v) hbs_numeric(x[[v]])))
      any_observed <- rowSums(!is.na(as.matrix(cost_mat))) > 0
      any_positive <- rowSums(as.matrix(cost_mat) > 0, na.rm = TRUE) > 0
      cost_positive <- dplyr::if_else(any_observed, as.integer(any_positive), NA_integer_)
    }

    tutor_flag <- dplyr::coalesce(
      hbs_binary(hbs_col_or_na(x, "edu_tutor")),
      dplyr::if_else(
        !is.na(hbs_numeric(hbs_col_or_na(x, "edu_tutor_fee"))),
        as.integer(hbs_numeric(hbs_col_or_na(x, "edu_tutor_fee")) > 0),
        NA_integer_
      ),
      dplyr::if_else(
        !is.na(hbs_numeric(hbs_col_or_na(x, "edu_cost_tutor"))),
        as.integer(hbs_numeric(hbs_col_or_na(x, "edu_cost_tutor")) > 0),
        NA_integer_
      )
    )

    tibble::tibble(
      year = hbs_extract_year(path),
      hhid = as.character(hbs_col_or_na(x, "hhid")),
      has_enrolled_member = hbs_binary(hbs_col_or_na(x, "edu_enrolled")),
      education_spending_positive = cost_positive,
      has_tutoring = tutor_flag
    ) %>%
      dplyr::group_by(year, hhid) %>%
      dplyr::summarise(
        has_enrolled_member = hbs_collapse_binary(has_enrolled_member),
        education_spending_positive = hbs_collapse_binary(education_spending_positive),
        has_tutoring = hbs_collapse_binary(has_tutoring),
        .groups = "drop"
      )
  })
}

enhance_household_context_for_descriptive_module <- function(household_context) {
  household_context <- household_context %>%
    mutate(
      year = as.integer(year),
      hhid = as.character(hhid)
    )

  education_refresh <- refresh_education_household_flags()
  if (nrow(education_refresh) == 0) {
    return(household_context)
  }

  household_context %>%
    left_join(
      education_refresh,
      by = c("year", "hhid"),
      suffix = c("", "_refresh")
    ) %>%
    mutate(
      has_enrolled_member = coalesce(has_enrolled_member_refresh, has_enrolled_member),
      education_spending_positive = coalesce(education_spending_positive_refresh, education_spending_positive),
      has_tutoring = coalesce(has_tutoring_refresh, has_tutoring)
    ) %>%
    select(-ends_with("_refresh"))
}

read_hbs_merged_person_input <- function() {
  parquet_path <- file.path(PROJ_PATHS$processed_data, "hbs_expansion_merged.parquet")
  csv_path <- file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal", "tables", "_tmp_hbs_expansion_merged.csv")

  if (file.exists(parquet_path) && requireNamespace("arrow", quietly = TRUE)) {
    return(arrow::read_parquet(parquet_path) %>% tibble::as_tibble())
  }
  if (file.exists(csv_path)) {
    return(read_csv_safe(csv_path))
  }
  stop("Merged HBS person overlay is unavailable. Expected either the processed parquet or the synced merged CSV.")
}

attach_household_support_overlay <- function(person_df, household_context) {
  support_vars <- c(
    "has_enrolled_member",
    "education_spending_positive",
    "has_tutoring",
    "has_emigrant_hh",
    "has_remittance_hh",
    "remittance_for_education",
    "internet_access_hh",
    "multigenerational_hh"
  )

  household_overlay <- household_context %>%
    transmute(
      survey_year = as.integer(year),
      household_id = as.character(hhid),
      dplyr::across(all_of(support_vars))
    )

  joined <- person_df %>%
    mutate(
      survey_year = as.integer(survey_year),
      household_id = as.character(household_id)
    ) %>%
    left_join(household_overlay, by = c("survey_year", "household_id"), suffix = c("_existing", ""))

  for (var in support_vars) {
    existing_var <- paste0(var, "_existing")
    if (existing_var %in% names(joined)) {
      joined[[var]] <- dplyr::coalesce(joined[[var]], joined[[existing_var]])
      joined[[existing_var]] <- NULL
    }
  }

  joined
}

metric_share_from_df <- function(df, var, denominator_var = NA_character_, weight_var = "household_weight") {
  denom_mask <- rep(TRUE, nrow(df))
  if (!is.na(denominator_var) && denominator_var %in% names(df)) {
    denom_mask <- df[[denominator_var]] == 1L
  }
  weighted_share_safe(df[[var]], df[[weight_var]], denom_mask)
}

build_support_by_year_table <- function(household_context) {
  years <- sort(unique(household_context$year))

  long <- purrr::pmap_dfr(metric_specs, function(metric, row_label, var, denominator_var, denominator_label) {
    purrr::map_dfr(years, function(year_i) {
      df_year <- household_context %>% filter(year == year_i)
      tibble::tibble(
        metric = metric,
        row_label = row_label,
        year = year_i,
        value = metric_share_from_df(df_year, var, denominator_var),
        denominator = denominator_label,
        available_years = value_available_years(household_context, var, denominator_var)
      )
    })
  })

  wide <- long %>%
    select(metric, row_label, year, value) %>%
    mutate(year = as.character(year)) %>%
    tidyr::pivot_wider(names_from = year, values_from = value)

  pooled <- purrr::pmap_dfr(metric_specs, function(metric, row_label, var, denominator_var, denominator_label) {
    tibble::tibble(
      metric = metric,
      row_label = row_label,
      pooled_national = metric_share_from_df(household_context, var, denominator_var),
      denominator = denominator_label,
      available_years = value_available_years(household_context, var, denominator_var)
    )
  })

  list(
    long = long,
    wide = pooled %>%
      left_join(wide, by = c("metric", "row_label")) %>%
      select(
        metric,
        row_label,
        all_of(sort(names(wide)[!(names(wide) %in% c("metric", "row_label"))])),
        pooled_national,
        denominator,
        available_years
      )
  )
}

build_support_urban_rural_table <- function(household_context) {
  purrr::pmap_dfr(metric_specs, function(metric, row_label, var, denominator_var, denominator_label) {
    denom_mask <- rep(TRUE, nrow(household_context))
    if (!is.na(denominator_var) && denominator_var %in% names(household_context)) {
      denom_mask <- household_context[[denominator_var]] == 1L
    }

    tibble::tibble(
      metric = metric,
      row_label = row_label,
      national = weighted_share_safe(household_context[[var]], household_context$household_weight, denom_mask),
      urban = weighted_share_safe(household_context[[var]], household_context$household_weight, denom_mask & household_context$urban == 1L),
      rural = weighted_share_safe(household_context[[var]], household_context$household_weight, denom_mask & household_context$urban == 0L),
      urban_minus_rural = urban - rural,
      denominator = denominator_label,
      available_years = value_available_years(household_context, var, denominator_var)
    )
  })
}

build_support_by_region_table <- function(household_context) {
  purrr::pmap_dfr(metric_specs, function(metric, row_label, var, denominator_var, denominator_label) {
    national_value <- metric_share_from_df(household_context, var, denominator_var)
    household_context %>%
      filter(!is.na(province), province != "") %>%
      group_by(province) %>%
      group_modify(~ tibble::tibble(value = metric_share_from_df(.x, var, denominator_var))) %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      arrange(desc(value), province) %>%
      mutate(
        metric = metric,
        row_label = row_label,
        region = province,
        national = national_value,
        gap_vs_national = value - national_value,
        rank_within_metric = row_number(),
        denominator = denominator_label,
        available_years = value_available_years(household_context, var, denominator_var)
      ) %>%
      select(metric, row_label, region, rank_within_metric, value, national, gap_vs_national, denominator, available_years)
  })
}

build_young_cohort_profile_table <- function(person_df) {
  person_df <- person_df %>%
    mutate(
      age = suppressWarnings(as.numeric(age)),
      sample_weight = suppressWarnings(as.numeric(sample_weight)),
      urban = suppressWarnings(as.numeric(urban))
    )

  purrr::pmap_dfr(cohort_specs, function(metric, row_label, var, denominator_label) {
    values <- purrr::imap_dbl(HBS_YOUNG_COHORTS, function(bounds, cohort_name) {
      df_cohort <- person_df %>% filter(!is.na(age), age >= bounds[[1]], age <= bounds[[2]])
      if (metric == "rural_share") {
        return(weighted_share_safe(as.integer(df_cohort[[var]] == 0L), df_cohort$sample_weight))
      }
      weighted_share_safe(df_cohort[[var]], df_cohort$sample_weight)
    })

    available_years <- purrr::imap_chr(HBS_YOUNG_COHORTS, function(bounds, cohort_name) {
      df_cohort <- person_df %>% filter(!is.na(age), age >= bounds[[1]], age <= bounds[[2]])
      years <- sort(unique(df_cohort$survey_year[!is.na(df_cohort[[var]])]))
      if (length(years) == 0) {
        return(NA_character_)
      }
      paste(years, collapse = ", ")
    })

    tibble::tibble(
      metric = metric,
      row_label = row_label,
      `18_24` = values[["18_24"]],
      `25_30` = values[["25_30"]],
      `18_30_combined` = values[["18_30_combined"]],
      denominator = denominator_label,
      available_years_18_24 = available_years[["18_24"]],
      available_years_25_30 = available_years[["25_30"]],
      available_years_18_30 = available_years[["18_30_combined"]]
    )
  })
}

read_admin_treatment_panel <- function() {
  panel_path <- file.path(PROJ_PATHS$raw_data, "admin", "uzbekistan_expansion_treatment_panel_final.csv")
  readr::read_csv(panel_path, show_col_types = FALSE, progress = FALSE) %>%
    janitor::clean_names() %>%
    transmute(
      admin_region = admin_region_label(geography),
      academic_year_start = as.integer(academic_year_start),
      expansion_index_main = as.numeric(expansion_index_main),
      high_expansion_region = as.integer(high_expansion_region_main)
    )
}

attach_expansion_groups <- function(household_context, admin_panel) {
  min_admin_year <- min(admin_panel$academic_year_start, na.rm = TRUE)
  max_admin_year <- max(admin_panel$academic_year_start, na.rm = TRUE)

  household_context %>%
    mutate(
      admin_region = hbs_region_to_admin(province),
      academic_year_start_for_merge = pmax(min_admin_year, pmin(year - 1L, max_admin_year))
    ) %>%
    left_join(
      admin_panel,
      by = c("admin_region", "academic_year_start_for_merge" = "academic_year_start")
    )
}

build_high_low_expansion_table <- function(household_expansion_df) {
  purrr::pmap_dfr(metric_specs, function(metric, row_label, var, denominator_var, denominator_label) {
    low_value <- metric_share_from_df(
      household_expansion_df %>% filter(high_expansion_region == 0L),
      var,
      denominator_var
    )
    high_value <- metric_share_from_df(
      household_expansion_df %>% filter(high_expansion_region == 1L),
      var,
      denominator_var
    )

    tibble::tibble(
      metric = metric,
      row_label = row_label,
      low_expansion = low_value,
      high_expansion = high_value,
      high_minus_low = high_value - low_value,
      pooled_national = metric_share_from_df(household_expansion_df, var, denominator_var),
      denominator = denominator_label,
      available_years = value_available_years(household_expansion_df, var, denominator_var),
      merge_rule = "Survey year joined to academic year start = survey year - 1; descriptive grouping only"
    )
  })
}

plot_support_trends_by_year <- function(by_year_long, path) {
  safe_png_plot(path, width = 1600, height = 1400, res = 150, expr = {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mfrow = c(4, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

    years_all <- sort(unique(by_year_long$year))
    label_lookup <- setNames(metric_specs$row_label, metric_specs$metric)

    for (metric_i in metric_specs$metric) {
      dat <- by_year_long %>%
        filter(metric == metric_i) %>%
        arrange(year)

      ymax <- max(dat$value, na.rm = TRUE)
      if (!is.finite(ymax)) {
        ymax <- 1
      }
      ymax <- max(0.1, ymax * 1.15)

      plot(
        years_all,
        rep(NA_real_, length(years_all)),
        type = "n",
        ylim = c(0, ymax * 100),
        xlab = "Year",
        ylab = "Percent",
        main = label_lookup[[metric_i]]
      )
      grid()
      if (nrow(dat) > 0) {
        lines(dat$year, 100 * dat$value, type = "b", pch = 19, lwd = 2, col = "#1f4e79")
      } else {
        text(mean(range(years_all)), ymax * 50, "No data available")
      }
    }

    plot.new()
    text(0.5, 0.6, "Household shares are weighted within year.", cex = 1)
    text(0.5, 0.45, "Internet access is limited to years with a working internet module.", cex = 1)
    mtext("HBS Household Support Indicators by Year", outer = TRUE, cex = 1.4, font = 2)
  })
}

plot_support_urban_rural <- function(urban_rural_table, path) {
  dat <- urban_rural_table %>% arrange(desc(national))

  safe_png_plot(path, width = 1400, height = 900, res = 140, expr = {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5, 13, 4, 2))

    y_pos <- rev(seq_len(nrow(dat)))
    all_vals <- 100 * c(dat$national, dat$urban, dat$rural)
    x_lim <- c(0, max(all_vals, na.rm = TRUE) * 1.1)

    plot(
      x_lim,
      c(0.5, nrow(dat) + 0.5),
      type = "n",
      yaxt = "n",
      ylab = "",
      xlab = "Percent of households",
      main = "HBS Support Indicators: National, Urban, and Rural"
    )
    axis(2, at = y_pos, labels = dat$row_label, las = 2)
    grid()
    segments(100 * dat$rural, y_pos, 100 * dat$urban, y_pos, lwd = 2, col = "#c7c7c7")
    points(100 * dat$national, y_pos, pch = 18, cex = 1.4, col = "#2f2f2f")
    points(100 * dat$urban, y_pos, pch = 19, cex = 1.2, col = "#1f77b4")
    points(100 * dat$rural, y_pos, pch = 19, cex = 1.2, col = "#d95f02")
    legend(
      "bottomright",
      legend = c("National", "Urban", "Rural"),
      pch = c(18, 19, 19),
      col = c("#2f2f2f", "#1f77b4", "#d95f02"),
      bty = "n"
    )
  })
}

plot_region_ranked <- function(region_table, path) {
  plot_data <- region_table %>%
    filter(metric %in% HBS_REGION_PLOT_METRICS) %>%
    group_by(metric) %>%
    arrange(desc(value), region, .by_group = TRUE) %>%
    mutate(order_id = rev(seq_len(n()))) %>%
    ungroup()

  safe_png_plot(path, width = 1800, height = 900, res = 150, expr = {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mfrow = c(1, 2), mar = c(5, 11, 4, 2))

    for (metric_i in HBS_REGION_PLOT_METRICS) {
      dat <- plot_data %>% filter(metric == metric_i)
      if (nrow(dat) == 0) {
        plot.new()
        title(metric_i)
        text(0.5, 0.5, "No data available")
        next
      }

      x_vals <- 100 * dat$value
      national_val <- 100 * dat$national[[1]]
      plot(
        x = x_vals,
        y = dat$order_id,
        xlim = c(0, max(x_vals, national_val, na.rm = TRUE) * 1.1),
        ylim = c(0.5, max(dat$order_id) + 0.5),
        yaxt = "n",
        ylab = "",
        xlab = "Percent of households",
        pch = 19,
        col = "#1f4e79",
        main = dat$row_label[[1]]
      )
      axis(2, at = dat$order_id, labels = dat$region, las = 2, cex.axis = 0.9)
      abline(v = national_val, lty = 2, lwd = 2, col = "#a11d33")
      grid()
      legend(
        "bottomright",
        legend = c("Region", "National reference"),
        pch = c(19, NA),
        lty = c(NA, 2),
        col = c("#1f4e79", "#a11d33"),
        bty = "n"
      )
    }
  })
}

plot_high_low_expansion <- function(expansion_table, path) {
  dat <- expansion_table %>% arrange(desc(pooled_national))

  safe_png_plot(path, width = 1400, height = 900, res = 140, expr = {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    par(mar = c(5, 13, 4, 2))

    y_pos <- rev(seq_len(nrow(dat)))
    all_vals <- 100 * c(dat$high_expansion, dat$low_expansion)
    x_lim <- c(0, max(all_vals, na.rm = TRUE) * 1.1)

    plot(
      x_lim,
      c(0.5, nrow(dat) + 0.5),
      type = "n",
      yaxt = "n",
      ylab = "",
      xlab = "Percent of households",
      main = "HBS Support Indicators by High- and Low-Expansion Regions"
    )
    axis(2, at = y_pos, labels = dat$row_label, las = 2)
    grid()
    segments(100 * dat$low_expansion, y_pos, 100 * dat$high_expansion, y_pos, lwd = 2, col = "#c7c7c7")
    points(100 * dat$low_expansion, y_pos, pch = 19, cex = 1.2, col = "#6c757d")
    points(100 * dat$high_expansion, y_pos, pch = 19, cex = 1.2, col = "#1b9e77")
    legend(
      "bottomright",
      legend = c("Low expansion", "High expansion"),
      pch = 19,
      col = c("#6c757d", "#1b9e77"),
      bty = "n"
    )
    mtext("Descriptive grouping only; no causal interpretation.", side = 3, line = 0.5, cex = 0.9)
  })
}

write_module_readme <- function() {
  lines <- c(
    "# HBS Descriptive Companion Outputs",
    "",
    "This directory contains the supplementary HBS descriptive layer for the Uzbekistan LiTS paper.",
    "",
    "Scope:",
    "- HBS is used only for descriptive context on household support conditions.",
    "- These outputs do not introduce headline HBS intergenerational mobility estimates.",
    "- All comparisons are descriptive and non-causal.",
    "",
    "Core inputs reused:",
    "- `data/processed/hbs_household_context.csv`",
    "- `data/processed/hbs_expansion_merged.parquet` when available, otherwise the synced merged CSV from the existing HBS expansion module",
    "- `data/raw/admin/uzbekistan_expansion_treatment_panel_final.csv`",
    "",
    "Core tables:",
    "- `tables/hbs_support_by_year.csv`",
    "- `tables/hbs_support_urban_rural.csv`",
    "- `tables/hbs_support_by_region.csv`",
    "- `tables/hbs_young_cohort_profile.csv`",
    "- `tables/hbs_high_low_expansion_comparison.csv`",
    "",
    "Core figures:",
    "- `figures/hbs_support_trends_by_year.png`",
    "- `figures/hbs_support_urban_rural.png`",
    "- `figures/hbs_region_ranked_plot.png`",
    "- `figures/hbs_high_low_expansion_plot.png`",
    "",
    "Interpretation rules:",
    "- LiTS remains the source of the paper's mobility estimates.",
    "- HBS region and expansion-group overlays are descriptive grouping devices only.",
    "- Internet-access comparisons use only years with a working internet module in the current processed layer."
  )
  write_md_lines(MODULE_PATHS$readme, lines)
}

main <- function() {
  ensure_module_dirs()
  write_progress_log()

  household_context <- read_hbs_household_context_input()
  household_context <- enhance_household_context_for_descriptive_module(household_context)
  person_df <- read_hbs_merged_person_input()
  person_df <- attach_household_support_overlay(person_df, household_context)
  admin_panel <- read_admin_treatment_panel()

  record_progress(
    stage = "Workspace setup and input reuse",
    key_findings = paste(
      "Reused the existing processed HBS household context and the established HBS expansion overlay for descriptive cohort and region groupings.",
      "No new HBS mobility engine was introduced."
    ),
    next_action = "Build the descriptive HBS tables."
  )

  support_by_year <- build_support_by_year_table(household_context)
  support_urban_rural <- build_support_urban_rural_table(household_context)
  support_by_region <- build_support_by_region_table(household_context)
  young_cohort_profile <- build_young_cohort_profile_table(person_df)
  household_expansion_df <- attach_expansion_groups(household_context, admin_panel)
  high_low_expansion <- build_high_low_expansion_table(household_expansion_df)

  safe_write_csv(support_by_year$wide, file.path(MODULE_PATHS$tables, "hbs_support_by_year.csv"))
  safe_write_csv(support_urban_rural, file.path(MODULE_PATHS$tables, "hbs_support_urban_rural.csv"))
  safe_write_csv(support_by_region, file.path(MODULE_PATHS$tables, "hbs_support_by_region.csv"))
  safe_write_csv(young_cohort_profile, file.path(MODULE_PATHS$tables, "hbs_young_cohort_profile.csv"))
  safe_write_csv(high_low_expansion, file.path(MODULE_PATHS$tables, "hbs_high_low_expansion_comparison.csv"))

  record_progress(
    stage = "Descriptive table construction",
    key_findings = paste(
      "Built year, urban-rural, region, younger-cohort, and high-versus-low expansion descriptive tables.",
      "Internet access remains year-limited relative to the other support indicators, while remittance-for-education keeps a remittance-household denominator."
    ),
    next_action = "Generate publication-ready figures from the new tables."
  )

  plot_support_trends_by_year(support_by_year$long, file.path(MODULE_PATHS$figures, "hbs_support_trends_by_year.png"))
  plot_support_urban_rural(support_urban_rural, file.path(MODULE_PATHS$figures, "hbs_support_urban_rural.png"))
  plot_region_ranked(support_by_region, file.path(MODULE_PATHS$figures, "hbs_region_ranked_plot.png"))
  plot_high_low_expansion(high_low_expansion, file.path(MODULE_PATHS$figures, "hbs_high_low_expansion_plot.png"))

  record_progress(
    stage = "Figure production",
    key_findings = paste(
      "Generated clean descriptive visuals for time trends, urban-rural contrasts, regional ranking, and high-versus-low expansion grouping.",
      "The expansion comparison is explicitly labeled as descriptive and non-causal."
    ),
    next_action = "Refresh the module README and render the companion note."
  )

  write_module_readme()

  record_progress(
    stage = "Companion note inputs ready",
    key_findings = paste(
      "The HBS descriptive output set is complete and ready for the standalone companion note.",
      "The LiTS main paper remains the source of all headline mobility evidence."
    ),
    next_action = "Render reports/41_hbs_descriptive_note.qmd."
  )
}

main()
