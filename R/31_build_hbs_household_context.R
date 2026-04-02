hbs_year_folders <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  if (!dir.exists(raw_dir)) {
    return(character())
  }
  folders <- list.dirs(raw_dir, recursive = FALSE, full.names = TRUE)
  folders[order(basename(folders))]
}

hbs_extract_year <- function(path) {
  out <- stringr::str_match(basename(path), "uzb_hbs_(\\d{4})_")[, 2]
  suppressWarnings(as.integer(out))
}

hbs_find_module_path <- function(folder, pattern) {
  hits <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

hbs_col_or_na <- function(df, col) {
  if (!col %in% names(df)) {
    return(rep(NA, nrow(df)))
  }
  df[[col]]
}

hbs_numeric <- function(x) {
  raw_chr <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(raw_chr))
  num[num < 0] <- NA_real_
  num
}

hbs_text <- function(x) {
  trimws(tolower(as_label_text(x)))
}

hbs_normalize_province <- function(x) {
  txt <- hbs_text(x)
  dplyr::case_when(
    stringr::str_detect(txt, "andij|андиж") ~ "Andijan",
    stringr::str_detect(txt, "bux|bukh|бух") ~ "Bukhara",
    stringr::str_detect(txt, "farg|ferg|фарғ|ферг") ~ "Fergana",
    stringr::str_detect(txt, "jizz|jiz|жиз") ~ "Jizzakh",
    stringr::str_detect(txt, "qoraqal|karakal|қорақал") ~ "Karakalpakstan",
    stringr::str_detect(txt, "xoraz|khor|хор") ~ "Khorezm",
    stringr::str_detect(txt, "namang|наманг") ~ "Namangan",
    stringr::str_detect(txt, "navoi|navoiy|наво") ~ "Navoi",
    stringr::str_detect(txt, "qash|kash|қаш") ~ "Kashkadarya",
    stringr::str_detect(txt, "samar|самар") ~ "Samarkand",
    stringr::str_detect(txt, "sird|syrd|сирд") ~ "Syrdarya",
    stringr::str_detect(txt, "surx|surk|сурх") ~ "Surkhandarya",
    stringr::str_detect(txt, "toshkent.*sh|tashkent.*city|ташкент шаҳ") ~ "Tashkent City",
    stringr::str_detect(txt, "toshkent|tashkent|ташкент") ~ "Tashkent Region",
    TRUE ~ stringr::str_to_title(trimws(as.character(x)))
  )
}

hbs_gender_label <- function(x) {
  txt <- trimws(tolower(as_label_text(x)))
  dplyr::case_when(
    txt %in% c("2", "female", "2 [female]") ~ "female",
    txt %in% c("1", "male", "1 [male]") ~ "male",
    stringr::str_detect(txt, "female") ~ "female",
    stringr::str_detect(txt, "male") ~ "male",
    TRUE ~ NA_character_
  )
}

hbs_binary <- function(x) {
  txt <- hbs_text(x)
  num <- suppressWarnings(as.numeric(trimws(as.character(x))))
  observed_num <- unique(num[!is.na(num)])
  use_numeric_binary <- length(observed_num) > 0 && all(observed_num %in% c(0, 1))

  out <- dplyr::case_when(
    use_numeric_binary & !is.na(num) & num == 1 ~ 1L,
    use_numeric_binary & !is.na(num) & num == 0 ~ 0L,
    stringr::str_detect(txt, "no|йўқ|yo'q|not selected|rural") ~ 0L,
    stringr::str_detect(txt, "yes|ҳа|ha|selected|urban") ~ 1L,
    TRUE ~ NA_integer_
  )
  as.integer(out)
}

hbs_binary_any <- function(x) {
  out <- hbs_binary(x)
  ifelse(is.na(out) & !is.na(hbs_numeric(x)) & hbs_numeric(x) > 0, 1L, out)
}

hbs_first_non_missing <- function(x) {
  idx <- which(!is.na(x) & trimws(as.character(x)) != "")
  if (length(idx) == 0) {
    return(NA)
  }
  x[[idx[[1]]]]
}

hbs_collapse_binary <- function(x) {
  x <- as.integer(x)
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_integer_)
  }
  as.integer(any(x == 1L, na.rm = TRUE))
}

hbs_weighted_share <- function(x, w, mask = NULL) {
  if (is.null(mask)) {
    mask <- rep(TRUE, length(x))
  }
  keep <- mask & !is.na(x) & !is.na(w)
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep])
}

hbs_available_years_label <- function(df, var_name) {
  years <- sort(unique(df$year[!is.na(df[[var_name]])]))
  if (length(years) == 0) {
    return(NA_character_)
  }
  paste(years, collapse = ", ")
}

hbs_map_education_years <- function(edu_years, edu_highest) {
  years_num <- hbs_numeric(edu_years)
  txt <- hbs_text(edu_highest)

  mapped <- dplyr::case_when(
    !is.na(years_num) ~ years_num,
    stringr::str_detect(txt, "phd|fan nomzodi|magistr|master") ~ 18,
    stringr::str_detect(txt, "bachelor|бакалавр|higher education|олий таълим") ~ 16,
    stringr::str_detect(txt, "vocational|ўрта махсус|орта махсус") ~ 13,
    stringr::str_detect(txt, "high school|тўлиқ ўрта|тулиқ урта") ~ 11,
    stringr::str_detect(txt, "general secondary|9 \\(8\\)|тўлиқсиз ўрта|туликсиз урта") ~ 9,
    stringr::str_detect(txt, "primary|бошланғич|boshlang") ~ 6,
    stringr::str_detect(txt, "preschool|kindergarten|мактабгача|none|маълумотга эга эмас") ~ 0,
    TRUE ~ NA_real_
  )

  as.numeric(mapped)
}

hbs_read_household_weights <- function(folder) {
  path <- hbs_find_module_path(folder, "m00_weight\\.dta$|M00_weight\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }

  x <- haven::read_dta(path)
  tibble::tibble(
    year = hbs_extract_year(path),
    hhid = as.character(hbs_col_or_na(x, "hhid")),
    province = hbs_normalize_province(hbs_col_or_na(x, "province")),
    urban = coerce_urban_binary(as_label_text(hbs_col_or_na(x, "urban"))),
    household_weight = dplyr::coalesce(
      hbs_numeric(hbs_col_or_na(x, "uwgt")),
      hbs_numeric(hbs_col_or_na(x, "popw")),
      hbs_numeric(hbs_col_or_na(x, "indw"))
    )
  ) %>%
    dplyr::distinct(year, hhid, .keep_all = TRUE)
}

hbs_build_multigenerational <- function(folder) {
  path <- hbs_find_module_path(folder, "m01_roster\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }

  x <- haven::read_dta(path)
  tibble::tibble(
    year = hbs_extract_year(path),
    hhid = as.character(hbs_col_or_na(x, "hhid")),
    age = hbs_numeric(hbs_col_or_na(x, "age"))
  ) %>%
    dplyr::group_by(year, hhid) %>%
    dplyr::summarise(
      multigenerational_hh = dplyr::if_else(
        any(!is.na(age) & age <= 17) & any(!is.na(age) & age >= 60),
        1L,
        0L,
        missing = NA_integer_
      ),
      .groups = "drop"
    )
}

hbs_build_education_household_flags <- function(folder) {
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
      "edu_tutor_fee"
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
    dplyr::if_else(!is.na(hbs_numeric(hbs_col_or_na(x, "edu_tutor_fee"))), as.integer(hbs_numeric(hbs_col_or_na(x, "edu_tutor_fee")) > 0), NA_integer_)
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
}

hbs_build_migration_household_flags <- function(folder) {
  path <- hbs_find_module_path(folder, "m02_migration\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }

  x <- haven::read_dta(path)
  remit_binary <- dplyr::coalesce(
    hbs_binary_any(hbs_col_or_na(x, "emig_remit")),
    dplyr::if_else(!is.na(hbs_numeric(hbs_col_or_na(x, "emig_remit_val"))), as.integer(hbs_numeric(hbs_col_or_na(x, "emig_remit_val")) > 0), NA_integer_)
  )

  tibble::tibble(
    year = hbs_extract_year(path),
    hhid = as.character(hbs_col_or_na(x, "hhid")),
    has_emigrant_hh = hbs_binary_any(hbs_col_or_na(x, "emig")),
    has_remittance_hh = remit_binary,
    remittance_for_education = hbs_binary_any(hbs_col_or_na(x, "emig_remit_use_educ"))
  ) %>%
    dplyr::group_by(year, hhid) %>%
    dplyr::summarise(
      has_emigrant_hh = hbs_collapse_binary(has_emigrant_hh),
      has_remittance_hh = hbs_collapse_binary(has_remittance_hh),
      remittance_for_education = hbs_collapse_binary(remittance_for_education),
      .groups = "drop"
    )
}

hbs_build_internet_household_flags <- function(folder) {
  path <- hbs_find_module_path(folder, "m13_internet\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }

  x <- haven::read_dta(path)
  tibble::tibble(
    year = hbs_extract_year(path),
    hhid = as.character(hbs_col_or_na(x, "hhid")),
    internet_access_hh = hbs_binary_any(hbs_col_or_na(x, "internet"))
  ) %>%
    dplyr::group_by(year, hhid) %>%
    dplyr::summarise(
      internet_access_hh = hbs_collapse_binary(internet_access_hh),
      .groups = "drop"
    )
}

build_hbs_household_context <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  ensure_project_dirs()
  folders <- hbs_year_folders(raw_dir)
  if (length(folders) == 0) {
    return(list(
      household_context = tibble::tibble(),
      household_support_context = tibble::tibble()
    ))
  }

  weights <- purrr::map_dfr(folders, hbs_read_household_weights)
  multigen <- purrr::map_dfr(folders, hbs_build_multigenerational)
  education <- purrr::map_dfr(folders, hbs_build_education_household_flags)
  migration <- purrr::map_dfr(folders, hbs_build_migration_household_flags)
  internet <- purrr::map_dfr(folders, hbs_build_internet_household_flags)

  education_years <- sort(unique(education$year))
  migration_years <- sort(unique(migration$year))
  internet_years <- sort(unique(internet$year))
  multigen_years <- sort(unique(multigen$year))

  household_context <- weights %>%
    dplyr::left_join(multigen, by = c("year", "hhid")) %>%
    dplyr::left_join(education, by = c("year", "hhid")) %>%
    dplyr::left_join(migration, by = c("year", "hhid")) %>%
    dplyr::left_join(internet, by = c("year", "hhid")) %>%
    dplyr::mutate(
      multigenerational_hh = dplyr::if_else(year %in% multigen_years & is.na(multigenerational_hh), 0L, multigenerational_hh),
      has_enrolled_member = dplyr::if_else(year %in% education_years & is.na(has_enrolled_member), 0L, has_enrolled_member),
      education_spending_positive = dplyr::if_else(year %in% education_years & is.na(education_spending_positive), 0L, education_spending_positive),
      has_tutoring = dplyr::if_else(year %in% education_years & is.na(has_tutoring), 0L, has_tutoring),
      has_emigrant_hh = dplyr::if_else(year %in% migration_years & is.na(has_emigrant_hh), 0L, has_emigrant_hh),
      has_remittance_hh = dplyr::if_else(year %in% migration_years & is.na(has_remittance_hh), 0L, has_remittance_hh),
      remittance_for_education = dplyr::if_else(year %in% migration_years & is.na(remittance_for_education), 0L, remittance_for_education),
      internet_access_hh = dplyr::if_else(year %in% internet_years & is.na(internet_access_hh), 0L, internet_access_hh)
    ) %>%
    dplyr::arrange(year, province, hhid)

  metric_specs <- tibble::tribble(
    ~metric, ~label, ~var, ~denominator_var,
    "has_enrolled_member", "Households with enrolled members", "has_enrolled_member", NA_character_,
    "education_spending_positive", "Households with positive education spending", "education_spending_positive", NA_character_,
    "has_tutoring", "Households with tutoring", "has_tutoring", NA_character_,
    "has_emigrant_hh", "Households with emigrant member", "has_emigrant_hh", NA_character_,
    "has_remittance_hh", "Households receiving remittances", "has_remittance_hh", NA_character_,
    "remittance_for_education", "Remittance households using remittances for education", "remittance_for_education", "has_remittance_hh",
    "internet_access_hh", "Households with internet access", "internet_access_hh", NA_character_
  )

  support_context <- purrr::pmap_dfr(metric_specs, function(metric, label, var, denominator_var) {
    denom_mask <- rep(TRUE, nrow(household_context))
    if (!is.na(denominator_var)) {
      denom_mask <- household_context[[denominator_var]] == 1L
    }

    tibble::tibble(
      metric = metric,
      row_label = label,
      national = hbs_weighted_share(household_context[[var]], household_context$household_weight, denom_mask),
      urban = hbs_weighted_share(
        household_context[[var]],
        household_context$household_weight,
        denom_mask & household_context$urban == 1L
      ),
      rural = hbs_weighted_share(
        household_context[[var]],
        household_context$household_weight,
        denom_mask & household_context$urban == 0L
      ),
      denominator = dplyr::if_else(is.na(denominator_var), "all households", "remittance households"),
      available_years = hbs_available_years_label(household_context, var)
    )
  })

  list(
    household_context = household_context,
    household_support_context = support_context
  )
}

write_hbs_household_context_outputs <- function(
  hbs_context,
  processed_path = file.path(PROJ_PATHS$processed_data, "hbs_household_context.csv"),
  table_path = file.path(PROJ_PATHS$tables, "hbs_household_support_context.csv")
) {
  context_df <- hbs_context$household_context
  support_df <- hbs_context$household_support_context

  if (is.null(context_df)) context_df <- tibble::tibble()
  if (is.null(support_df)) support_df <- tibble::tibble()

  safe_write_csv(context_df, processed_path)
  safe_write_csv(support_df, table_path)

  c(processed_path, table_path)
}
