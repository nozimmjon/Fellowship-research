suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(janitor)
  library(haven)
  library(broom)
})

source(file.path("R", "00_config.R"))
activate_local_lib()
source(file.path("R", "20_ingest_data.R"))
source(file.path("R", "31_build_hbs_household_context.R"))
source(file.path("R", "32_build_hbs_linkage_diagnostics.R"))
source(file.path("R", "90_reporting_helpers.R"))

MODULE_PATHS <- list(
  analysis = file.path(PROJECT_ROOT, "analysis", "hbs_expansion_causal"),
  outputs = file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal"),
  tables = file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal", "tables"),
  figures = file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal", "figures"),
  reports = file.path(PROJECT_ROOT, "reports"),
  progress = file.path(PROJECT_ROOT, "HBS_EXPANSION_PROGRESS.md"),
  harmonized_parquet = file.path(PROJ_PATHS$processed_data, "hbs_expansion_person_harmonized.parquet"),
  merged_parquet = file.path(PROJ_PATHS$processed_data, "hbs_expansion_merged.parquet"),
  harmonized_tmp_csv = file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal", "tables", "_tmp_hbs_expansion_person_harmonized.csv"),
  merged_tmp_csv = file.path(PROJECT_ROOT, "outputs", "hbs_expansion_causal", "tables", "_tmp_hbs_expansion_merged.csv")
)

AGE_WINDOWS <- list(
  `18_24` = c(18L, 24L),
  `22_30` = c(22L, 30L),
  `25_35` = c(25L, 35L)
)

CORE_DOMAINS <- c("roster", "education", "weights", "migration", "internet", "welfare")

ensure_hbs_expansion_dirs <- function() {
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

finite_max <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

weighted_mean_safe <- function(x, w = NULL) {
  x <- suppressWarnings(as.numeric(x))
  keep <- !is.na(x)
  if (!any(keep)) {
    return(NA_real_)
  }
  if (is.null(w)) {
    return(mean(x[keep]))
  }
  w <- suppressWarnings(as.numeric(w))
  if (all(is.na(w[keep]))) {
    return(mean(x[keep]))
  }
  w_use <- w
  w_use[is.na(w_use) | w_use <= 0] <- 1
  stats::weighted.mean(x[keep], w_use[keep])
}

weighted_share_safe <- function(x, w = NULL) {
  weighted_mean_safe(as.numeric(x), w)
}

weighted_group_shares <- function(df, group_var, weight_var = "sample_weight") {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      group = character(),
      share = numeric()
    ))
  }
  group_vals <- as.character(df[[group_var]])
  weight_vals <- suppressWarnings(as.numeric(df[[weight_var]]))
  weight_vals[is.na(weight_vals) | weight_vals <= 0] <- 1
  tmp <- tibble::tibble(group = group_vals, weight = weight_vals) %>%
    filter(!is.na(group), group != "") %>%
    group_by(group) %>%
    summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop")
  total_weight <- sum(tmp$weight, na.rm = TRUE)
  if (is.na(total_weight) || total_weight <= 0) {
    tmp$share <- NA_real_
  } else {
    tmp$share <- tmp$weight / total_weight
  }
  tmp %>% select(group, share)
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(format(round(100 * x, digits), nsmall = digits, trim = TRUE), "%"))
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

progress_state <- new.env(parent = emptyenv())
progress_state$entries <- list()

write_progress_log <- function() {
  lines <- c(
    "# HBS Expansion Progress",
    "",
    "This log tracks the exploratory HBS university-expansion module.",
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

select_admin_file <- function(filename) {
  candidates <- c(
    file.path(PROJ_PATHS$processed_data, "admin", filename),
    file.path(PROJ_PATHS$raw_data, "admin", filename)
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0) {
    stop("Required admin input missing: ", filename)
  }
  hits[[1]]
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

module_stub_from_path <- function(path) {
  sub("^uzb_hbs_\\d{4}_\\d{8}_", "", tools::file_path_sans_ext(basename(path)))
}

module_code_from_stub <- function(stub) {
  out <- stringr::str_extract(stub, "^[Mm]\\d+[a-z]?")
  ifelse(is.na(out), stub, out)
}

classify_hbs_domain <- function(stub) {
  stub <- tolower(stub)
  case_when(
    str_detect(stub, "roster") ~ "roster",
    str_detect(stub, "education") ~ "education",
    str_detect(stub, "weight") ~ "weights",
    str_detect(stub, "migration") ~ "migration",
    str_detect(stub, "internet") ~ "internet",
    str_detect(stub, "dwelling|food|nonfood|asset|cost|socben|passport|fuel|production|fao") ~ "welfare",
    TRUE ~ "other"
  )
}

build_hbs_manifest_inventory <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  folders <- hbs_year_folders(raw_dir)
  if (length(folders) == 0) {
    return(list(
      manifest = tibble::tibble(),
      inventory = tibble::tibble(),
      years_present = integer()
    ))
  }

  manifest_rows <- list()
  inventory_rows <- list()

  for (folder in folders) {
    files <- list.files(folder, pattern = "\\.dta$", full.names = TRUE)
    for (path in files) {
      df <- haven::read_dta(path)
      survey_year <- hbs_extract_year(path)
      stub <- module_stub_from_path(path)
      domain <- classify_hbs_domain(stub)
      module_code <- module_code_from_stub(stub)
      labels <- vapply(
        names(df),
        function(var_name) {
          label_val <- attr(df[[var_name]], "label", exact = TRUE)
          if (is.null(label_val)) {
            NA_character_
          } else {
            as.character(label_val)
          }
        },
        FUN.VALUE = character(1)
      )

      manifest_rows[[length(manifest_rows) + 1L]] <- tibble::tibble(
        survey_year = survey_year,
        extract_folder = basename(folder),
        file_name = basename(path),
        module_code = module_code,
        module_stub = stub,
        core_domain = domain,
        n_rows = nrow(df),
        n_cols = ncol(df)
      )

      inventory_rows[[length(inventory_rows) + 1L]] <- tibble::tibble(
        survey_year = survey_year,
        extract_folder = basename(folder),
        file_name = basename(path),
        module_code = module_code,
        module_stub = stub,
        core_domain = domain,
        variable_name = names(df),
        variable_label = labels,
        storage_class = vapply(df, function(x) class(x)[1], FUN.VALUE = character(1)),
        non_missing_share = vapply(df, function(x) mean(!is.na(x)), FUN.VALUE = numeric(1))
      )
    }
  }

  manifest <- bind_rows(manifest_rows)
  inventory <- bind_rows(inventory_rows)
  years_present <- sort(unique(manifest$survey_year))
  n_years <- length(years_present)

  domain_status <- manifest %>%
    filter(core_domain %in% CORE_DOMAINS) %>%
    group_by(survey_year, core_domain) %>%
    summarise(
      files_in_domain = n(),
      module_files = paste(sort(file_name), collapse = "; "),
      .groups = "drop"
    )

  domain_stability <- inventory %>%
    filter(core_domain %in% CORE_DOMAINS) %>%
    group_by(core_domain, variable_name) %>%
    summarise(years_present = n_distinct(survey_year), .groups = "drop") %>%
    group_by(core_domain) %>%
    summarise(
      variables_observed = n(),
      variables_present_all_years = sum(years_present == n_years),
      stable_variable_share = variables_present_all_years / variables_observed,
      .groups = "drop"
    )

  manifest <- manifest %>%
    left_join(
      manifest %>%
        filter(core_domain %in% CORE_DOMAINS) %>%
        count(survey_year, core_domain, name = "files_in_domain"),
      by = c("survey_year", "core_domain")
    ) %>%
    left_join(domain_stability, by = "core_domain") %>%
    mutate(
      domain_available_this_year = !is.na(files_in_domain) & files_in_domain > 0,
      domain_available_all_5y = core_domain %in% CORE_DOMAINS &
        core_domain %in% (domain_status %>% count(core_domain) %>% filter(n == n_years) %>% pull(core_domain))
    )

  inventory <- inventory %>%
    left_join(
      inventory %>%
        filter(core_domain %in% CORE_DOMAINS) %>%
        group_by(core_domain, variable_name) %>%
        summarise(years_present = n_distinct(survey_year), .groups = "drop"),
      by = c("core_domain", "variable_name")
    ) %>%
    mutate(
      present_all_5y_in_domain = !is.na(years_present) & years_present == n_years
    )

  list(
    manifest = manifest,
    inventory = inventory,
    years_present = years_present
  )
}

select_person_id <- function(df) {
  dplyr::coalesce(
    as.character(hbs_col_or_na(df, "fmid")),
    as.character(hbs_col_or_na(df, "sid")),
    as.character(hbs_col_or_na(df, "iid"))
  )
}

relationship_text <- function(x) {
  trimws(as_label_text(x))
}

tertiary_text_flag <- function(x) {
  txt <- hbs_text(x)
  as.integer(
    stringr::str_detect(
      txt,
      "higher education|bachelor|master|phd|doctor|postgraduate|oliy|бакалавр|магист|доктор|post graduate"
    )
  )
}

owner_flag_from_text <- function(x) {
  txt <- hbs_text(x)
  dplyr::case_when(
    txt == "" ~ NA_integer_,
    stringr::str_detect(txt, "owner") ~ 1L,
    stringr::str_detect(txt, "mortgage") ~ 1L,
    stringr::str_detect(txt, "rent|free|other") ~ 0L,
    TRUE ~ NA_integer_
  )
}

read_roster_person_file <- function(folder) {
  path <- hbs_find_module_path(folder, "m01_roster\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }
  x <- haven::read_dta(path)
  tibble::tibble(
    survey_year = hbs_extract_year(path),
    household_id = as.character(hbs_col_or_na(x, "hhid")),
    person_id = select_person_id(x),
    interview_date = as.Date(hbs_col_or_na(x, "date")),
    region_raw = hbs_normalize_province(hbs_col_or_na(x, "province")),
    relationship_code = suppressWarnings(as.integer(as.numeric(hbs_col_or_na(x, "relationship")))),
    relationship_to_head = relationship_text(hbs_col_or_na(x, "relationship")),
    sex = hbs_gender_label(hbs_col_or_na(x, "gender")),
    age = hbs_numeric(hbs_col_or_na(x, "age")),
    household_size = hbs_numeric(hbs_col_or_na(x, "hhsize"))
  )
}

read_education_person_file <- function(folder) {
  path <- hbs_find_module_path(folder, "m03_education\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }
  x <- haven::read_dta(path)
  education_years <- hbs_map_education_years(hbs_col_or_na(x, "edu_years"), hbs_col_or_na(x, "edu_highest"))
  highest_txt <- hbs_col_or_na(x, "edu_highest")
  tertiary_level <- tertiary_text_flag(highest_txt)
  currently_enrolled <- hbs_binary(hbs_col_or_na(x, "edu_enrolled"))
  edu_complete <- hbs_binary(hbs_col_or_na(x, "edu_complete"))
  education_observed <- !is.na(education_years) | !is.na(tertiary_level)
  tertiary_completed <- dplyr::case_when(
    !education_observed ~ NA_integer_,
    tertiary_level == 0L ~ 0L,
    tertiary_level == 1L & edu_complete == 1L ~ 1L,
    tertiary_level == 1L & edu_complete == 0L ~ 0L,
    tertiary_level == 1L & currently_enrolled == 1L ~ 0L,
    tertiary_level == 1L ~ 1L,
    TRUE ~ NA_integer_
  )
  tibble::tibble(
    survey_year = hbs_extract_year(path),
    household_id = as.character(hbs_col_or_na(x, "hhid")),
    person_id = select_person_id(x),
    education_level = years_to_education_level(education_years),
    education_years = education_years,
    currently_enrolled = currently_enrolled,
    tertiary_enrolled_proxy = dplyr::case_when(
      is.na(currently_enrolled) | is.na(tertiary_level) ~ NA_integer_,
      currently_enrolled == 1L & tertiary_level == 1L ~ 1L,
      currently_enrolled == 1L & tertiary_level == 0L ~ 0L,
      currently_enrolled == 0L ~ 0L,
      TRUE ~ NA_integer_
    ),
    tertiary_completed_proxy = tertiary_completed
  )
}

read_person_migration_flags <- function(folder) {
  path <- hbs_find_module_path(folder, "m02_migration\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }
  x <- haven::read_dta(path)
  immigration_origin_region_raw <- trimws(as_label_text(hbs_col_or_na(x, "immig_province")))
  immigration_origin_region_raw[immigration_origin_region_raw %in% c("", "NA")] <- NA_character_
  tibble::tibble(
    survey_year = hbs_extract_year(path),
    household_id = as.character(hbs_col_or_na(x, "hhid")),
    person_id = select_person_id(x),
    immigrant_indicator = hbs_binary_any(hbs_col_or_na(x, "immig")),
    emigrant_indicator = hbs_binary_any(hbs_col_or_na(x, "emig")),
    return_migrant_indicator = hbs_binary_any(hbs_col_or_na(x, "emig_return")),
    immigration_origin_region_raw = immigration_origin_region_raw,
    immigration_year = hbs_numeric(hbs_col_or_na(x, "immig_year")),
    emigration_year = hbs_numeric(hbs_col_or_na(x, "emig_year")),
    return_migration_year = hbs_numeric(hbs_col_or_na(x, "emig_return_year"))
  )
}

build_household_welfare_proxy <- function(folder) {
  path <- hbs_find_module_path(folder, "dwelling\\.dta$")
  if (is.na(path)) {
    return(tibble::tibble())
  }
  x <- haven::read_dta(path)
  hhid <- as.character(hbs_col_or_na(x, "hhid"))
  hhsize <- hbs_numeric(hbs_col_or_na(x, "hhsize"))
  area_m2 <- dplyr::coalesce(
    hbs_numeric(hbs_col_or_na(x, "dwell_size")),
    hbs_numeric(hbs_col_or_na(x, "dwella2_area"))
  )
  rooms <- dplyr::coalesce(
    hbs_numeric(hbs_col_or_na(x, "dwell_room")),
    hbs_numeric(hbs_col_or_na(x, "dwella3"))
  )
  owner_txt <- dplyr::coalesce(
    hbs_col_or_na(x, "dwell_owner"),
    hbs_col_or_na(x, "dwella5")
  )
  owner_flag <- owner_flag_from_text(owner_txt)
  tibble::tibble(
    survey_year = hbs_extract_year(path),
    household_id = hhid,
    hhsize = hhsize,
    area_m2 = area_m2,
    rooms = rooms,
    owner_occupied_hh = owner_flag
  ) %>%
    mutate(
      area_per_capita = dplyr::if_else(!is.na(area_m2) & !is.na(hhsize) & hhsize > 0, area_m2 / hhsize, NA_real_),
      rooms_per_capita = dplyr::if_else(!is.na(rooms) & !is.na(hhsize) & hhsize > 0, rooms / hhsize, NA_real_),
      welfare_proxy_raw = dplyr::coalesce(log1p(area_per_capita), log1p(rooms_per_capita))
    ) %>%
    distinct(survey_year, household_id, .keep_all = TRUE)
}

build_expansion_person_harmonized <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  folders <- hbs_year_folders(raw_dir)
  if (length(folders) == 0) {
    return(tibble::tibble())
  }

  weights <- purrr::map_dfr(folders, hbs_read_household_weights) %>%
    transmute(
      survey_year = year,
      household_id = hhid,
      region_weight = province,
      urban = urban,
      sample_weight = household_weight
    )

  multigen <- purrr::map_dfr(folders, hbs_build_multigenerational) %>%
    transmute(
      survey_year = year,
      household_id = hhid,
      multigenerational_hh = multigenerational_hh
    )

  hh_migration <- purrr::map_dfr(folders, hbs_build_migration_household_flags) %>%
    transmute(
      survey_year = year,
      household_id = hhid,
      has_emigrant_hh = has_emigrant_hh,
      has_remittance_hh = has_remittance_hh,
      remittance_for_education = remittance_for_education
    )

  internet <- purrr::map_dfr(folders, hbs_build_internet_household_flags) %>%
    transmute(
      survey_year = year,
      household_id = hhid,
      internet_access_hh = internet_access_hh
    )

  welfare <- purrr::map_dfr(folders, build_household_welfare_proxy)
  roster <- purrr::map_dfr(folders, read_roster_person_file)
  education <- purrr::map_dfr(folders, read_education_person_file)
  person_migration <- purrr::map_dfr(folders, read_person_migration_flags)

  person_df <- roster %>%
    left_join(education, by = c("survey_year", "household_id", "person_id")) %>%
    left_join(person_migration, by = c("survey_year", "household_id", "person_id")) %>%
    left_join(weights, by = c("survey_year", "household_id")) %>%
    left_join(multigen, by = c("survey_year", "household_id")) %>%
    left_join(hh_migration, by = c("survey_year", "household_id")) %>%
    left_join(internet, by = c("survey_year", "household_id")) %>%
    left_join(welfare, by = c("survey_year", "household_id")) %>%
    mutate(
      region = dplyr::coalesce(region_weight, region_raw),
      admin_region = hbs_region_to_admin(region),
      birth_year = dplyr::if_else(!is.na(age), as.integer(round(survey_year - age)), NA_integer_),
      survey_month = suppressWarnings(as.integer(format(interview_date, "%m"))),
      household_id = as.character(household_id),
      person_id = as.character(person_id)
    ) %>%
    group_by(survey_year) %>%
    mutate(
      welfare_proxy = if (all(is.na(welfare_proxy_raw))) {
        rep(NA_real_, dplyr::n())
      } else {
        as.numeric(scale(welfare_proxy_raw))
      }
    ) %>%
    ungroup()

  parent_summary <- person_df %>%
    filter(relationship_code %in% c(1L, 2L)) %>%
    group_by(survey_year, household_id) %>%
    summarise(
      head_age = finite_max(age[relationship_code == 1L]),
      spouse_age = finite_max(age[relationship_code == 2L]),
      head_female_education = finite_max(education_years[relationship_code == 1L & sex == "female"]),
      spouse_female_education = finite_max(education_years[relationship_code == 2L & sex == "female"]),
      head_male_education = finite_max(education_years[relationship_code == 1L & sex == "male"]),
      spouse_male_education = finite_max(education_years[relationship_code == 2L & sex == "male"]),
      .groups = "drop"
    )

  person_df %>%
    left_join(parent_summary, by = c("survey_year", "household_id")) %>%
    mutate(
      mother_education_years = dplyr::case_when(
        relationship_code == 3L & !is.na(head_age) & head_age >= age + 12 & !is.na(head_female_education) ~ head_female_education,
        relationship_code == 3L & !is.na(spouse_age) & spouse_age >= age + 12 & !is.na(spouse_female_education) ~ spouse_female_education,
        TRUE ~ NA_real_
      ),
      father_education_years = dplyr::case_when(
        relationship_code == 3L & !is.na(head_age) & head_age >= age + 12 & !is.na(head_male_education) ~ head_male_education,
        relationship_code == 3L & !is.na(spouse_age) & spouse_age >= age + 12 & !is.na(spouse_male_education) ~ spouse_male_education,
        TRUE ~ NA_real_
      ),
      linked_to_mother = as.integer(!is.na(mother_education_years)),
      linked_to_father = as.integer(!is.na(father_education_years)),
      parent_proxy_years = dplyr::case_when(
        !is.na(mother_education_years) & !is.na(father_education_years) ~ pmax(mother_education_years, father_education_years),
        !is.na(mother_education_years) ~ mother_education_years,
        !is.na(father_education_years) ~ father_education_years,
        TRUE ~ NA_real_
      ),
      parent_proxy_available = as.integer(!is.na(parent_proxy_years)),
      linked_under_rule = as.integer(relationship_code == 3L & !is.na(parent_proxy_years)),
      parent_proxy_source = dplyr::case_when(
        linked_to_mother == 1L & linked_to_father == 1L ~ "both parents",
        linked_to_mother == 1L & linked_to_father == 0L ~ "mother only",
        linked_to_mother == 0L & linked_to_father == 1L ~ "father only",
        relationship_code == 3L & parent_proxy_available == 1L ~ "parent proxy",
        TRUE ~ "none"
      ),
      low_parent_education = dplyr::case_when(
        is.na(parent_proxy_years) ~ NA_integer_,
        parent_proxy_years <= 11 ~ 1L,
        parent_proxy_years > 11 ~ 0L,
        TRUE ~ NA_integer_
      ),
      upward_mobility_proxy = dplyr::case_when(
        is.na(education_years) | is.na(parent_proxy_years) ~ NA_integer_,
        education_years > parent_proxy_years ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    transmute(
      survey_year,
      interview_date,
      survey_month,
      household_id,
      person_id,
      age,
      birth_year,
      sex,
      region,
      admin_region,
      urban,
      relationship_to_head,
      relationship_code,
      education_level,
      education_years,
      currently_enrolled,
      tertiary_enrolled_proxy,
      tertiary_completed_proxy,
      mother_education_years,
      father_education_years,
      parent_proxy_years,
      parent_proxy_available,
      parent_proxy_source,
      linked_to_mother,
      linked_to_father,
      linked_under_rule,
      low_parent_education,
      upward_mobility_proxy,
      welfare_proxy,
      welfare_proxy_raw,
      area_per_capita,
      rooms_per_capita,
      owner_occupied_hh,
      immigrant_indicator,
      emigrant_indicator,
      return_migrant_indicator,
      immigration_origin_region_raw,
      immigration_year,
      emigration_year,
      return_migration_year,
      has_emigrant_hh,
      has_remittance_hh,
      remittance_for_education,
      internet_access_hh,
      multigenerational_hh,
      sample_weight,
      person_migration_signal = as.integer(
        dplyr::coalesce(immigrant_indicator, 0L) == 1L |
          dplyr::coalesce(emigrant_indicator, 0L) == 1L |
          dplyr::coalesce(return_migrant_indicator, 0L) == 1L
      ),
      explicit_residence_history_signal = as.integer(
        dplyr::coalesce(immigrant_indicator, 0L) == 1L |
          dplyr::coalesce(emigrant_indicator, 0L) == 1L |
          dplyr::coalesce(return_migrant_indicator, 0L) == 1L |
          !is.na(immigration_origin_region_raw) |
          !is.na(immigration_year) |
          !is.na(emigration_year) |
          !is.na(return_migration_year)
      ),
      household_migration_signal = as.integer(
        dplyr::coalesce(has_emigrant_hh, 0L) == 1L |
          dplyr::coalesce(has_remittance_hh, 0L) == 1L |
          dplyr::coalesce(remittance_for_education, 0L) == 1L
      ),
      residence_instability_signal = as.integer(
        dplyr::coalesce(explicit_residence_history_signal, 0L) == 1L |
          dplyr::coalesce(household_migration_signal, 0L) == 1L
      )
    )
}

get_old_pooled_link_rate <- function() {
  diagnostics_path <- file.path(PROJ_PATHS$processed_data, "hbs_linkage_diagnostics.csv")
  if (file.exists(diagnostics_path)) {
    old_diag <- readr::read_csv(diagnostics_path, show_col_types = FALSE, progress = FALSE)
    if ("link_rate" %in% names(old_diag) && nrow(old_diag) > 0) {
      return(suppressWarnings(as.numeric(old_diag$link_rate[[1]])))
    }
  }
  linkage_results <- build_hbs_linkage_diagnostics()
  if (!is.null(linkage_results$diagnostics) && nrow(linkage_results$diagnostics) > 0) {
    return(suppressWarnings(as.numeric(linkage_results$diagnostics$link_rate[[1]])))
  }
  NA_real_
}

build_linkage_outputs <- function(person_df, old_pooled_link_rate) {
  linkage_summary <- purrr::imap_dfr(AGE_WINDOWS, function(bounds, window_name) {
    eligible <- person_df %>% filter(!is.na(age), age >= bounds[[1]], age <= bounds[[2]])
    linked <- eligible %>% filter(linked_under_rule == 1L)

    full_region <- weighted_group_shares(eligible, "admin_region")
    linked_region <- weighted_group_shares(linked, "admin_region")
    region_gap <- full_join(full_region, linked_region, by = "group", suffix = c("_eligible", "_linked")) %>%
      mutate(gap = abs(share_linked - share_eligible))
    region_gap_max <- if (nrow(region_gap) == 0) NA_real_ else max(region_gap$gap, na.rm = TRUE)
    if (!is.finite(region_gap_max)) region_gap_max <- NA_real_
    largest_region <- if (nrow(region_gap) == 0 || all(is.na(region_gap$gap))) NA_character_ else region_gap$group[[which.max(replace(region_gap$gap, is.na(region_gap$gap), -Inf))]]

    tibble::tibble(
      age_window = window_name,
      age_min = bounds[[1]],
      age_max = bounds[[2]],
      eligible_sample_n = nrow(eligible),
      linked_sample_n = nrow(linked),
      link_rate = if (nrow(eligible) > 0) nrow(linked) / nrow(eligible) else NA_real_,
      linked_to_mother_rate = weighted_share_safe(eligible$linked_to_mother == 1L, eligible$sample_weight),
      linked_to_father_rate = weighted_share_safe(eligible$linked_to_father == 1L, eligible$sample_weight),
      usable_parent_proxy_share = weighted_share_safe(eligible$parent_proxy_available == 1L, eligible$sample_weight),
      eligible_mean_age = weighted_mean_safe(eligible$age, eligible$sample_weight),
      linked_mean_age = weighted_mean_safe(linked$age, linked$sample_weight),
      eligible_female_share = weighted_share_safe(eligible$sex == "female", eligible$sample_weight),
      linked_female_share = weighted_share_safe(linked$sex == "female", linked$sample_weight),
      eligible_urban_share = weighted_share_safe(eligible$urban == 1L, eligible$sample_weight),
      linked_urban_share = weighted_share_safe(linked$urban == 1L, linked$sample_weight),
      age_gap = abs(linked_mean_age - eligible_mean_age),
      female_gap = abs(linked_female_share - eligible_female_share),
      urban_gap = abs(linked_urban_share - eligible_urban_share),
      max_abs_region_share_gap = region_gap_max,
      largest_region_gap_region = largest_region,
      old_pooled_25_64_link_rate = old_pooled_link_rate,
      improvement_vs_old_pooled = link_rate - old_pooled_link_rate,
      materially_better_than_old_pooled = !is.na(link_rate) & !is.na(old_pooled_link_rate) & (link_rate - old_pooled_link_rate) >= 0.05
    )
  })

  linkage_by_year <- purrr::imap_dfr(AGE_WINDOWS, function(bounds, window_name) {
    person_df %>%
      filter(!is.na(age), age >= bounds[[1]], age <= bounds[[2]]) %>%
      group_by(age_window = window_name, survey_year) %>%
      summarise(
        eligible_sample_n = n(),
        linked_sample_n = sum(linked_under_rule == 1L, na.rm = TRUE),
        link_rate = linked_sample_n / eligible_sample_n,
        linked_to_mother_rate = weighted_share_safe(linked_to_mother == 1L, sample_weight),
        linked_to_father_rate = weighted_share_safe(linked_to_father == 1L, sample_weight),
        usable_parent_proxy_share = weighted_share_safe(parent_proxy_available == 1L, sample_weight),
        immigrant_share = weighted_share_safe(immigrant_indicator == 1L, sample_weight),
        .groups = "drop"
      )
  })

  failure_modes <- person_df %>%
    mutate(
      age_window = case_when(
        !is.na(age) & age >= 18 & age <= 24 ~ "18_24",
        !is.na(age) & age >= 22 & age <= 30 ~ "22_30",
        !is.na(age) & age >= 25 & age <= 35 ~ "25_35",
        TRUE ~ NA_character_
      ),
      failure_mode = case_when(
        is.na(age_window) ~ NA_character_,
        relationship_code != 3L ~ "Not coded as son/daughter of household head",
        relationship_code == 3L & is.na(parent_proxy_years) ~ "Co-resident child but no usable parental education under age-gap rule",
        relationship_code == 3L & is.na(mother_education_years) & !is.na(father_education_years) ~ "Mother link missing",
        relationship_code == 3L & !is.na(mother_education_years) & is.na(father_education_years) ~ "Father link missing",
        relationship_code == 3L & !is.na(parent_proxy_years) ~ "Linked under conservative rule",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(age_window), !is.na(failure_mode)) %>%
    count(age_window, failure_mode, name = "n") %>%
    group_by(age_window) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()

  failure_lines <- c(
    "# Linkage Failure Modes",
    "",
    "These counts are based on the new exploratory HBS expansion-design linkage audit and not on the older pooled ages 25-64 supplement.",
    ""
  )
  if (nrow(failure_modes) == 0) {
    failure_lines <- c(failure_lines, "No linkage-failure modes were available.")
  } else {
    for (window_name in unique(failure_modes$age_window)) {
      sub_df <- failure_modes %>% filter(age_window == window_name) %>% arrange(desc(n))
      failure_lines <- c(failure_lines, paste0("## Age Window ", window_name), "")
      failure_lines <- c(
        failure_lines,
        paste0("- ", sub_df$failure_mode, ": ", sub_df$n, " cases (", fmt_pct(sub_df$share), ")."),
        ""
      )
    }
  }

  list(
    linkage_summary = linkage_summary,
    linkage_by_year = linkage_by_year,
    linkage_failure_lines = failure_lines
  )
}

attach_admin_treatment <- function(person_df, admin_panel) {
  admin_current <- admin_panel %>%
    transmute(
      admin_region,
      academic_year_start = as.integer(academic_year_start),
      admissions_bachelor_per_1000_youth_20_24,
      hei_count_per_1000_youth_20_24,
      students_total_per_1000_youth_20_24,
      expansion_index_main,
      high_expansion_region = as.integer(high_expansion_region_main)
    )

  max_admin_year <- max(admin_current$academic_year_start, na.rm = TRUE)
  min_admin_year <- min(admin_current$academic_year_start, na.rm = TRUE)

  merged <- person_df %>%
    mutate(
      academic_year_start_for_merge = pmax(min_admin_year, pmin(survey_year - 1L, max_admin_year))
    ) %>%
    left_join(admin_current, by = c("admin_region", "academic_year_start_for_merge" = "academic_year_start")) %>%
    mutate(row_id = row_number())

  exposure_long <- purrr::map_dfr(17:20, function(exposure_age) {
    tibble::tibble(
      row_id = merged$row_id,
      admin_region = merged$admin_region,
      academic_year_start = merged$birth_year + exposure_age,
      exposure_age = exposure_age
    )
  }) %>%
    filter(!is.na(admin_region), !is.na(academic_year_start)) %>%
    filter(academic_year_start >= min_admin_year, academic_year_start <= max_admin_year)

  exposure_summary <- exposure_long %>%
    left_join(admin_current, by = c("admin_region", "academic_year_start")) %>%
    group_by(row_id) %>%
    summarise(
      exposure_overlap_years = sum(!is.na(expansion_index_main)),
      exposure_index_17_20 = ifelse(exposure_overlap_years > 0, mean(expansion_index_main, na.rm = TRUE), 0),
      admissions_exposure_17_20 = ifelse(exposure_overlap_years > 0, mean(admissions_bachelor_per_1000_youth_20_24, na.rm = TRUE), 0),
      hei_exposure_17_20 = ifelse(exposure_overlap_years > 0, mean(hei_count_per_1000_youth_20_24, na.rm = TRUE), 0),
      students_exposure_17_20 = ifelse(exposure_overlap_years > 0, mean(students_total_per_1000_youth_20_24, na.rm = TRUE), 0),
      high_expansion_overlap_share = ifelse(exposure_overlap_years > 0, mean(high_expansion_region, na.rm = TRUE), 0),
      .groups = "drop"
    )

  merged <- merged %>%
    left_join(exposure_summary, by = "row_id") %>%
    mutate(
      exposure_overlap_years = dplyr::if_else(is.na(admin_region), NA_integer_, dplyr::coalesce(as.integer(exposure_overlap_years), 0L)),
      exposure_index_17_20 = dplyr::if_else(is.na(admin_region), NA_real_, dplyr::coalesce(exposure_index_17_20, 0)),
      admissions_exposure_17_20 = dplyr::if_else(is.na(admin_region), NA_real_, dplyr::coalesce(admissions_exposure_17_20, 0)),
      hei_exposure_17_20 = dplyr::if_else(is.na(admin_region), NA_real_, dplyr::coalesce(hei_exposure_17_20, 0)),
      students_exposure_17_20 = dplyr::if_else(is.na(admin_region), NA_real_, dplyr::coalesce(students_exposure_17_20, 0)),
      high_expansion_overlap_share = dplyr::if_else(is.na(admin_region), NA_real_, dplyr::coalesce(high_expansion_overlap_share, 0)),
      exposed_cohort_any_overlap = dplyr::if_else(is.na(exposure_overlap_years), NA_integer_, as.integer(exposure_overlap_years > 0)),
      exposed_cohort_full_overlap = dplyr::if_else(is.na(exposure_overlap_years), NA_integer_, as.integer(exposure_overlap_years == 4L)),
      cohort_exposure_group = dplyr::case_when(
        is.na(exposure_overlap_years) ~ NA_character_,
        exposure_overlap_years == 0L ~ "comparison_no_overlap",
        exposure_overlap_years %in% c(1L, 2L) ~ "partial_overlap_1_2",
        exposure_overlap_years == 3L ~ "partial_overlap_3",
        exposure_overlap_years == 4L ~ "full_overlap_4",
        TRUE ~ "other"
      )
    ) %>%
    select(-row_id)

  merge_coverage <- merged %>%
    group_by(survey_year, academic_year_start_for_merge, admin_region) %>%
    summarise(
      person_n = n(),
      matched_current_n = sum(!is.na(expansion_index_main)),
      current_merge_coverage = matched_current_n / person_n,
      mean_exposure_overlap_years = mean(exposure_overlap_years, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    merged = merged,
    merge_coverage = merge_coverage
  )
}

window_filter <- function(df, window_name) {
  bounds <- AGE_WINDOWS[[window_name]]
  df %>% filter(!is.na(age), age >= bounds[[1]], age <= bounds[[2]])
}

build_outcome_maturity_checks <- function(merged_df) {
  outcome_specs <- tibble::tribble(
    ~outcome_name, ~outcome_var,
    "tertiary enrollment", "tertiary_enrolled_proxy",
    "tertiary completion", "tertiary_completed_proxy",
    "upward mobility proxy", "upward_mobility_proxy"
  )

  purrr::imap_dfr(AGE_WINDOWS, function(bounds, window_name) {
    df_window <- merged_df %>%
      filter(linked_under_rule == 1L, !is.na(age), age >= bounds[[1]], age <= bounds[[2]])
    purrr::pmap_dfr(outcome_specs, function(outcome_name, outcome_var) {
      outcome_vals <- df_window[[outcome_var]]
      positive_rate <- if (all(is.na(outcome_vals))) NA_real_ else mean(outcome_vals == 1L, na.rm = TRUE)
      non_missing_share <- if (length(outcome_vals) == 0) NA_real_ else mean(!is.na(outcome_vals))
      age_appropriate <- dplyr::case_when(
        outcome_name == "tertiary enrollment" ~ bounds[[2]] <= 30,
        outcome_name == "tertiary completion" ~ bounds[[1]] >= 22,
        outcome_name == "upward mobility proxy" ~ bounds[[1]] >= 25,
        TRUE ~ FALSE
      )
      tibble::tibble(
        age_window = window_name,
        outcome_name = outcome_name,
        linked_sample_n = nrow(df_window),
        non_missing_share = non_missing_share,
        positive_rate = positive_rate,
        age_appropriate = age_appropriate
      )
    })
  })
}

choose_preferred_window <- function(linkage_summary) {
  linkage_summary %>%
    arrange(desc(link_rate), desc(linked_sample_n), desc(usable_parent_proxy_share), max_abs_region_share_gap) %>%
    slice(1) %>%
    pull(age_window)
}

choose_preferred_outcome <- function(outcome_checks, preferred_window) {
  outcome_checks %>%
    filter(age_window == preferred_window, age_appropriate) %>%
    mutate(score = non_missing_share + pmin(positive_rate, 1 - positive_rate, na.rm = TRUE)) %>%
    arrange(desc(score), desc(non_missing_share)) %>%
    slice(1) %>%
    pull(outcome_name)
}

outcome_var_from_name <- function(outcome_name) {
  dplyr::case_when(
    outcome_name == "tertiary enrollment" ~ "tertiary_enrolled_proxy",
    outcome_name == "tertiary completion" ~ "tertiary_completed_proxy",
    outcome_name == "upward mobility proxy" ~ "upward_mobility_proxy",
    TRUE ~ NA_character_
  )
}

write_harmonization_note <- function(admin_inputs) {
  admin_sources <- paste0("- `", basename(admin_inputs$paths), "` loaded from `", dirname(admin_inputs$paths), "`.")
  lines <- c(
    "# Harmonization Notes",
    "",
    "The exploratory HBS expansion module is isolated from the frozen LiTS main paper and from the older HBS supplement.",
    "",
    "## HBS person-level construction",
    "",
    "- The harmonized file uses the annual HBS roster as the person spine and left-joins education, migration, household weights, internet, migration/remittance, multigenerational, and dwelling-based welfare fields.",
    "- `survey_year`, `household_id`, and `person_id` are retained as the person identifiers.",
    "- `birth_year` is constructed as `survey_year - age`.",
    "- `region` is harmonized from the HBS province labels and then mapped to the admin treatment geography.",
    "- `currently_enrolled` comes from `edu_enrolled`.",
    "- `tertiary_enrolled_proxy` flags currently enrolled respondents whose education labels indicate a tertiary track.",
    "- `tertiary_completed_proxy` is a conservative proxy based on tertiary-level labels plus completion or non-enrollment status when available.",
    "- Parent linkage follows the conservative co-resident son/daughter rule, using head/spouse education with a minimum 12-year age gap.",
    "- `welfare_proxy` is a within-year standardized housing-space proxy built from dwelling area per capita, with rooms per capita as fallback.",
    "",
    "## Admin treatment inputs",
    "",
    admin_sources,
    "",
    "- Current treatment variables are merged with `academic_year_start = survey_year - 1`, which aligns each calendar-year HBS extract to the academic year in force across most of that survey year.",
    "- Respondent-specific exposure measures additionally average region-level treatment intensity over the years when each respondent was ages 17-20, limited to the observed 2019-2024 admin panel.",
    "",
    "## Output isolation",
    "",
    "- All new module outputs are written under `outputs/hbs_expansion_causal/` or to dedicated `hbs_expansion_*.parquet` processed files."
  )
  write_md_lines(file.path(MODULE_PATHS$outputs, "harmonization_notes.md"), lines)
}

write_cohort_definition_note <- function(admin_panel) {
  min_admin <- min(admin_panel$academic_year_start, na.rm = TRUE)
  max_admin <- max(admin_panel$academic_year_start, na.rm = TRUE)
  any_overlap_birth_min <- min_admin - 20L
  any_overlap_birth_max <- max_admin - 17L
  full_overlap_birth_min <- min_admin - 17L
  full_overlap_birth_max <- max_admin - 20L
  lines <- c(
    "# Cohort Definition Note",
    "",
    paste0("- The canonical university-expansion panel covers academic years ", min_admin, " through ", max_admin, "."),
    paste0("- Respondents born between ", any_overlap_birth_min, " and ", any_overlap_birth_max, " were ages 17-20 during at least one observed expansion year."),
    paste0("- Respondents born between ", full_overlap_birth_min, " and ", full_overlap_birth_max, " were ages 17-20 during all four observed ages within the 2019-2024 panel."),
    "- The exploratory design therefore distinguishes comparison cohorts with zero overlap from cohorts with partial or full overlap in ages 17-20 exposure years.",
    "- Current region is used as the exposure-region proxy, with migration prevalence checked separately before any model is estimated."
  )
  write_md_lines(file.path(MODULE_PATHS$outputs, "cohort_definition_note.md"), lines)
}

build_analysis_base_df <- function(merged_df, preferred_window, outcome_var = NULL, require_outcome = FALSE) {
  out <- merged_df %>%
    window_filter(preferred_window) %>%
    filter(
      linked_under_rule == 1L,
      !is.na(admin_region),
      !is.na(low_parent_education)
    )
  if (isTRUE(require_outcome) && !is.null(outcome_var)) {
    out <- out %>% filter(!is.na(.data[[outcome_var]]))
  }
  out
}

get_residence_sample_df <- function(df, sample_label) {
  if (sample_label == "full_linked") {
    return(df)
  }
  if (sample_label == "no_migration_signal") {
    return(
      df %>%
        filter(dplyr::coalesce(person_migration_signal, 0L) == 0L)
    )
  }
  if (sample_label == "conservative_likely_stayer") {
    return(
      df %>%
        filter(
          dplyr::coalesce(explicit_residence_history_signal, 0L) == 0L,
          dplyr::coalesce(household_migration_signal, 0L) == 0L
        )
    )
  }
  df[0, ]
}

build_residence_stability_comparison <- function(merged_df, preferred_window, preferred_outcome) {
  outcome_var <- outcome_var_from_name(preferred_outcome)
  eligible_window_df <- merged_df %>% window_filter(preferred_window)
  analysis_base_df <- build_analysis_base_df(
    merged_df = merged_df,
    preferred_window = preferred_window,
    outcome_var = outcome_var,
    require_outcome = TRUE
  )

  eligible_n <- nrow(eligible_window_df)
  full_linked_n <- nrow(analysis_base_df)

  sample_specs <- tibble::tribble(
    ~sample_label, ~sample_name, ~sample_description,
    "full_linked", "Full linked sample", "Full linked sample.",
    "no_migration_signal", "No migration-signal sample", "No migration-signal sample based on no person-level migration flag.",
    "conservative_likely_stayer", "Conservative likely-stayer sample", "Conservative likely-stayer sample based on explicit residence-history cues plus household migration cues."
  )

  comparison <- purrr::pmap_dfr(sample_specs, function(sample_label, sample_name, sample_description) {
    df_sample <- get_residence_sample_df(analysis_base_df, sample_label)
    cell_counts <- df_sample %>%
      count(admin_region, cohort_exposure_group, low_parent_education, name = "n")

    tibble::tibble(
      sample_label = sample_label,
      sample_name = sample_name,
      sample_description = sample_description,
      eligible_window_n = eligible_n,
      sample_n = nrow(df_sample),
      effective_link_rate = if (eligible_n > 0) nrow(df_sample) / eligible_n else NA_real_,
      retained_share_of_full_linked = if (full_linked_n > 0) nrow(df_sample) / full_linked_n else NA_real_,
      immigrant_share = weighted_share_safe(df_sample$immigrant_indicator == 1L, df_sample$sample_weight),
      person_migration_signal_share = weighted_share_safe(df_sample$person_migration_signal == 1L, df_sample$sample_weight),
      explicit_residence_history_share = weighted_share_safe(df_sample$explicit_residence_history_signal == 1L, df_sample$sample_weight),
      household_migration_signal_share = weighted_share_safe(df_sample$household_migration_signal == 1L, df_sample$sample_weight),
      exposure_region_ambiguity_share = weighted_share_safe(df_sample$residence_instability_signal == 1L, df_sample$sample_weight),
      female_share = weighted_share_safe(df_sample$sex == "female", df_sample$sample_weight),
      urban_share = weighted_share_safe(df_sample$urban == 1L, df_sample$sample_weight),
      merge_coverage = if (nrow(df_sample) > 0) mean(!is.na(df_sample$expansion_index_main)) else NA_real_,
      outcome_non_missing_share = if (nrow(df_sample) > 0) mean(!is.na(df_sample[[outcome_var]])) else NA_real_,
      outcome_positive_rate = weighted_share_safe(df_sample[[outcome_var]] == 1L, df_sample$sample_weight),
      cell_count_n = nrow(cell_counts),
      cell_median_n = if (nrow(cell_counts) > 0) median(cell_counts$n, na.rm = TRUE) else NA_real_,
      cell_share_ge_10 = if (nrow(cell_counts) > 0) mean(cell_counts$n >= 10, na.rm = TRUE) else NA_real_,
      sample_adequacy_pass = nrow(df_sample) >= 1500 && if (eligible_n > 0) nrow(df_sample) / eligible_n >= 0.10 else FALSE,
      merge_pass = !is.na(merge_coverage) && merge_coverage >= 0.95,
      outcome_pass = !is.na(outcome_non_missing_share) && outcome_non_missing_share >= 0.85,
      cell_pass = nrow(cell_counts) > 0 && cell_median_n >= 20 && cell_share_ge_10 >= 0.75
    )
  })

  full_ambiguity <- comparison %>%
    filter(sample_label == "full_linked") %>%
    pull(exposure_region_ambiguity_share)
  full_ambiguity <- if (length(full_ambiguity) == 0) NA_real_ else full_ambiguity[[1]]

  comparison <- comparison %>%
    mutate(
      ambiguity_reduction_vs_full = full_ambiguity - exposure_region_ambiguity_share,
      ambiguity_pass = sample_label != "full_linked" &
        !is.na(exposure_region_ambiguity_share) &
        exposure_region_ambiguity_share <= 0.15 &
        !is.na(ambiguity_reduction_vs_full) &
        ambiguity_reduction_vs_full >= 0.10,
      overall_pass = sample_adequacy_pass & merge_pass & outcome_pass & cell_pass & ambiguity_pass
    )

  cohort_counts <- purrr::pmap_dfr(sample_specs, function(sample_label, sample_name, sample_description) {
    get_residence_sample_df(analysis_base_df, sample_label) %>%
      count(sample_label = sample_label, survey_year, admin_region, cohort_exposure_group, low_parent_education, name = "n")
  })

  chosen_sample <- dplyr::case_when(
    any(comparison$sample_label == "conservative_likely_stayer" & comparison$overall_pass) ~ "conservative_likely_stayer",
    any(comparison$sample_label == "no_migration_signal" & comparison$overall_pass) ~ "no_migration_signal",
    TRUE ~ NA_character_
  )

  chosen_df <- if (is.na(chosen_sample)) {
    analysis_base_df
  } else {
    get_residence_sample_df(analysis_base_df, chosen_sample)
  }

  list(
    comparison = comparison,
    cohort_counts = cohort_counts,
    chosen_sample = chosen_sample,
    chosen_df = chosen_df,
    outcome_var = outcome_var
  )
}

assess_design_readiness <- function(merged_df, linkage_summary, outcome_checks, preferred_window, preferred_outcome) {
  preferred_linkage <- linkage_summary %>% filter(age_window == preferred_window)
  outcome_row <- outcome_checks %>% filter(age_window == preferred_window, outcome_name == preferred_outcome)
  stability <- build_residence_stability_comparison(merged_df, preferred_window, preferred_outcome)
  comparison <- stability$comparison

  chosen_sample <- stability$chosen_sample
  chosen_row <- if (is.na(chosen_sample)) {
    comparison %>% filter(sample_label == "full_linked")
  } else {
    comparison %>% filter(sample_label == chosen_sample)
  }

  linkage_pass <- nrow(preferred_linkage) > 0 &&
    preferred_linkage$linked_sample_n[[1]] >= 1500 &&
    preferred_linkage$link_rate[[1]] >= 0.15 &&
    preferred_linkage$usable_parent_proxy_share[[1]] >= 0.20

  outcome_pass <- nrow(outcome_row) > 0 &&
    isTRUE(outcome_row$age_appropriate[[1]]) &&
    outcome_row$non_missing_share[[1]] >= 0.85

  any_stability_pass <- any(comparison$overall_pass, na.rm = TRUE)
  overall_pass <- linkage_pass && outcome_pass && any_stability_pass

  candidate_rows <- comparison %>% filter(sample_label != "full_linked")
  binding_constraint <- if (overall_pass) {
    "none"
  } else if (!linkage_pass) {
    "linkage"
  } else if (!outcome_pass) {
    "outcome_maturity"
  } else if (all(!candidate_rows$merge_pass, na.rm = TRUE)) {
    "merge_coverage"
  } else if (all(!candidate_rows$ambiguity_pass, na.rm = TRUE)) {
    "region_exposure_ambiguity"
  } else if (all(!candidate_rows$cell_pass, na.rm = TRUE)) {
    "cohort_cell_size"
  } else if (all(!candidate_rows$sample_adequacy_pass, na.rm = TRUE)) {
    "linkage"
  } else {
    "region_exposure_ambiguity"
  }

  note_lines <- c(
    "# Design Readiness Note",
    "",
    paste0("- Preferred linkage window: `", preferred_window, "`."),
    paste0("- Preferred outcome: `", preferred_outcome, "`."),
    paste0("- Linkage pass at the window level: `", linkage_pass, "`."),
    paste0("- Outcome-maturity pass at the window level: `", outcome_pass, "`."),
    paste0("- Residence-stability samples compared: full linked sample; no migration-signal sample; conservative likely-stayer sample based on explicit residence-history cues."),
    ""
  )

  for (i in seq_len(nrow(comparison))) {
    row <- comparison[i, ]
    note_lines <- c(
      note_lines,
      paste0(
        "- `", row$sample_name, "`: N = ", format(row$sample_n, big.mark = ","),
        "; effective link rate = ", fmt_pct(row$effective_link_rate),
        "; immigrant share = ", fmt_pct(row$immigrant_share),
        "; ambiguity share = ", fmt_pct(row$exposure_region_ambiguity_share),
        "; ambiguity reduction vs full = ", fmt_pct(row$ambiguity_reduction_vs_full),
        "; cell median = ", fmt_num(row$cell_median_n, 1),
        "; cell share >=10 = ", fmt_pct(row$cell_share_ge_10),
        "; overall pass = `", row$overall_pass, "`."
      )
    )
  }

  note_lines <- c(
    note_lines,
    "",
    paste0("- Binding constraint: `", binding_constraint, "`."),
    if (is.na(chosen_sample)) {
      "- No residence-stability restriction rescued the design enough to unlock even Model A."
    } else {
      paste0("- Chosen analytical sample for a gated next step: `", chosen_sample, "`.")
    },
    ""
  )

  if (overall_pass) {
    note_lines <- c(
      note_lines,
      "A residence-stability restriction materially reduced exposure-region ambiguity while preserving enough usable cells. The next unlocked step is Model A only."
    )
  } else {
    note_lines <- c(
      note_lines,
      "The residence-stability pass did not rescue the exposure-region proxy enough to justify estimation. The design note should therefore remain a negative-result audit."
    )
  }

  write_md_lines(file.path(MODULE_PATHS$outputs, "design_readiness_note.md"), note_lines)

  list(
    overall_pass = overall_pass,
    binding_constraint = binding_constraint,
    immigrant_share = chosen_row$immigrant_share[[1]],
    merge_coverage = chosen_row$merge_coverage[[1]],
    cohort_counts = stability$cohort_counts,
    preferred_analysis_df = stability$chosen_df,
    preferred_outcome_var = stability$outcome_var,
    chosen_sample = chosen_sample,
    stability_comparison = comparison
  )
}

build_exposure_gap_metrics <- function(df, outcome_var) {
  if (nrow(df) == 0 || all(is.na(df$exposure_index_17_20))) {
    return(tibble::tibble(
      sample_n = nrow(df),
      quartile_1_rate = NA_real_,
      quartile_4_rate = NA_real_,
      top_bottom_gap = NA_real_,
      top_half_bottom_half_gap = NA_real_
    ))
  }

  exposure_df <- df %>%
    filter(!is.na(exposure_index_17_20)) %>%
    mutate(exposure_bin = dplyr::ntile(exposure_index_17_20, 4L))

  q1_rate <- exposure_df %>%
    filter(exposure_bin == 1L) %>%
    summarise(rate = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight)) %>%
    pull(rate)
  q4_rate <- exposure_df %>%
    filter(exposure_bin == 4L) %>%
    summarise(rate = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight)) %>%
    pull(rate)
  bottom_half_rate <- exposure_df %>%
    filter(exposure_bin %in% c(1L, 2L)) %>%
    summarise(rate = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight)) %>%
    pull(rate)
  top_half_rate <- exposure_df %>%
    filter(exposure_bin %in% c(3L, 4L)) %>%
    summarise(rate = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight)) %>%
    pull(rate)

  tibble::tibble(
    sample_n = nrow(exposure_df),
    quartile_1_rate = if (length(q1_rate) == 0) NA_real_ else q1_rate[[1]],
    quartile_4_rate = if (length(q4_rate) == 0) NA_real_ else q4_rate[[1]],
    top_bottom_gap = ifelse(is.na(q4_rate[[1]]) | is.na(q1_rate[[1]]), NA_real_, q4_rate[[1]] - q1_rate[[1]]),
    top_half_bottom_half_gap = ifelse(is.na(top_half_rate[[1]]) | is.na(bottom_half_rate[[1]]), NA_real_, top_half_rate[[1]] - bottom_half_rate[[1]])
  )
}

pull_scalar_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x[[1]]))
}

write_empty_model_a_review_outputs <- function(note_text = "Model A review gate not run in this pass.") {
  safe_write_csv(
    tibble::tibble(
      check_name = character(),
      metric_summary = character(),
      pass = logical()
    ),
    file.path(MODULE_PATHS$tables, "model_b_gate_status.csv")
  )
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_direction_review.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_region_sensitivity.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_cell_balance_summary.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_cell_counts.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_year_stability.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_a_sample_profile_comparison.csv"))
  write_md_lines(
    file.path(MODULE_PATHS$outputs, "model_a_review_note.md"),
    c(
      "# Model A Review Note",
      "",
      note_text
    )
  )
}

review_model_a_gate <- function(merged_df, preferred_window, preferred_outcome_name, chosen_sample) {
  if (is.na(chosen_sample)) {
    write_empty_model_a_review_outputs("Model A review gate not run because no residence-stability sample passed the earlier readiness gate.")
    return(list(model_b_gate_pass = FALSE, decision = "hold_model_b"))
  }

  outcome_var <- outcome_var_from_name(preferred_outcome_name)
  if (is.na(outcome_var)) {
    write_empty_model_a_review_outputs("Model A review gate not run because the preferred outcome name could not be mapped to an analysis variable.")
    return(list(model_b_gate_pass = FALSE, decision = "hold_model_b"))
  }

  full_linked_complete <- build_analysis_base_df(
    merged_df = merged_df,
    preferred_window = preferred_window,
    outcome_var = outcome_var,
    require_outcome = TRUE
  )
  chosen_complete <- get_residence_sample_df(full_linked_complete, chosen_sample)
  full_linked_prefilter <- build_analysis_base_df(
    merged_df = merged_df,
    preferred_window = preferred_window,
    outcome_var = outcome_var,
    require_outcome = FALSE
  )
  chosen_prefilter <- get_residence_sample_df(full_linked_prefilter, chosen_sample)

  if (nrow(chosen_complete) == 0) {
    write_empty_model_a_review_outputs("Model A review gate not run because the chosen analytical sample had no complete-case observations for the preferred outcome.")
    return(list(model_b_gate_pass = FALSE, decision = "hold_model_b"))
  }

  weights_use <- chosen_complete$sample_weight
  weights_use[is.na(weights_use) | weights_use <= 0] <- 1
  chosen_complete <- chosen_complete %>% mutate(sample_weight = weights_use)

  full_weights <- full_linked_complete$sample_weight
  full_weights[is.na(full_weights) | full_weights <= 0] <- 1
  full_linked_complete <- full_linked_complete %>% mutate(sample_weight = full_weights)

  direction_groups <- list(
    overall = chosen_complete %>% filter(exposed_cohort_any_overlap == 1L),
    lower_parent_education = chosen_complete %>% filter(exposed_cohort_any_overlap == 1L, low_parent_education == 1L),
    higher_parent_education = chosen_complete %>% filter(exposed_cohort_any_overlap == 1L, low_parent_education == 0L),
    full_overlap_only = chosen_complete %>% filter(exposed_cohort_full_overlap == 1L)
  )

  direction_review <- purrr::imap_dfr(direction_groups, function(df_group, review_group) {
    if (nrow(df_group) == 0 || all(is.na(df_group$exposure_index_17_20))) {
      return(tibble::tibble(
        review_group = review_group,
        exposure_bin = integer(),
        sample_n = integer(),
        weighted_mean = numeric()
      ))
    }
    df_group %>%
      mutate(exposure_bin = dplyr::ntile(exposure_index_17_20, 4L)) %>%
      group_by(review_group = review_group, exposure_bin) %>%
      summarise(
        sample_n = n(),
        weighted_mean = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight),
        .groups = "drop"
      )
  })

  overall_gap <- build_exposure_gap_metrics(
    chosen_complete %>% filter(exposed_cohort_any_overlap == 1L),
    outcome_var
  )
  full_overlap_gap <- build_exposure_gap_metrics(
    chosen_complete %>% filter(exposed_cohort_full_overlap == 1L),
    outcome_var
  )

  direction_pass <- !is.na(overall_gap$top_bottom_gap[[1]]) &&
    overall_gap$top_bottom_gap[[1]] >= 0 &&
    !is.na(overall_gap$top_half_bottom_half_gap[[1]]) &&
    overall_gap$top_half_bottom_half_gap[[1]] >= 0 &&
    (is.na(full_overlap_gap$top_bottom_gap[[1]]) || full_overlap_gap$top_bottom_gap[[1]] >= -0.01)

  baseline_sensitivity <- build_exposure_gap_metrics(
    chosen_complete %>% filter(exposed_cohort_any_overlap == 1L),
    outcome_var
  ) %>%
    mutate(excluded_region = "all_regions")

  region_sensitivity <- bind_rows(
    baseline_sensitivity,
    purrr::map_dfr(sort(unique(stats::na.omit(chosen_complete$admin_region))), function(region_name) {
      build_exposure_gap_metrics(
        chosen_complete %>%
          filter(exposed_cohort_any_overlap == 1L, admin_region != region_name),
        outcome_var
      ) %>%
        mutate(excluded_region = region_name)
    })
  ) %>%
    select(excluded_region, everything())

  region_positive_contribution <- chosen_complete %>%
    filter(!is.na(admin_region)) %>%
    group_by(admin_region) %>%
    summarise(
      sample_n = n(),
      region_weight = sum(sample_weight, na.rm = TRUE),
      positive_weight = sum(sample_weight * as.numeric(.data[[outcome_var]] == 1L), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      weighted_sample_share = ifelse(sum(region_weight, na.rm = TRUE) > 0, region_weight / sum(region_weight, na.rm = TRUE), NA_real_),
      weighted_positive_share = ifelse(sum(positive_weight, na.rm = TRUE) > 0, positive_weight / sum(positive_weight, na.rm = TRUE), NA_real_)
    ) %>%
    arrange(desc(weighted_positive_share), desc(sample_n))

  dominant_region <- if (nrow(region_positive_contribution) == 0) NA_character_ else region_positive_contribution$admin_region[[1]]
  dominant_positive_share <- if (nrow(region_positive_contribution) == 0) NA_real_ else region_positive_contribution$weighted_positive_share[[1]]
  target_region <- if ("Tashkent city" %in% region_sensitivity$excluded_region) "Tashkent city" else dominant_region
  target_row <- region_sensitivity %>% filter(excluded_region == target_region)

  region_dominance_pass <- !is.na(dominant_positive_share) &&
    dominant_positive_share <= 0.50 &&
    nrow(target_row) > 0 &&
    !is.na(target_row$top_bottom_gap[[1]]) &&
    target_row$top_bottom_gap[[1]] >= -0.005 &&
    !is.na(target_row$top_half_bottom_half_gap[[1]]) &&
    target_row$top_half_bottom_half_gap[[1]] >= -0.005

  cell_counts <- chosen_complete %>%
    count(admin_region, cohort_exposure_group, low_parent_education, name = "n")

  cell_balance_summary <- tibble::tibble(
    sample_label = chosen_sample,
    sample_n = nrow(chosen_complete),
    cell_n = nrow(cell_counts),
    cell_min_n = if (nrow(cell_counts) > 0) min(cell_counts$n, na.rm = TRUE) else NA_real_,
    cell_p25_n = if (nrow(cell_counts) > 0) as.numeric(stats::quantile(cell_counts$n, probs = 0.25, na.rm = TRUE)) else NA_real_,
    cell_median_n = if (nrow(cell_counts) > 0) median(cell_counts$n, na.rm = TRUE) else NA_real_,
    cell_p75_n = if (nrow(cell_counts) > 0) as.numeric(stats::quantile(cell_counts$n, probs = 0.75, na.rm = TRUE)) else NA_real_,
    cell_max_n = if (nrow(cell_counts) > 0) max(cell_counts$n, na.rm = TRUE) else NA_real_,
    cell_share_ge_5 = if (nrow(cell_counts) > 0) mean(cell_counts$n >= 5, na.rm = TRUE) else NA_real_,
    cell_share_ge_10 = if (nrow(cell_counts) > 0) mean(cell_counts$n >= 10, na.rm = TRUE) else NA_real_,
    cell_share_ge_20 = if (nrow(cell_counts) > 0) mean(cell_counts$n >= 20, na.rm = TRUE) else NA_real_
  )

  cell_balance_pass <- !is.na(cell_balance_summary$cell_median_n[[1]]) &&
    cell_balance_summary$cell_median_n[[1]] >= 20 &&
    !is.na(cell_balance_summary$cell_share_ge_10[[1]]) &&
    cell_balance_summary$cell_share_ge_10[[1]] >= 0.75 &&
    !is.na(cell_balance_summary$cell_share_ge_20[[1]]) &&
    cell_balance_summary$cell_share_ge_20[[1]] >= 0.50

  year_stability <- chosen_prefilter %>%
    group_by(survey_year) %>%
    summarise(
      sample_n = n(),
      outcome_non_missing_share = mean(!is.na(.data[[outcome_var]])),
      outcome_positive_rate = weighted_share_safe(.data[[outcome_var]] == 1L, sample_weight),
      exposed_share = weighted_share_safe(exposed_cohort_any_overlap == 1L, sample_weight),
      .groups = "drop"
    ) %>%
    arrange(survey_year)

  year_rate_range <- if (nrow(year_stability) == 0 || all(is.na(year_stability$outcome_positive_rate))) {
    NA_real_
  } else {
    diff(range(year_stability$outcome_positive_rate, na.rm = TRUE))
  }

  year_stability_pass <- nrow(year_stability) >= 3 &&
    all(year_stability$sample_n >= 100) &&
    all(year_stability$outcome_non_missing_share >= 0.85) &&
    (is.na(year_rate_range) || year_rate_range <= 0.05)

  profile_for_sample <- function(df, sample_label) {
    region_shares <- weighted_group_shares(df, "admin_region")
    tibble::tibble(
      sample_label = sample_label,
      sample_n = nrow(df),
      weighted_mean_age = weighted_mean_safe(df$age, df$sample_weight),
      female_share = weighted_share_safe(df$sex == "female", df$sample_weight),
      urban_share = weighted_share_safe(df$urban == 1L, df$sample_weight),
      tashkent_city_share = region_shares %>% filter(group == "Tashkent city") %>% pull(share) %>% { if (length(.) == 0) 0 else .[[1]] },
      largest_region_share = if (nrow(region_shares) == 0) NA_real_ else max(region_shares$share, na.rm = TRUE)
    )
  }

  sample_profile <- bind_rows(
    profile_for_sample(full_linked_complete, "full_linked"),
    profile_for_sample(chosen_complete, chosen_sample)
  )

  profile_gap <- sample_profile %>%
    select(sample_label, weighted_mean_age, female_share, urban_share, tashkent_city_share, largest_region_share) %>%
    tidyr::pivot_longer(cols = -sample_label, names_to = "metric", values_to = "value") %>%
    tidyr::pivot_wider(names_from = sample_label, values_from = value) %>%
    mutate(abs_gap = abs(.data[[chosen_sample]] - full_linked))

  full_region_shares <- weighted_group_shares(full_linked_complete, "admin_region")
  chosen_region_shares <- weighted_group_shares(chosen_complete, "admin_region")
  region_profile_gap <- full_join(
    full_region_shares,
    chosen_region_shares,
    by = "group",
    suffix = c("_full", "_chosen")
  ) %>%
    mutate(
      share_full = dplyr::coalesce(share_full, 0),
      share_chosen = dplyr::coalesce(share_chosen, 0)
    ) %>%
    mutate(abs_gap = abs(share_chosen - share_full))

  max_region_profile_gap <- if (nrow(region_profile_gap) == 0) NA_real_ else max(region_profile_gap$abs_gap, na.rm = TRUE)
  age_gap_val <- pull_scalar_or_na(profile_gap %>% filter(metric == "weighted_mean_age") %>% pull(abs_gap))
  female_gap_val <- pull_scalar_or_na(profile_gap %>% filter(metric == "female_share") %>% pull(abs_gap))
  urban_gap_val <- pull_scalar_or_na(profile_gap %>% filter(metric == "urban_share") %>% pull(abs_gap))
  tashkent_gap_val <- pull_scalar_or_na(profile_gap %>% filter(metric == "tashkent_city_share") %>% pull(abs_gap))
  min_non_missing_share <- if (nrow(year_stability) == 0) NA_real_ else suppressWarnings(min(year_stability$outcome_non_missing_share, na.rm = TRUE))

  sample_interpretability_pass <- !is.na(age_gap_val) &&
    age_gap_val <= 0.50 &&
    !is.na(female_gap_val) &&
    female_gap_val <= 0.08 &&
    !is.na(urban_gap_val) &&
    urban_gap_val <= 0.10 &&
    !is.na(tashkent_gap_val) &&
    tashkent_gap_val <= 0.10 &&
    !is.na(max_region_profile_gap) &&
    max_region_profile_gap <= 0.10

  gate_status <- tibble::tribble(
    ~check_name, ~metric_summary, ~pass,
    "Direction sensible",
    paste0(
      "Exposed-cohort top vs bottom quartile gap = ", fmt_pct(overall_gap$top_bottom_gap[[1]]),
      "; top-half vs bottom-half gap = ", fmt_pct(overall_gap$top_half_bottom_half_gap[[1]]),
      "; full-overlap top vs bottom gap = ", fmt_pct(full_overlap_gap$top_bottom_gap[[1]])
    ),
    direction_pass,
    "No single-region dominance",
    paste0(
      "Leading positive-contribution region = ", ifelse(is.na(dominant_region), "NA", dominant_region),
      " at ", fmt_pct(dominant_positive_share),
      "; gap after excluding ", ifelse(is.na(target_region), "target region", target_region),
      " = ", fmt_pct(if (nrow(target_row) > 0) target_row$top_bottom_gap[[1]] else NA_real_)
    ),
    region_dominance_pass,
    "Cell balance",
    paste0(
      "Median cell n = ", fmt_num(cell_balance_summary$cell_median_n[[1]], 1),
      "; share >=10 = ", fmt_pct(cell_balance_summary$cell_share_ge_10[[1]]),
      "; share >=20 = ", fmt_pct(cell_balance_summary$cell_share_ge_20[[1]])
    ),
    cell_balance_pass,
    "Year stability",
    paste0(
      "Outcome non-missing share min = ", fmt_pct(min_non_missing_share),
      "; year-to-year rate range = ", fmt_pct(year_rate_range),
      "; years covered = ", nrow(year_stability)
    ),
    year_stability_pass,
    "Sample interpretability",
    paste0(
      "Age gap vs full linked = ", fmt_num(age_gap_val, 2),
      "; female-share gap = ", fmt_pct(female_gap_val),
      "; max region-share gap = ", fmt_pct(max_region_profile_gap)
    ),
    sample_interpretability_pass
  )

  model_b_gate_pass <- all(gate_status$pass)

  safe_write_csv(gate_status, file.path(MODULE_PATHS$tables, "model_b_gate_status.csv"))
  safe_write_csv(direction_review, file.path(MODULE_PATHS$tables, "model_a_direction_review.csv"))
  safe_write_csv(region_sensitivity, file.path(MODULE_PATHS$tables, "model_a_region_sensitivity.csv"))
  safe_write_csv(cell_balance_summary, file.path(MODULE_PATHS$tables, "model_a_cell_balance_summary.csv"))
  safe_write_csv(cell_counts, file.path(MODULE_PATHS$tables, "model_a_cell_counts.csv"))
  safe_write_csv(year_stability, file.path(MODULE_PATHS$tables, "model_a_year_stability.csv"))
  safe_write_csv(sample_profile, file.path(MODULE_PATHS$tables, "model_a_sample_profile_comparison.csv"))

  note_lines <- c(
    "# Model A Review Note",
    "",
    paste0("- Working sample reviewed: `", chosen_sample, "`."),
    paste0("- Preferred linkage window: `", preferred_window, "`."),
    paste0("- Preferred outcome: `", preferred_outcome_name, "`."),
    paste0("- Direction check pass: `", direction_pass, "`."),
    paste0("- Region-dominance check pass: `", region_dominance_pass, "`."),
    paste0("- Cell-balance check pass: `", cell_balance_pass, "`."),
    paste0("- Year-stability check pass: `", year_stability_pass, "`."),
    paste0("- Sample-interpretability check pass: `", isTRUE(sample_interpretability_pass), "`."),
    ""
  )

  if (model_b_gate_pass) {
    note_lines <- c(
      note_lines,
      paste0(
        "Model A looks clean enough on the `", chosen_sample,
        "` sample to unlock Model B as the next gated step. Model B is not estimated in this run; Model C remains deferred."
      )
    )
  } else {
    note_lines <- c(
      note_lines,
      paste0(
        "Model A does not yet clear all five review checks on the `", chosen_sample,
        "` sample. The module should therefore stay at Model A only, with Model B and Model C still deferred."
      )
    )
  }

  write_md_lines(file.path(MODULE_PATHS$outputs, "model_a_review_note.md"), note_lines)

  list(
    model_b_gate_pass = model_b_gate_pass,
    decision = ifelse(model_b_gate_pass, "unlock_model_b_next", "hold_model_b"),
    gate_status = gate_status
  )
}

estimate_exploratory_models <- function(analysis_df, preferred_window, preferred_outcome_var, preferred_outcome_name, chosen_sample) {
  status_path <- file.path(MODULE_PATHS$tables, "model_status.csv")
  formula_path <- file.path(MODULE_PATHS$tables, "model_formulae.csv")
  coef_path <- file.path(MODULE_PATHS$tables, "model_coefficients.csv")
  descriptive_path <- file.path(MODULE_PATHS$tables, "model_descriptive_comparisons.csv")

  if (nrow(analysis_df) < 500) {
    safe_write_csv(
      tibble::tibble(
        model = c("Model A", "Model B", "Model C"),
        estimated = FALSE,
        note = "The preferred residence-stability sample is too small for stable exploratory estimation."
      ),
      status_path
    )
    safe_write_csv(tibble::tibble(model = character(), formula = character()), formula_path)
    safe_write_csv(tibble::tibble(), coef_path)
    safe_write_csv(tibble::tibble(), descriptive_path)
    return(invisible(FALSE))
  }

  weights_use <- analysis_df$sample_weight
  weights_use[is.na(weights_use) | weights_use <= 0] <- 1
  analysis_df <- analysis_df %>% mutate(sample_weight = weights_use)

  descriptive_table <- analysis_df %>%
    mutate(exposure_bin = ntile(exposure_index_17_20, 4)) %>%
    group_by(cohort_exposure_group, exposure_bin, low_parent_education) %>%
    summarise(
      sample_n = n(),
      weighted_mean = weighted_share_safe(.data[[preferred_outcome_var]] == 1L, sample_weight),
      .groups = "drop"
    ) %>%
    mutate(
      preferred_window = preferred_window,
      preferred_outcome = preferred_outcome_name
    )

  status_df <- tibble::tibble(
    model = c("Model A", "Model B", "Model C"),
    estimated = c(TRUE, FALSE, FALSE),
    note = c(
      "Weighted descriptive cohort-exposure comparisons on the rescued residence-stability sample.",
      "Deferred until the residence-stability evidence is strong enough to justify a region x cohort exposure model.",
      "Deferred until the descriptive and Model B evidence justify a parent-background interaction model."
    ),
    preferred_window = preferred_window,
    preferred_outcome = preferred_outcome_name,
    chosen_sample = chosen_sample,
    sample_n = nrow(analysis_df)
  )

  formula_df <- tibble::tibble(
    model = c("Model A", "Model B", "Model C"),
    formula = c(
      paste0(preferred_outcome_var, " by cohort_exposure_group x exposure quartile x low_parent_education"),
      "deferred",
      "deferred"
    )
  )

  safe_write_csv(status_df, status_path)
  safe_write_csv(formula_df, formula_path)
  safe_write_csv(tibble::tibble(), coef_path)
  safe_write_csv(descriptive_table, descriptive_path)
  invisible(TRUE)
}

write_negative_result_tables <- function(binding_constraint, preferred_window, preferred_outcome_name, chosen_sample = NA_character_) {
  safe_write_csv(
    tibble::tibble(
      model = c("Model A", "Model B", "Model C"),
      estimated = FALSE,
      note = paste0("Exploratory estimation suppressed because the design is not ready. Binding constraint: ", binding_constraint, "."),
      preferred_window = preferred_window,
      preferred_outcome = preferred_outcome_name,
      chosen_sample = chosen_sample
    ),
    file.path(MODULE_PATHS$tables, "model_status.csv")
  )
  safe_write_csv(
    tibble::tibble(
      model = c("Model A", "Model B", "Model C"),
      formula = c("not estimated", "not estimated", "not estimated")
    ),
    file.path(MODULE_PATHS$tables, "model_formulae.csv")
  )
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_coefficients.csv"))
  safe_write_csv(tibble::tibble(), file.path(MODULE_PATHS$tables, "model_descriptive_comparisons.csv"))
  write_empty_model_a_review_outputs(
    paste0(
      "Model A review gate not run because the design remained blocked before estimation. Binding constraint: ",
      binding_constraint,
      "."
    )
  )
}

run_hbs_expansion_module <- function() {
  ensure_hbs_expansion_dirs()
  write_progress_log()

  admin_inputs <- list(
    paths = c(
      select_admin_file("uzbekistan_expansion_treatment_panel_final.csv"),
      select_admin_file("uzbekistan_bachelor_access_panel_final.csv"),
      select_admin_file("uzbekistan_he_capacity_panel_final.csv"),
      select_admin_file("uzbekistan_youth_population_20_24_panel.csv"),
      select_admin_file("uzbekistan_expansion_source_registry_final.csv"),
      select_admin_file("uzbekistan_expansion_qa_checks_final.csv"),
      select_admin_file("uzbekistan_university_expansion_final.xlsx")
    )
  )
  admin_panel <- readr::read_csv(select_admin_file("uzbekistan_expansion_treatment_panel_final.csv"), show_col_types = FALSE, progress = FALSE) %>%
    janitor::clean_names() %>%
    mutate(
      admin_region = admin_region_label(geography),
      academic_year_start = as.integer(academic_year_start),
      high_expansion_region_main = as.integer(high_expansion_region_main)
    )

  manifest_bundle <- build_hbs_manifest_inventory()
  safe_write_csv(manifest_bundle$manifest, file.path(MODULE_PATHS$outputs, "hbs_5y_file_manifest.csv"))
  safe_write_csv(manifest_bundle$inventory, file.path(MODULE_PATHS$outputs, "hbs_5y_variable_inventory.csv"))
  record_progress(
    stage = "HBS file audit",
    key_findings = paste0(
      "Detected HBS extracts for survey years ",
      paste(manifest_bundle$years_present, collapse = ", "),
      ". The raw five-year audit has been written to the module manifest and variable inventory."
    ),
    next_action = "Construct the new design-specific person-level harmonized HBS file and linkage diagnostics."
  )

  person_df <- build_expansion_person_harmonized()
  write_harmonization_note(admin_inputs)
  safe_write_csv(person_df, MODULE_PATHS$harmonized_tmp_csv)
  record_progress(
    stage = "Person-level harmonization",
    key_findings = paste0(
      "Built a harmonized HBS person file with ",
      format(nrow(person_df), big.mark = ","),
      " person-year observations and conservative co-resident parent-linkage fields."
    ),
    next_action = "Compute the new younger-cohort linkage diagnostics and failure-mode audit."
  )

  old_pooled_link_rate <- get_old_pooled_link_rate()
  linkage_outputs <- build_linkage_outputs(person_df, old_pooled_link_rate)
  linkage_summary <- linkage_outputs$linkage_summary
  for (window_name in linkage_summary$age_window) {
    safe_write_csv(
      linkage_summary %>% filter(age_window == window_name),
      file.path(MODULE_PATHS$outputs, paste0("linkage_diagnostics_", window_name, ".csv"))
    )
  }
  safe_write_csv(linkage_outputs$linkage_by_year, file.path(MODULE_PATHS$outputs, "linkage_by_year.csv"))
  write_md_lines(file.path(MODULE_PATHS$outputs, "linkage_failure_modes.md"), linkage_outputs$linkage_failure_lines)
  preferred_window <- choose_preferred_window(linkage_summary)
  record_progress(
    stage = "Linkage diagnostics",
    key_findings = paste0(
      "Computed dedicated linkage diagnostics for ages 18-24, 22-30, and 25-35. The preliminary leading window is ",
      preferred_window,
      ", with an old pooled 25-64 comparison rate of ",
      fmt_pct(old_pooled_link_rate),
      "."
    ),
    next_action = "Merge the canonical admin treatment panel and evaluate cohort timing, merge coverage, and outcome readiness."
  )

  merged_bundle <- attach_admin_treatment(person_df, admin_panel)
  merged_df <- merged_bundle$merged
  safe_write_csv(merged_bundle$merge_coverage, file.path(MODULE_PATHS$outputs, "merge_coverage_by_year_region.csv"))
  safe_write_csv(merged_df, MODULE_PATHS$merged_tmp_csv)
  write_cohort_definition_note(admin_panel)

  outcome_checks <- build_outcome_maturity_checks(merged_df)
  safe_write_csv(outcome_checks, file.path(MODULE_PATHS$outputs, "outcome_maturity_checks.csv"))
  preferred_outcome <- choose_preferred_outcome(outcome_checks, preferred_window)

  readiness <- assess_design_readiness(
    merged_df = merged_df,
    linkage_summary = linkage_summary,
    outcome_checks = outcome_checks,
    preferred_window = preferred_window,
    preferred_outcome = preferred_outcome
  )

  safe_write_csv(
    readiness$stability_comparison,
    file.path(MODULE_PATHS$outputs, "residence_stability_sample_comparison.csv")
  )
  safe_write_csv(readiness$cohort_counts, file.path(MODULE_PATHS$outputs, "cohort_counts_by_year_region.csv"))

  record_progress(
    stage = "Residence-stability readiness pass",
    key_findings = paste0(
      "Compared full linked, no migration-signal, and conservative likely-stayer samples for the ",
      preferred_window,
      " window. The binding constraint is ",
      readiness$binding_constraint,
      if (!is.na(readiness$chosen_sample)) {
        paste0(", and the rescued sample is ", readiness$chosen_sample)
      } else {
        "."
      }
    ),
    next_action = "Unlock Model A only if a residence-stability restriction passes the diagnostic gate; otherwise keep the design note negative."
  )

  if (isTRUE(readiness$overall_pass)) {
    estimate_exploratory_models(
      analysis_df = readiness$preferred_analysis_df,
      preferred_window = preferred_window,
      preferred_outcome_var = readiness$preferred_outcome_var,
      preferred_outcome_name = preferred_outcome,
      chosen_sample = readiness$chosen_sample
    )
    model_a_gate <- review_model_a_gate(
      merged_df = merged_df,
      preferred_window = preferred_window,
      preferred_outcome_name = preferred_outcome,
      chosen_sample = readiness$chosen_sample
    )
    record_progress(
      stage = "Model A review gate",
      key_findings = paste0(
        "Reviewed Model A on the rescued sample ",
        readiness$chosen_sample,
        ". Model B unlock decision: ",
        ifelse(model_a_gate$model_b_gate_pass, "yes", "no"),
        "."
      ),
      blockers = ifelse(model_a_gate$model_b_gate_pass, "None at this stage.", "At least one Model A review check still failed."),
      next_action = ifelse(
        model_a_gate$model_b_gate_pass,
        "Keep Model B as the next gated step on the same no-migration-signal sample, but do not estimate it yet.",
        "Hold the module at Model A only until the failed review checks are resolved."
      )
    )
    estimation_note <- if (model_a_gate$model_b_gate_pass) {
      "The residence-stability pass rescued the exposure-region proxy enough to unlock Model A, and the Model A review gate now supports Model B as the next gated step. Model B and Model C were not estimated in this run."
    } else {
      "The residence-stability pass rescued the exposure-region proxy enough to unlock Model A, but the Model A review gate still holds Model B and Model C back."
    }
  } else {
    write_negative_result_tables(
      binding_constraint = readiness$binding_constraint,
      preferred_window = preferred_window,
      preferred_outcome_name = preferred_outcome,
      chosen_sample = readiness$chosen_sample
    )
    estimation_note <- "The residence-stability pass did not rescue the design enough to unlock Model A, so the design note remains a negative-result audit."
  }

  record_progress(
    stage = "Exploratory estimation decision",
    key_findings = estimation_note,
    next_action = "Render the separate HBS expansion design note and verify the final output set."
  )

  invisible(
    list(
      preferred_window = preferred_window,
      preferred_outcome = preferred_outcome,
      readiness = readiness
    )
  )
}

run_hbs_expansion_module()
