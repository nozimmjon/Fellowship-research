select_existing_file <- function(candidates) {
  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0) {
    return(NA_character_)
  }
  hits[[1]]
}

read_any_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    return(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))
  }
  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path))
  }
  if (ext == "dta") {
    return(haven::read_dta(path))
  }
  if (ext == "sav") {
    return(haven::read_sav(path))
  }
  stop("Unsupported file type: ", ext)
}

infer_wave_from_filename <- function(path) {
  nm <- tolower(basename(path))
  dplyr::case_when(
    stringr::str_detect(nm, "lits2|lits_ii") ~ 2010L,
    stringr::str_detect(nm, "lits_iii") ~ 2016L,
    stringr::str_detect(nm, "lits_iv|2022|2023") ~ 2022L,
    stringr::str_detect(nm, "2010") ~ 2010L,
    stringr::str_detect(nm, "2016") ~ 2016L,
    TRUE ~ NA_integer_
  )
}

clean_numeric <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[out < 0] <- NA_real_
  out
}

as_label_text <- function(x) {
  if (inherits(x, "haven_labelled")) {
    return(as.character(haven::as_factor(x)))
  }
  as.character(x)
}

map_education_level <- function(x) {
  x <- tolower(as.character(x))
  dplyr::case_when(
    stringr::str_detect(x, "master|phd|bachelor|tertiary education \\(not a university") ~ "tertiary",
    stringr::str_detect(x, "post-secondary non-tertiary|post-secondary non tertiary") ~ "post_secondary_non_tertiary",
    stringr::str_detect(x, "upper") ~ "upper_secondary",
    stringr::str_detect(x, "lower") ~ "lower_secondary",
    stringr::str_detect(x, "primary") ~ "primary",
    stringr::str_detect(x, "no degree|no education|no formal") ~ "no_formal",
    TRUE ~ NA_character_
  )
}

education_level_to_years <- function(level) {
  dplyr::case_when(
    level == "no_formal" ~ 0,
    level == "primary" ~ 6,
    level == "lower_secondary" ~ 9,
    level == "upper_secondary" ~ 11,
    level == "post_secondary_non_tertiary" ~ 13,
    level == "tertiary" ~ 16,
    TRUE ~ NA_real_
  )
}

years_to_education_level <- function(years) {
  y <- suppressWarnings(as.numeric(years))
  dplyr::case_when(
    is.na(y) ~ NA_character_,
    y < 1 ~ "no_formal",
    y < 7 ~ "primary",
    y < 10 ~ "lower_secondary",
    y < 13 ~ "upper_secondary",
    y < 15 ~ "post_secondary_non_tertiary",
    TRUE ~ "tertiary"
  )
}

parent_max_level <- function(father_level, mother_level) {
  f_idx <- match(father_level, EDUCATION_LEVELS)
  m_idx <- match(mother_level, EDUCATION_LEVELS)
  idx <- pmax(
    dplyr::if_else(is.na(f_idx), -Inf, as.numeric(f_idx)),
    dplyr::if_else(is.na(m_idx), -Inf, as.numeric(m_idx))
  )
  out <- rep(NA_character_, length(idx))
  valid <- is.finite(idx)
  out[valid] <- EDUCATION_LEVELS[idx[valid]]
  out
}

coerce_urban_binary <- function(x) {
  x <- tolower(as.character(x))
  dplyr::case_when(
    x %in% c("urban", "1", "1 [urban]", "1 [urbanity status]") ~ 1L,
    x %in% c("rural", "2", "2 [rural]", "2 [rurality status]") ~ 0L,
    stringr::str_detect(x, "urban") ~ 1L,
    stringr::str_detect(x, "rural") ~ 0L,
    TRUE ~ NA_integer_
  )
}

coerce_gender_label <- function(x) {
  x <- tolower(as.character(x))
  dplyr::case_when(
    x %in% c("1", "male", "1 [male]") ~ "male",
    x %in% c("2", "female", "2 [female]") ~ "female",
    stringr::str_detect(x, "male") ~ "male",
    stringr::str_detect(x, "female") ~ "female",
    TRUE ~ NA_character_
  )
}

yes_no_to_binary <- function(x) {
  x <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    x == "yes" ~ 1L,
    x == "no" ~ 0L,
    TRUE ~ NA_integer_
  )
}

build_multigenerational_proxy <- function(df, age_cols, child_max_age = 17, older_min_age = 60) {
  age_cols <- intersect(age_cols, names(df))
  if (length(age_cols) == 0) {
    return(rep(NA_integer_, nrow(df)))
  }

  age_df <- df %>%
    dplyr::select(dplyr::all_of(age_cols)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), clean_numeric))

  age_mat <- as.matrix(age_df)
  has_any_age <- rowSums(!is.na(age_mat)) > 0
  has_child <- rowSums(!is.na(age_mat) & age_mat <= child_max_age) > 0
  has_older <- rowSums(!is.na(age_mat) & age_mat >= older_min_age) > 0

  out <- ifelse(has_any_age, as.integer(has_child & has_older), NA_integer_)
  as.integer(out)
}

age_from_band <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "18-24" ~ 21,
    x == "25-34" ~ 30,
    x == "35-44" ~ 40,
    x == "45-54" ~ 50,
    x == "55-64" ~ 60,
    x == "65+" ~ 68,
    TRUE ~ NA_real_
  )
}

assign_cohort <- function(age) {
  dplyr::case_when(
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 44 ~ "35-44",
    age >= 45 & age <= 54 ~ "45-54",
    age >= 55 & age <= 64 ~ "55-64",
    TRUE ~ NA_character_
  )
}

as_harmonized_frame <- function(
  wave_year,
  country,
  region,
  age,
  gender,
  urban,
  own_level,
  parent_level,
  own_years,
  parent_years,
  weight,
  hh_income_proxy = NULL,
  migration_exposure = NULL,
  multigenerational_hh = NULL
) {
  n <- length(wave_year)
  if (is.null(hh_income_proxy)) hh_income_proxy <- rep(NA_real_, n)
  if (is.null(migration_exposure)) migration_exposure <- rep(NA_integer_, n)
  if (is.null(multigenerational_hh)) multigenerational_hh <- rep(NA_integer_, n)

  tibble::tibble(
    wave_year = as.integer(wave_year),
    country = as.character(country),
    region = as.character(region),
    age = clean_numeric(age),
    gender = coerce_gender_label(gender),
    urban = coerce_urban_binary(urban),
    own_ed_level = map_education_level(own_level),
    parent_ed_level = map_education_level(parent_level),
    own_years_schooling = clean_numeric(own_years),
    parent_years_schooling = clean_numeric(parent_years),
    sample_weight = clean_numeric(weight),
    hh_income_proxy = clean_numeric(hh_income_proxy),
    migration_exposure = suppressWarnings(as.integer(migration_exposure)),
    multigenerational_hh = suppressWarnings(as.integer(multigenerational_hh))
  ) %>%
    dplyr::mutate(
      cohort = assign_cohort(age)
    )
}

read_lits_2010 <- function(raw_dir) {
  path <- select_existing_file(c(
    file.path(raw_dir, "lits_ii.csv"),
    file.path(raw_dir, "lits2.dta")
  ))
  if (is.na(path)) {
    return(tibble::tibble())
  }

  # LiTS II DTA in this workspace is unreadable for row data, so CSV is primary.
  if (tolower(tools::file_ext(path)) != "csv") {
    return(tibble::tibble())
  }

  d <- readr::read_csv(
    path,
    col_select = c("country", "Region1", "Region2", "respondentage", "Select_0", "respondentgender", "tablec", "q515", "q718", "q719", "q708", "q104a_1", "q104a_2", "q104a_3", "q104a_4", "q104a_5", "q104a_6", "q104a_7", "q104a_8", "q104a_9", "q104a_10", "q104a_11", "q104a_12", "weight"),
    show_col_types = FALSE,
    progress = FALSE
  )

  d <- d %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(country), "uzbek")) %>%
    dplyr::mutate(
      age_final = dplyr::coalesce(clean_numeric(respondentage), age_from_band(Select_0)),
      region_final = dplyr::coalesce(Region2, Region1),
      migration = yes_no_to_binary(q708),
      own_level = map_education_level(q515),
      own_years = education_level_to_years(own_level),
      father_years = clean_numeric(q718),
      mother_years = clean_numeric(q719),
      father_level = years_to_education_level(father_years),
      mother_level = years_to_education_level(mother_years),
      parent_level = parent_max_level(father_level, mother_level),
      parent_years = education_level_to_years(parent_level),
      multigen_proxy = build_multigenerational_proxy(
        .,
        age_cols = c("q104a_1", "q104a_2", "q104a_3", "q104a_4", "q104a_5", "q104a_6", "q104a_7", "q104a_8", "q104a_9", "q104a_10", "q104a_11", "q104a_12")
      )
    )

  as_harmonized_frame(
    wave_year = 2010L,
    country = d$country,
    region = d$region_final,
    age = d$age_final,
    gender = d$respondentgender,
    urban = d$tablec,
    own_level = d$own_level,
    parent_level = d$parent_level,
    own_years = d$own_years,
    parent_years = d$parent_years,
    weight = d$weight,
    hh_income_proxy = NA_real_,
    migration_exposure = d$migration,
    multigenerational_hh = d$multigen_proxy
  )
}

read_lits_2016 <- function(raw_dir) {
  path <- select_existing_file(c(
    file.path(raw_dir, "lits_iii.dta"),
    file.path(raw_dir, "lits_iii.csv")
  ))
  if (is.na(path)) {
    return(tibble::tibble())
  }

  if (tolower(tools::file_ext(path)) == "dta") {
      d <- haven::read_dta(
      path,
      col_select = c(country, region_name, age_pr, gender_pr, urban, q109_1, q110_1, q111_1, q223, q912, q105_1, q105_2, q105_3, q105_4, q105_5, q105_6, q105_7, q105_8, q105_9, q105_10, weight_population)
    ) %>%
      dplyr::mutate(
        country = as.character(country),
        own_cat = as_label_text(q109_1),
        father_cat = as_label_text(q110_1),
        mother_cat = as_label_text(q111_1),
        migration = yes_no_to_binary(as_label_text(q912)),
        hh_income_raw = clean_numeric(q223),
        gender = as_label_text(gender_pr),
        urban_raw = as_label_text(urban),
        multigen_proxy = build_multigenerational_proxy(
          .,
          age_cols = c("q105_1", "q105_2", "q105_3", "q105_4", "q105_5", "q105_6", "q105_7", "q105_8", "q105_9", "q105_10")
        )
      ) %>%
      dplyr::filter(country == "Uzbekistan")
  } else {
    d <- readr::read_csv(
      path,
      col_select = c("country", "region_name", "age_pr", "gender_pr", "urban", "q109_1", "q110_1", "q111_1", "q223", "q912", "q105_1", "q105_2", "q105_3", "q105_4", "q105_5", "q105_6", "q105_7", "q105_8", "q105_9", "q105_10", "weight_population"),
      show_col_types = FALSE,
      progress = FALSE
    ) %>%
      dplyr::mutate(
        own_cat = as.character(q109_1),
        father_cat = as.character(q110_1),
        mother_cat = as.character(q111_1),
        migration = yes_no_to_binary(q912),
        hh_income_raw = clean_numeric(q223),
        gender = as.character(gender_pr),
        urban_raw = as.character(urban),
        multigen_proxy = build_multigenerational_proxy(
          .,
          age_cols = c("q105_1", "q105_2", "q105_3", "q105_4", "q105_5", "q105_6", "q105_7", "q105_8", "q105_9", "q105_10")
        )
      ) %>%
      dplyr::filter(country == "Uzbekistan")
  }

  d <- d %>%
    dplyr::mutate(
      own_level = map_education_level(own_cat),
      father_level = map_education_level(father_cat),
      mother_level = map_education_level(mother_cat),
      own_years = education_level_to_years(own_level),
      parent_level = parent_max_level(father_level, mother_level),
      parent_years = education_level_to_years(parent_level)
    )

  as_harmonized_frame(
    wave_year = 2016L,
    country = d$country,
    region = d$region_name,
    age = d$age_pr,
    gender = d$gender,
    urban = d$urban_raw,
    own_level = d$own_level,
    parent_level = d$parent_level,
    own_years = d$own_years,
    parent_years = d$parent_years,
    weight = d$weight_population,
    hh_income_proxy = d$hh_income_raw,
    migration_exposure = d$migration,
    multigenerational_hh = d$multigen_proxy
  )
}

read_lits_2022 <- function(raw_dir) {
  path <- select_existing_file(c(
    file.path(raw_dir, "lits_iv_dta", "lits_iv.dta")
  ))
  if (is.na(path)) {
    return(tibble::tibble())
  }

  d <- haven::read_dta(
    path,
    col_select = c(country, region, q1051, q1052, q1053, q1054, q1055, q1056, q1057, q1058, q1059, q10510, q10511, q10512, q10513, q10514, q10515, q10516, q10517, q10518, q10519, q10520, int_gender, urbanity, q109a, q109b, q110a, q110b, q111a, q111b, q225, q505, weight_pop, weight)
  ) %>%
    dplyr::mutate(
      country = as.character(haven::as_factor(country)),
      own_cat = dplyr::coalesce(as_label_text(q109b), as_label_text(q109a)),
      father_cat = dplyr::coalesce(as_label_text(q110b), as_label_text(q110a)),
      mother_cat = dplyr::coalesce(as_label_text(q111b), as_label_text(q111a)),
      migration = yes_no_to_binary(as_label_text(q505)),
      hh_income_raw = clean_numeric(q225),
      gender = as_label_text(int_gender),
      urban_raw = as_label_text(urbanity),
      weight_final = dplyr::coalesce(weight_pop, weight),
      multigen_proxy = build_multigenerational_proxy(
        .,
        age_cols = c("q1051", "q1052", "q1053", "q1054", "q1055", "q1056", "q1057", "q1058", "q1059", "q10510", "q10511", "q10512", "q10513", "q10514", "q10515", "q10516", "q10517", "q10518", "q10519", "q10520")
      )
    ) %>%
    dplyr::filter(country == "Uzbekistan") %>%
    dplyr::mutate(
      own_level = map_education_level(own_cat),
      father_level = map_education_level(father_cat),
      mother_level = map_education_level(mother_cat),
      own_years = education_level_to_years(own_level),
      parent_level = parent_max_level(father_level, mother_level),
      parent_years = education_level_to_years(parent_level)
    )

  as_harmonized_frame(
    wave_year = 2022L,
    country = d$country,
    region = d$region,
    age = d$q1051,
    gender = d$gender,
    urban = d$urban_raw,
    own_level = d$own_level,
    parent_level = d$parent_level,
    own_years = d$own_years,
    parent_years = d$parent_years,
    weight = d$weight_final,
    hh_income_proxy = d$hh_income_raw,
    migration_exposure = d$migration,
    multigenerational_hh = d$multigen_proxy
  )
}

build_lits_harmonized <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "lits")) {
  ensure_project_dirs()

  all_waves <- dplyr::bind_rows(
    read_lits_2010(raw_dir),
    read_lits_2016(raw_dir),
    read_lits_2022(raw_dir)
  ) %>%
    dplyr::filter(age >= ANALYSIS_SAMPLE$age_min, age <= ANALYSIS_SAMPLE$age_max)

  all_waves
}

write_lits_harmonized <- function(
  df,
  path = file.path(PROJ_PATHS$processed_data, "lits_harmonized.csv")
) {
  ensure_project_dirs()
  readr::write_csv(df, path)
  path
}

build_policy_panel <- function(
  raw_dir = file.path(PROJ_PATHS$raw_data, "admin")
) {
  ensure_project_dirs()
  files <- list.files(raw_dir, pattern = "region_year_policy_panel\\.(csv|xlsx|xls)$", full.names = TRUE)
  if (length(files) == 0) {
    message("No policy panel found. Returning an empty tibble.")
    return(tibble::tibble())
  }
  panel <- read_any_table(files[1]) %>% janitor::clean_names()
  panel
}
