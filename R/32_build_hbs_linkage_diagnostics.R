hbs_parent_role_fields <- function(adult_df) {
  mother_years <- dplyr::coalesce(adult_df$head_mother_edu_years, adult_df$spouse_mother_edu_years)
  father_years <- dplyr::coalesce(adult_df$head_father_edu_years, adult_df$spouse_father_edu_years)
  proxy_years <- dplyr::coalesce(mother_years, father_years)

  list(
    mother_years = mother_years,
    father_years = father_years,
    proxy_years = proxy_years
  )
}

hbs_mean_weighted <- function(x, w, mask = NULL) {
  if (is.null(mask)) {
    mask <- rep(TRUE, length(x))
  }
  keep <- mask & !is.na(x) & !is.na(w)
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep])
}

hbs_build_adult_person_file <- function(folder) {
  edu_path <- hbs_find_module_path(folder, "m03_education\\.dta$")
  if (is.na(edu_path)) {
    return(tibble::tibble())
  }

  weight_path <- hbs_find_module_path(folder, "m00_weight\\.dta$|M00_weight\\.dta$")
  edu <- haven::read_dta(edu_path)
  weights <- if (!is.na(weight_path)) haven::read_dta(weight_path) else tibble::tibble()

  weight_df <- tibble::tibble(
    year = hbs_extract_year(edu_path),
    hhid = as.character(hbs_col_or_na(weights, "hhid")),
    province = hbs_normalize_province(hbs_col_or_na(weights, "province")),
    urban = coerce_urban_binary(as_label_text(hbs_col_or_na(weights, "urban"))),
    sample_weight = dplyr::coalesce(
      hbs_numeric(hbs_col_or_na(weights, "indw")),
      hbs_numeric(hbs_col_or_na(weights, "uwgt")),
      hbs_numeric(hbs_col_or_na(weights, "popw"))
    )
  ) %>%
    dplyr::distinct(year, hhid, .keep_all = TRUE)

  person_df <- tibble::tibble(
    year = hbs_extract_year(edu_path),
    hhid = as.character(hbs_col_or_na(edu, "hhid")),
    person_id = dplyr::coalesce(as.character(hbs_col_or_na(edu, "fmid")), as.character(hbs_col_or_na(edu, "sid")), as.character(hbs_col_or_na(edu, "iid"))),
    age = hbs_numeric(hbs_col_or_na(edu, "age")),
    gender = hbs_gender_label(hbs_col_or_na(edu, "gender")),
    relationship_code = suppressWarnings(as.integer(as.numeric(hbs_col_or_na(edu, "relationship")))),
    own_education_years = hbs_map_education_years(hbs_col_or_na(edu, "edu_years"), hbs_col_or_na(edu, "edu_highest"))
  ) %>%
    dplyr::left_join(weight_df, by = c("year", "hhid"))

  parent_summary <- person_df %>%
    dplyr::filter(relationship_code %in% c(1L, 2L)) %>%
    dplyr::mutate(
      head_age = dplyr::if_else(relationship_code == 1L, age, NA_real_),
      spouse_age = dplyr::if_else(relationship_code == 2L, age, NA_real_),
      head_mother_edu_years = dplyr::if_else(relationship_code == 1L & gender == "female", own_education_years, NA_real_),
      spouse_mother_edu_years = dplyr::if_else(relationship_code == 2L & gender == "female", own_education_years, NA_real_),
      head_father_edu_years = dplyr::if_else(relationship_code == 1L & gender == "male", own_education_years, NA_real_),
      spouse_father_edu_years = dplyr::if_else(relationship_code == 2L & gender == "male", own_education_years, NA_real_)
    ) %>%
    dplyr::group_by(year, hhid) %>%
    dplyr::summarise(
      head_age = suppressWarnings(max(head_age, na.rm = TRUE)),
      spouse_age = suppressWarnings(max(spouse_age, na.rm = TRUE)),
      head_mother_edu_years = suppressWarnings(max(head_mother_edu_years, na.rm = TRUE)),
      spouse_mother_edu_years = suppressWarnings(max(spouse_mother_edu_years, na.rm = TRUE)),
      head_father_edu_years = suppressWarnings(max(head_father_edu_years, na.rm = TRUE)),
      spouse_father_edu_years = suppressWarnings(max(spouse_father_edu_years, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(head_age, spouse_age, head_mother_edu_years, spouse_mother_edu_years, head_father_edu_years, spouse_father_edu_years),
        ~ dplyr::if_else(is.infinite(.x), NA_real_, .x)
      )
    )

  person_df %>%
    dplyr::left_join(parent_summary, by = c("year", "hhid")) %>%
    dplyr::filter(age >= ANALYSIS_SAMPLE$age_min, age <= ANALYSIS_SAMPLE$age_max) %>%
    dplyr::mutate(cohort = assign_cohort(age))
}

build_hbs_linkage_diagnostics <- function(raw_dir = file.path(PROJ_PATHS$raw_data, "hbs")) {
  ensure_project_dirs()
  folders <- hbs_year_folders(raw_dir)
  if (length(folders) == 0) {
    return(list(
      diagnostics = tibble::tibble(),
      linked_adult_sample = tibble::tibble(),
      linkage_overview = tibble::tibble(),
      linkage_selection = tibble::tibble(),
      linkage_parent_measure_availability = tibble::tibble()
    ))
  }

  adult_sample <- purrr::map_dfr(folders, hbs_build_adult_person_file)
  if (nrow(adult_sample) == 0) {
    return(list(
      diagnostics = tibble::tibble(),
      linked_adult_sample = tibble::tibble(),
      linkage_overview = tibble::tibble(),
      linkage_selection = tibble::tibble(),
      linkage_parent_measure_availability = tibble::tibble()
    ))
  }

  parent_fields <- hbs_parent_role_fields(adult_sample)
  adult_sample <- adult_sample %>%
    dplyr::mutate(
      mother_education_years = dplyr::if_else(
        !is.na(parent_fields$mother_years) &
          (
            (!is.na(head_age) & head_age >= age + 12 & !is.na(head_mother_edu_years)) |
              (!is.na(spouse_age) & spouse_age >= age + 12 & !is.na(spouse_mother_edu_years))
          ),
        parent_fields$mother_years,
        NA_real_
      ),
      father_education_years = dplyr::if_else(
        !is.na(parent_fields$father_years) &
          (
            (!is.na(head_age) & head_age >= age + 12 & !is.na(head_father_edu_years)) |
              (!is.na(spouse_age) & spouse_age >= age + 12 & !is.na(spouse_father_edu_years))
          ),
        parent_fields$father_years,
        NA_real_
      )
    ) %>%
    dplyr::mutate(
      parent_proxy_years = dplyr::case_when(
        !is.na(mother_education_years) & !is.na(father_education_years) ~ pmax(mother_education_years, father_education_years),
        !is.na(mother_education_years) ~ mother_education_years,
        !is.na(father_education_years) ~ father_education_years,
        TRUE ~ NA_real_
      ),
      coresident_child_of_head = as.integer(relationship_code == 3L),
      linked_under_rule = as.integer(coresident_child_of_head == 1L & !is.na(parent_proxy_years)),
      female = as.integer(gender == "female"),
      parent_measure_category = dplyr::case_when(
        !is.na(mother_education_years) & !is.na(father_education_years) ~ "both parents",
        !is.na(mother_education_years) & is.na(father_education_years) ~ "mother only",
        is.na(mother_education_years) & !is.na(father_education_years) ~ "father only",
        coresident_child_of_head == 1L & !is.na(parent_proxy_years) ~ "proxy only",
        TRUE ~ "no usable parent measure"
      )
    )

  linked_sample <- adult_sample %>%
    dplyr::filter(linked_under_rule == 1L) %>%
    dplyr::arrange(year, province, hhid, person_id)

  adult_n <- nrow(adult_sample)
  linked_n <- nrow(linked_sample)
  usable_parent_n <- sum(!is.na(adult_sample$parent_proxy_years))
  link_rate <- if (adult_n > 0) linked_n / adult_n else NA_real_
  usable_parent_share <- if (adult_n > 0) usable_parent_n / adult_n else NA_real_

  mean_age_full <- hbs_mean_weighted(adult_sample$age, adult_sample$sample_weight)
  mean_age_linked <- hbs_mean_weighted(linked_sample$age, linked_sample$sample_weight)
  female_full <- hbs_weighted_share(adult_sample$female, adult_sample$sample_weight)
  female_linked <- hbs_weighted_share(linked_sample$female, linked_sample$sample_weight)
  urban_full <- hbs_weighted_share(adult_sample$urban, adult_sample$sample_weight)
  urban_linked <- hbs_weighted_share(linked_sample$urban, linked_sample$sample_weight)

  age_gap <- abs(mean_age_linked - mean_age_full)
  female_gap <- abs(female_linked - female_full)
  urban_gap <- abs(urban_linked - urban_full)

  diagnostics <- tibble::tibble(
    sample = "pooled_2021_2025",
    linkage_rule = "Adult aged 25-64 is linked only if coded as a co-resident child of the household head and at least one parental-generation education measure is available from the head/spouse structure with a 12-year minimum age gap.",
    adult_sample_n = adult_n,
    linked_sample_n = linked_n,
    link_rate = link_rate,
    usable_parent_measure_n = usable_parent_n,
    usable_parent_measure_share = usable_parent_share,
    mean_age_full = mean_age_full,
    mean_age_linked = mean_age_linked,
    female_share_full = female_full,
    female_share_linked = female_linked,
    urban_share_full = urban_full,
    urban_share_linked = urban_linked,
    age_gap = age_gap,
    female_gap = female_gap,
    urban_gap = urban_gap,
    linked_n_pass = linked_n >= 1500,
    link_rate_pass = !is.na(link_rate) & link_rate >= 0.20,
    parent_measure_pass = !is.na(usable_parent_share) & usable_parent_share >= 0.20,
    selection_balance_pass = !is.na(age_gap) & age_gap <= 5 &
      !is.na(female_gap) & female_gap <= 0.10 &
      !is.na(urban_gap) & urban_gap <= 0.10,
    model_justified = linked_n >= 1500 &
      !is.na(link_rate) & link_rate >= 0.20 &
      !is.na(usable_parent_share) & usable_parent_share >= 0.20 &
      !is.na(age_gap) & age_gap <= 5 &
      !is.na(female_gap) & female_gap <= 0.10 &
      !is.na(urban_gap) & urban_gap <= 0.10
  )

  linkage_overview <- tibble::tibble(
    metric = c(
      "Adult sample size (ages 25-64)",
      "Linked co-resident sample size",
      "Percent linkable under implemented rule",
      "Adults with usable parent education/proxy",
      "Share with usable parent education/proxy"
    ),
    value = c(
      adult_n,
      linked_n,
      link_rate,
      usable_parent_n,
      usable_parent_share
    )
  )

  linkage_selection <- dplyr::bind_rows(
    tibble::tibble(
      metric = c("Mean age", "Female share", "Urban share"),
      group = c("overall", "overall", "overall"),
      full_sample = c(mean_age_full, female_full, urban_full),
      linked_sample = c(mean_age_linked, female_linked, urban_linked)
    ),
    dplyr::full_join(
      adult_sample %>%
        dplyr::group_by(province) %>%
        dplyr::summarise(
          full_sample = sum(sample_weight, na.rm = TRUE) / sum(adult_sample$sample_weight, na.rm = TRUE),
          .groups = "drop"
        ),
      linked_sample %>%
        dplyr::group_by(province) %>%
        dplyr::summarise(
          linked_sample = sum(sample_weight, na.rm = TRUE) / sum(linked_sample$sample_weight, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "province"
    ) %>%
      dplyr::transmute(
        metric = "Province share",
        group = province,
        full_sample = full_sample,
        linked_sample = linked_sample
      )
  )

  parent_availability <- adult_sample %>%
    dplyr::count(parent_measure_category, name = "n") %>%
    dplyr::mutate(share = n / sum(n)) %>%
    dplyr::arrange(match(parent_measure_category, c("both parents", "mother only", "father only", "proxy only", "no usable parent measure")))

  list(
    diagnostics = diagnostics,
    linked_adult_sample = linked_sample,
    linkage_overview = linkage_overview,
    linkage_selection = linkage_selection,
    linkage_parent_measure_availability = parent_availability
  )
}

write_hbs_linkage_outputs <- function(
  linkage_results,
  diagnostics_path = file.path(PROJ_PATHS$processed_data, "hbs_linkage_diagnostics.csv"),
  linked_sample_path = file.path(PROJ_PATHS$processed_data, "hbs_linked_adult_sample.csv"),
  overview_path = file.path(PROJ_PATHS$tables, "hbs_linkage_overview.csv"),
  selection_path = file.path(PROJ_PATHS$tables, "hbs_linkage_selection.csv"),
  parent_path = file.path(PROJ_PATHS$tables, "hbs_linkage_parent_measure_availability.csv")
) {
  diagnostics_df <- linkage_results$diagnostics
  linked_df <- linkage_results$linked_adult_sample
  overview_df <- linkage_results$linkage_overview
  selection_df <- linkage_results$linkage_selection
  parent_df <- linkage_results$linkage_parent_measure_availability

  if (is.null(diagnostics_df)) diagnostics_df <- tibble::tibble()
  if (is.null(linked_df)) linked_df <- tibble::tibble()
  if (is.null(overview_df)) overview_df <- tibble::tibble()
  if (is.null(selection_df)) selection_df <- tibble::tibble()
  if (is.null(parent_df)) parent_df <- tibble::tibble()

  safe_write_csv(diagnostics_df, diagnostics_path)
  safe_write_csv(linked_df, linked_sample_path)
  safe_write_csv(overview_df, overview_path)
  safe_write_csv(selection_df, selection_path)
  safe_write_csv(parent_df, parent_path)

  c(diagnostics_path, linked_sample_path, overview_path, selection_path, parent_path)
}
