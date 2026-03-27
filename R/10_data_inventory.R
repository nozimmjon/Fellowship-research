build_data_inventory_template <- function() {
  tibble::tibble(
    source_id = c("lits_2010", "lits_2016", "lits_2022_23", "hbs", "admin_region_year"),
    source_name = c(
      "Life in Transition Survey 2010",
      "Life in Transition Survey 2016",
      "Life in Transition Survey 2022-23",
      "Household Budget Survey",
      "Administrative Education Panel"
    ),
    owner = c(
      "EBRD / World Bank",
      "EBRD / World Bank",
      "EBRD / World Bank",
      "Uzbekistan Statistics Agency",
      "Education ministries and statistics agency"
    ),
    access_status = "pending",
    time_coverage = c("2010", "2016", "2022-2023", "2010-2024", "2010-2024"),
    geo_coverage = c(
      "national + region",
      "national + region",
      "national + region",
      "national + region",
      "region-year"
    ),
    unit_of_observation = c(
      "individual",
      "individual",
      "individual",
      "household/individual",
      "region-year"
    ),
    key_variables = c(
      "own education; parental education; demographics",
      "own education; parental education; demographics",
      "own education; parental education; demographics",
      "income/consumption; household structure; migration",
      "enrollment; completion; teachers; capacity; spending"
    ),
    expected_file_path = c(
      "data/raw/lits/lits_2010.*",
      "data/raw/lits/lits_2016.*",
      "data/raw/lits/lits_2022_23.*",
      "data/raw/hbs/",
      "data/raw/admin/region_year_policy_panel.*"
    ),
    notes = NA_character_
  )
}

build_variable_dictionary_template <- function() {
  tibble::tibble(
    dataset = c("lits", "lits", "lits", "lits", "lits", "admin"),
    raw_variable = c(
      "respondent_education",
      "parent_education",
      "respondent_age",
      "region",
      "wave_year",
      "treatment_intensity"
    ),
    standard_variable = c(
      "own_ed_level",
      "parent_ed_level",
      "age",
      "region",
      "wave_year",
      "treatment_intensity"
    ),
    type = c("character", "character", "integer", "character", "integer", "double"),
    description = c(
      "Highest completed education of respondent",
      "Highest completed education of parent",
      "Respondent age in years",
      "Administrative region",
      "Survey year",
      "Region-year reform intensity measure"
    ),
    units_or_levels = c(
      "ordered levels",
      "ordered levels",
      "years",
      "official labels",
      "year",
      "index"
    ),
    required = c("yes", "yes", "yes", "yes", "yes", "yes"),
    module = c("A|B", "A|B", "A|B", "A|B|C", "A|B|C", "C")
  )
}

write_data_inventory_template <- function(
  path = file.path(PROJ_PATHS$metadata, "data_inventory_template.csv"),
  overwrite = FALSE
) {
  ensure_project_dirs()
  if (file.exists(path) && !overwrite) {
    return(path)
  }
  readr::write_csv(build_data_inventory_template(), path)
  path
}

write_variable_dictionary_template <- function(
  path = file.path(PROJ_PATHS$metadata, "variable_dictionary_template.csv"),
  overwrite = FALSE
) {
  ensure_project_dirs()
  if (file.exists(path) && !overwrite) {
    return(path)
  }
  readr::write_csv(build_variable_dictionary_template(), path)
  path
}
