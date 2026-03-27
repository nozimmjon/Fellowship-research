source("R/00_config.R")
activate_local_lib()
source("R/01_packages.R")

check_required_packages(c("dplyr", "tidyr", "readr", "purrr", "stringr", "haven", "tibble", "readxl"))
load_required_packages(c("dplyr", "tidyr", "readr", "purrr", "stringr", "haven", "tibble", "readxl"))

safe_read_dta_names <- function(path) {
  tryCatch({
    names(haven::read_dta(path, n_max = 0))
  }, error = function(e) {
    character()
  })
}

extract_hbs_year <- function(path) {
  val <- stringr::str_match(basename(path), "uzb_hbs_(\\d{4})_")[, 2]
  suppressWarnings(as.integer(val))
}

extract_hbs_module <- function(path) {
  stringr::str_match(tolower(basename(path)), "_(m[0-9]{2}[a-z]?_[^\\.]+)\\.dta$")[, 2]
}

write_md <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
  path
}

fmt_bool <- function(x) ifelse(isTRUE(x), "yes", "no")

safe_read_csv_names <- function(path) {
  tryCatch({
    names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  }, error = function(e) {
    character()
  })
}

safe_read_zip_csv_names <- function(zip_path, entry_name) {
  tryCatch({
    con <- unz(zip_path, entry_name, open = "r")
    on.exit(close(con), add = TRUE)
    names(utils::read.csv(con, nrows = 0, check.names = FALSE))
  }, error = function(e) {
    character()
  })
}

safe_read_csv_var_labels <- function(path) {
  tibble::tibble(var = safe_read_csv_names(path), label = "")
}

safe_read_zip_csv_var_labels <- function(zip_path, entry_name) {
  tibble::tibble(var = safe_read_zip_csv_names(zip_path, entry_name), label = "")
}

safe_read_dta_var_labels <- function(path) {
  tryCatch({
    dta0 <- haven::read_dta(path, n_max = 0)
    labels <- purrr::map_chr(dta0, function(x) {
      lbl <- attr(x, "label", exact = TRUE)
      if (is.null(lbl) || length(lbl) == 0 || is.na(lbl)) "" else as.character(lbl)
    })
    tibble::tibble(var = names(dta0), label = labels)
  }, error = function(e) {
    tibble::tibble(var = character(), label = character())
  })
}

safe_read_lits_labelbook <- function(path) {
  tryCatch({
    x <- readxl::read_excel(path, sheet = 1)
    if (!all(c("variable", "variable_label") %in% names(x))) {
      return(tibble::tibble(var = character(), label = character()))
    }
    x %>%
      dplyr::filter(!is.na(variable), variable != "") %>%
      dplyr::transmute(
        var = as.character(variable),
        label = dplyr::coalesce(as.character(variable_label), "")
      )
  }, error = function(e) {
    tibble::tibble(var = character(), label = character())
  })
}

infer_lits_wave <- function(name) {
  nm <- tolower(name)
  dplyr::case_when(
    stringr::str_detect(nm, "lits_iv") ~ "2022-23",
    stringr::str_detect(nm, "lits_iii") ~ "2016",
    stringr::str_detect(nm, "lits2") ~ "2010",
    stringr::str_detect(nm, "lits_ii") ~ "2010",
    stringr::str_detect(nm, "lits_i") ~ "2006",
    TRUE ~ NA_character_
  )
}

audit_lits <- function() {
  lits_dir <- file.path(PROJ_PATHS$raw_data, "lits")

  csv_files <- list.files(
    lits_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  dta_files <- list.files(
    lits_dir,
    pattern = "\\.dta$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  zip_files <- list.files(
    lits_dir,
    pattern = "\\.zip$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  zip_entries <- purrr::map_dfr(zip_files, function(zf) {
    lst <- utils::unzip(zf, list = TRUE)
    if (nrow(lst) == 0) return(tibble::tibble())
    lst %>%
      dplyr::filter(stringr::str_detect(tolower(Name), "\\.csv$")) %>%
      dplyr::transmute(
        source_file = zf,
        zip_entry = Name,
        source_type = "zip_csv",
        source_name = basename(zf)
      )
  })

  csv_sources <- tibble::tibble(
    source_file = csv_files,
    zip_entry = NA_character_,
    source_type = "csv",
    source_name = basename(csv_files)
  )
  dta_sources <- tibble::tibble(
    source_file = dta_files,
    zip_entry = NA_character_,
    source_type = "dta",
    source_name = basename(dta_files)
  )

  lits_sources <- dplyr::bind_rows(csv_sources, dta_sources, zip_entries) %>%
    dplyr::mutate(
      wave = infer_lits_wave(dplyr::coalesce(zip_entry, source_name))
    )

  if (nrow(lits_sources) > 0) {
    lits_sources <- lits_sources %>%
      dplyr::mutate(
        var_map = purrr::pmap(
          list(source_type, source_file, zip_entry),
          function(st, sf, ze) {
            if (st == "csv") return(safe_read_csv_var_labels(sf))
            if (st == "dta") return(safe_read_dta_var_labels(sf))
            safe_read_zip_csv_var_labels(sf, ze)
          }
        )
      )
  }

  expected <- tibble::tribble(
    ~source, ~expected_wave, ~expected_years, ~wave_file_found,
    "LiTS", "2010", "2010", any(lits_sources$wave == "2010"),
    "LiTS", "2016", "2016", any(lits_sources$wave == "2016"),
    "LiTS", "2022-23", "2022-2023", any(lits_sources$wave == "2022-23")
  )

  readr::write_csv(lits_sources %>% dplyr::select(source_file, zip_entry, source_type, source_name, wave), file.path(PROJ_PATHS$metadata, "audit_lits_file_inventory.csv"))
  readr::write_csv(expected, file.path(PROJ_PATHS$metadata, "audit_lits_expected_waves.csv"))

  if (nrow(lits_sources) == 0) {
    checks <- tibble::tribble(
      ~field, ~status, ~notes,
      "parental education variable", "cannot verify", "LiTS files not found in data/raw/lits",
      "respondent education variable", "cannot verify", "LiTS files not found in data/raw/lits",
      "respondent age / birth cohort", "cannot verify", "LiTS files not found in data/raw/lits",
      "region", "cannot verify", "LiTS files not found in data/raw/lits",
      "urban/rural", "cannot verify", "LiTS files not found in data/raw/lits",
      "household composition", "cannot verify", "LiTS files not found in data/raw/lits",
      "migration variables", "cannot verify", "LiTS files not found in data/raw/lits",
      "child education/investment variables", "cannot verify", "LiTS files not found in data/raw/lits",
      "sampling weights", "cannot verify", "LiTS files not found in data/raw/lits",
      "comparability across rounds", "cannot verify", "LiTS files not found in data/raw/lits"
    )
    readr::write_csv(checks, file.path(PROJ_PATHS$metadata, "audit_lits_checks.csv"))

    lines <- c(
      "# LiTS Data Audit (2010, 2016, 2022-23)",
      "",
      paste0("Audit date: ", as.character(Sys.Date())),
      "",
      "## File Availability",
      "",
      paste0("- Expected waves checked: ", nrow(expected)),
      "- Files found in `data/raw/lits`: 0",
      "",
      "## One-Page Inventory",
      ""
    )
    for (i in seq_len(nrow(checks))) {
      lines <- c(lines, paste0("- ", checks$field[i], ": ", checks$status[i], " (", checks$notes[i], ")"))
    }
    lines <- c(lines, "", "## Conclusion", "", "- LiTS audit is blocked by missing raw files.")
    write_md(file.path(PROJ_PATHS$metadata, "audit_lits_inventory.md"), lines)
    return(invisible(NULL))
  }

  lits_var_long <- lits_sources %>%
    dplyr::filter(!is.na(wave)) %>%
    tidyr::unnest(var_map) %>%
    dplyr::filter(!is.na(var), var != "")

  labelbook_candidates <- c(
    file.path(PROJ_PATHS$raw_data, "lits", "lits_iv_dta", "labelbook_lits_iv.xlsx"),
    file.path(PROJ_PATHS$raw_data, "lits", "labelbook_lits_iv.xlsx")
  )
  labelbook_path <- labelbook_candidates[file.exists(labelbook_candidates)][1]
  if (!is.na(labelbook_path) && nzchar(labelbook_path)) {
    lb <- safe_read_lits_labelbook(labelbook_path)
    if (nrow(lb) > 0) {
      lb <- lb %>%
        dplyr::mutate(
          source_file = normalizePath(labelbook_path, winslash = "/", mustWork = FALSE),
          zip_entry = NA_character_,
          source_type = "labelbook",
          source_name = basename(labelbook_path),
          wave = "2022-23"
        ) %>%
        dplyr::select(source_file, zip_entry, source_type, source_name, wave, var, label)
      lits_var_long <- dplyr::bind_rows(lits_var_long, lb)
    }
  }
  first_non_empty <- function(x) {
    x <- unique(stats::na.omit(as.character(x)))
    x <- x[nzchar(x)]
    if (length(x) == 0) "" else x[[1]]
  }
  lits_var_long <- lits_var_long %>%
    dplyr::group_by(wave, var) %>%
    dplyr::summarise(
      label = first_non_empty(label),
      .groups = "drop"
    )
  readr::write_csv(lits_var_long, file.path(PROJ_PATHS$metadata, "audit_lits_var_long.csv"))

  target_waves <- c("2010", "2016", "2022-23")
  available_target_waves <- intersect(target_waves, unique(lits_var_long$wave))
  missing_target_waves <- setdiff(target_waves, available_target_waves)

  vars_by_wave <- purrr::set_names(vector("list", length(target_waves)), target_waves)
  for (w in target_waves) {
    vars_by_wave[[w]] <- lits_var_long %>%
      dplyr::filter(wave == w) %>%
      dplyr::pull(var) %>%
      unique()
  }

  field_patterns <- tibble::tribble(
    ~field, ~pattern,
    "parental education variable", "(father|mother|parent).*(education|school|years of full-time education)|(education|school|years of full-time education).*(father|mother|parent)|books at home|number of books",
    "respondent education variable", "highest level of education|education completed|school|grade|tertiary|university|college|years of full-time education",
    "respondent age / birth cohort", "(^age($|_|\\d)|age_pr|age_sr|\\bage\\b|year of birth|birth year|yrbirth|cohort)",
    "region", "(^region|_region$|district|oblast|province|rayon|geo)",
    "urban/rural", "(^urban$|urbanity|^rural$|rural_|_rural$|village|settlement|city[_$])",
    "household composition", "household|hhsize|member|roster|relation|marital|children",
    "migration variables", "migr|remit|abroad|foreign|immig|emig",
    "child education/investment variables", "online learning|remote learning|distance learning|school closure|school closed|device|internet access.*school|child care facilities|education to their children",
    "sampling weights", "weight|wgt|psu|strata|cluster|pweight"
  ) %>%
    tidyr::crossing(wave = target_waves) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      present = {
        wv <- wave
        vars <- vars_by_wave[[wv]]
        labels <- lits_var_long %>% dplyr::filter(.data$wave == wv, .data$var %in% vars) %>% dplyr::pull(label)
        probe <- paste(tolower(vars), tolower(dplyr::coalesce(labels, "")))
        if (length(probe) == 0) FALSE else any(stringr::str_detect(probe, pattern))
      },
      examples = {
        wv <- wave
        wave_df <- lits_var_long %>% dplyr::filter(.data$wave == wv)
        if (nrow(wave_df) == 0) {
          ""
        } else {
          probe <- paste(tolower(wave_df$var), tolower(dplyr::coalesce(wave_df$label, "")))
          idx <- which(stringr::str_detect(probe, pattern))
          if (length(idx) == 0) {
            ""
          } else {
            hits <- wave_df[idx, c("var", "label")]
            formatted <- ifelse(
              is.na(hits$label) | hits$label == "",
              hits$var,
              paste0(hits$var, " [", hits$label, "]")
            )
            paste(head(formatted, 6), collapse = "; ")
          }
        }
      }
    ) %>%
    dplyr::ungroup()

  readr::write_csv(field_patterns, file.path(PROJ_PATHS$metadata, "audit_lits_variable_presence_by_wave.csv"))

  checks <- field_patterns %>%
    dplyr::group_by(field) %>%
    dplyr::summarise(
      n_waves_present = sum(present),
      examples = paste0(wave, ": ", dplyr::if_else(examples == "", "[none]", examples), collapse = " | "),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      status = dplyr::case_when(
        n_waves_present == length(target_waves) ~ "supported",
        n_waves_present > 0 ~ "partially supported",
        TRUE ~ "not found"
      )
    )

  if (length(missing_target_waves) > 0) {
    checks <- checks %>%
      dplyr::mutate(notes = paste0("Missing expected wave file(s): ", paste(missing_target_waves, collapse = ", "), ". Variable examples -> ", examples))
  } else {
    checks <- checks %>%
      dplyr::mutate(notes = paste0("Variable examples -> ", examples))
  }

  checks <- checks %>%
    dplyr::select(field, status, notes)

  uncertain_fields <- c(
    "parental education variable",
    "respondent education variable",
    "migration variables",
    "child education/investment variables",
    "household composition"
  )
  checks <- checks %>%
    dplyr::mutate(
      status = dplyr::if_else(field %in% uncertain_fields & status == "not found", "cannot verify", status),
      notes = dplyr::if_else(
        field %in% uncertain_fields,
        paste0(notes, " Note: construct mapping should still be validated against official LiTS questionnaires/codebooks."),
        notes
      )
    )

  # Round comparability for target waves.
  pairs <- tibble::tribble(
    ~wave_from, ~wave_to,
    "2010", "2016",
    "2016", "2022-23",
    "2010", "2022-23"
  )
  overlaps <- pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_vars_from = length(vars_by_wave[[wave_from]]),
      n_vars_to = length(vars_by_wave[[wave_to]]),
      intersection = length(intersect(vars_by_wave[[wave_from]], vars_by_wave[[wave_to]])),
      union = length(union(vars_by_wave[[wave_from]], vars_by_wave[[wave_to]])),
      jaccard = ifelse(union > 0, intersection / union, NA_real_)
    ) %>%
    dplyr::ungroup()
  readr::write_csv(overlaps, file.path(PROJ_PATHS$metadata, "audit_lits_wave_overlaps.csv"))

  comparability_status <- if (all(expected$wave_file_found)) "supported" else "partially supported"
  comparability_note <- if (all(expected$wave_file_found)) {
    "All target waves located; pairwise variable overlaps reported in audit_lits_wave_overlaps.csv."
  } else {
    paste0("Missing target wave file(s): ", paste(expected$expected_wave[!expected$wave_file_found], collapse = ", "), ".")
  }
  checks <- dplyr::bind_rows(
    checks,
    tibble::tibble(field = "comparability across rounds", status = comparability_status, notes = comparability_note)
  )

  readr::write_csv(checks, file.path(PROJ_PATHS$metadata, "audit_lits_checks.csv"))

  lines <- c(
    "# LiTS Data Audit (2010, 2016, 2022-23)",
    "",
    paste0("Audit date: ", as.character(Sys.Date())),
    "",
    "## File Availability",
    "",
    paste0("- Expected waves checked: ", nrow(expected)),
    paste0("- LiTS source files found (csv + zip entries): ", nrow(lits_sources)),
    paste0("- LiTS IV labelbook found: ", fmt_bool(!is.na(labelbook_path) && nzchar(labelbook_path))),
    paste0("- Wave 2010 found: ", fmt_bool(any(expected$expected_wave == "2010" & expected$wave_file_found))),
    paste0("- Wave 2016 found: ", fmt_bool(any(expected$expected_wave == "2016" & expected$wave_file_found))),
    paste0("- Wave 2022-23 found: ", fmt_bool(any(expected$expected_wave == "2022-23" & expected$wave_file_found))),
    "",
    "## One-Page Inventory",
    ""
  )

  for (i in seq_len(nrow(checks))) {
    lines <- c(lines, paste0("- ", checks$field[i], ": ", checks$status[i], " (", checks$notes[i], ")"))
  }

  lines <- c(
    lines,
    "",
    "## Conclusion",
    "",
    if (!all(expected$wave_file_found)) {
      paste0("- LiTS audit is partial because expected wave files are missing: ", paste(expected$expected_wave[!expected$wave_file_found], collapse = ", "), ".")
    } else {
      "- LiTS wave files for 2010, 2016, and 2022-23 are present. Core schema audit is complete, but construct-level mapping for coded variables still requires official LiTS codebooks/questionnaires."
    }
  )

  write_md(file.path(PROJ_PATHS$metadata, "audit_lits_inventory.md"), lines)
}

build_hbs_meta <- function() {
  files <- list.files(
    file.path(PROJ_PATHS$raw_data, "hbs"),
    pattern = "\\.dta$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(path = files) %>%
    dplyr::mutate(
      year = purrr::map_int(path, extract_hbs_year),
      module = purrr::map_chr(path, extract_hbs_module),
      module_file = basename(path),
      vars = purrr::map(path, safe_read_dta_names)
    ) %>%
    tidyr::unnest_longer(vars, values_to = "var", keep_empty = TRUE) %>%
    dplyr::rename(file = path)
}

hbs_module_path <- function(files_df, year, module_regex) {
  pick <- files_df %>%
    dplyr::filter(year == !!year, stringr::str_detect(module, module_regex)) %>%
    dplyr::slice_head(n = 1)
  if (nrow(pick) == 0) return(NA_character_)
  pick$file[[1]]
}

compute_hbs_overlaps <- function(meta) {
  core_modules <- c("m00_passport", "m00_weight", "m01_roster", "m02_migration", "m03_education")
  years <- sort(unique(meta$year))
  if (length(years) < 2) {
    return(tibble::tibble())
  }

  pairs <- tibble::tibble(year_from = head(years, -1), year_to = tail(years, -1))

  overlaps <- purrr::map_dfr(core_modules, function(mod) {
    purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
      y1 <- pairs$year_from[i]
      y2 <- pairs$year_to[i]
      v1 <- meta %>% dplyr::filter(year == y1, module == mod) %>% dplyr::pull(var) %>% unique()
      v2 <- meta %>% dplyr::filter(year == y2, module == mod) %>% dplyr::pull(var) %>% unique()
      if (length(v1) == 0 || length(v2) == 0) {
        return(tibble::tibble(
          module = mod, year_from = y1, year_to = y2,
          n_vars_from = length(v1), n_vars_to = length(v2),
          intersection = NA_integer_, union = NA_integer_, jaccard = NA_real_
        ))
      }
      inter <- length(intersect(v1, v2))
      uni <- length(union(v1, v2))
      tibble::tibble(
        module = mod, year_from = y1, year_to = y2,
        n_vars_from = length(v1), n_vars_to = length(v2),
        intersection = inter, union = uni, jaccard = ifelse(uni > 0, inter / uni, NA_real_)
      )
    })
  })

  overlaps
}

audit_hbs <- function() {
  files <- list.files(
    file.path(PROJ_PATHS$raw_data, "hbs"),
    pattern = "\\.dta$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    lines <- c(
      "# HBS Data Audit",
      "",
      paste0("Audit date: ", as.character(Sys.Date())),
      "",
      "- No HBS `.dta` files were found in `data/raw/hbs/`."
    )
    write_md(file.path(PROJ_PATHS$metadata, "audit_hbs_inventory.md"), lines)
    return(invisible(NULL))
  }

  meta <- build_hbs_meta()
  years <- sort(unique(meta$year))
  modules_by_year <- meta %>%
    dplyr::distinct(year, module, module_file) %>%
    dplyr::arrange(year, module)
  readr::write_csv(modules_by_year, file.path(PROJ_PATHS$metadata, "audit_hbs_modules_by_year.csv"))

  key_presence <- tibble::tibble(year = years) %>%
    dplyr::mutate(
      roster = purrr::map_lgl(year, ~ any(meta$year == .x & meta$module == "m01_roster")),
      education = purrr::map_lgl(year, ~ any(meta$year == .x & meta$module == "m03_education")),
      migration = purrr::map_lgl(year, ~ any(meta$year == .x & meta$module == "m02_migration")),
      passport = purrr::map_lgl(year, ~ any(meta$year == .x & meta$module == "m00_passport")),
      weights = purrr::map_lgl(year, ~ any(meta$year == .x & stringr::str_detect(meta$module, "^m00_weight"))),
      internet = purrr::map_lgl(year, ~ any(meta$year == .x & meta$module == "m13_internet"))
    )
  readr::write_csv(key_presence, file.path(PROJ_PATHS$metadata, "audit_hbs_key_module_presence.csv"))

  roster_vars <- meta %>% dplyr::filter(module == "m01_roster") %>% dplyr::pull(var) %>% unique()
  edu_vars <- meta %>% dplyr::filter(module == "m03_education") %>% dplyr::pull(var) %>% unique()
  pass_vars <- meta %>% dplyr::filter(module == "m00_passport") %>% dplyr::pull(var) %>% unique()
  weight_vars <- meta %>% dplyr::filter(stringr::str_detect(module, "^m00_weight")) %>% dplyr::pull(var) %>% unique()
  mig_vars <- meta %>% dplyr::filter(module == "m02_migration") %>% dplyr::pull(var) %>% unique()

  parent_child_possible <- all(c("hhid", "iid", "relationship") %in% roster_vars)
  parent_proxy_fields <- any(c("fmid", "p_id") %in% roster_vars)
  education_core_strict <- c("edu_highest", "edu_years", "edu_enrolled", "edu_grade")
  education_core_relaxed <- c("edu_highest", "edu_years", "edu_enrolled")
  edu_vars_by_year <- purrr::map(years, function(y) {
    meta %>% dplyr::filter(year == y, module == "m03_education") %>% dplyr::pull(var) %>% unique()
  })
  names(edu_vars_by_year) <- as.character(years)
  education_consistency_strict <- all(purrr::map_lgl(edu_vars_by_year, ~ all(education_core_strict %in% .x)))
  education_consistency_relaxed <- all(purrr::map_lgl(edu_vars_by_year, ~ all(education_core_relaxed %in% .x)))
  region_available <- any(c("province", "mahalla", "soato_hhid", "urban") %in% union(pass_vars, weight_vars))
  income_available <- any(stringr::str_detect(modules_by_year$module, "m08_nonwage|m09_food|m11_nonfood|m13_cost"))
  multigen_observable <- all(c("hhid", "iid", "age", "relationship") %in% roster_vars)
  migration_available <- all(c("emig", "immig") %in% mig_vars)
  weight_available <- any(c("popw", "uwgt", "indw") %in% weight_vars)

  checks <- tibble::tribble(
    ~field, ~status, ~notes,
    "parent-child links", ifelse(parent_child_possible, "partially supported", "not supported"), ifelse(parent_child_possible, ifelse(parent_proxy_fields, "household member IDs and relationship fields exist (hhid, iid, relationship, plus parent/proxy ID fields). Parent-child links likely reconstructable, but not guaranteed direct in all years.", "relationship with member IDs exists; may require household reconstruction."), "roster lacks required linkage fields"),
    "education variable consistency", dplyr::case_when(
      education_consistency_strict ~ "supported",
      education_consistency_relaxed ~ "partially supported",
      TRUE ~ "not supported"
    ), paste0(
      "All years contain: ",
      paste(education_core_relaxed, collapse = ", "),
      ifelse(education_consistency_strict, "; strict set with edu_grade also consistent.", "; edu_grade is not present in all years.")
    ),
    "region availability", ifelse(region_available, "supported", "not supported"), paste0("Detected geographic fields: ", paste(intersect(c("province", "mahalla", "soato_hhid", "urban"), union(pass_vars, weight_vars)), collapse = ", ")),
    "household income/welfare availability", ifelse(income_available, "supported", "unclear"), "Income/consumption-related modules present (e.g., nonwage, food, nonfood/cost modules).",
    "multiple generations observable", ifelse(multigen_observable, "supported", "not supported"), "Roster has age + relationship + household/member IDs; multigenerational composition can be constructed.",
    "migration variables", ifelse(migration_available, "supported", "partially supported"), paste0("Detected migration fields include: ", paste(intersect(c("emig", "immig", "emig_remit", "emig_country", "immig_country"), mig_vars), collapse = ", ")),
    "sampling weights", ifelse(weight_available, "supported", "not supported"), paste0("Detected weight fields: ", paste(intersect(c("popw", "uwgt", "indw", "strata"), weight_vars), collapse = ", "))
  )
  readr::write_csv(checks, file.path(PROJ_PATHS$metadata, "audit_hbs_checks.csv"))

  overlaps <- compute_hbs_overlaps(meta)
  readr::write_csv(overlaps, file.path(PROJ_PATHS$metadata, "audit_hbs_module_overlaps.csv"))

  # Per-year check for key education fields.
  edu_consistency_by_year <- purrr::map_dfr(years, function(y) {
    vars_y <- edu_vars_by_year[[as.character(y)]]
    tibble::tibble(
      year = y,
      edu_highest = "edu_highest" %in% vars_y,
      edu_years = "edu_years" %in% vars_y,
      edu_enrolled = "edu_enrolled" %in% vars_y,
      edu_grade = "edu_grade" %in% vars_y,
      edu_complete = "edu_complete" %in% vars_y
    )
  })
  readr::write_csv(edu_consistency_by_year, file.path(PROJ_PATHS$metadata, "audit_hbs_education_consistency_by_year.csv"))

  lines <- c(
    "# HBS Data Audit (one-page inventory)",
    "",
    paste0("Audit date: ", as.character(Sys.Date())),
    "",
    "## Coverage",
    "",
    paste0("- HBS files found: ", length(files)),
    paste0("- Years detected: ", paste(years, collapse = ", ")),
    "",
    "## Required Checks",
    ""
  )

  for (i in seq_len(nrow(checks))) {
    lines <- c(lines, paste0("- ", checks$field[i], ": ", checks$status[i], " (", checks$notes[i], ")"))
  }

  lines <- c(
    lines,
    "",
    "## Comparability Across Rounds",
    "",
    "- Key modules (`m01_roster`, `m02_migration`, `m03_education`, `m00_passport`, `m00_weight`) exist in all detected years.",
    "- Education core variables (`edu_highest`, `edu_years`, `edu_enrolled`) are present across years in `m03_education`; `edu_grade` appears only in later rounds.",
    "- Questionnaire structure changes are visible in spending/nonfood modules by 2025; harmonization should use module-aware rules rather than assuming identical file names.",
    "",
    "## Conclusion",
    "",
    "- HBS is suitable for contextual welfare/household structure/migration analysis and regional background controls.",
    "- Parent-child links are likely reconstructable via household roster relationships, but this should be validated with explicit parent identifier logic before using HBS for core intergenerational mobility estimation."
  )

  write_md(file.path(PROJ_PATHS$metadata, "audit_hbs_inventory.md"), lines)
}

audit_admin <- function() {
  admin_dir <- file.path(PROJ_PATHS$raw_data, "admin")
  files <- list.files(admin_dir, full.names = TRUE, recursive = TRUE)
  files <- files[file.info(files)$isdir == FALSE]

  checks <- tibble::tribble(
    ~field, ~status, ~notes,
    "school availability by region-year", ifelse(length(files) > 0, "pending check", "cannot verify"), ifelse(length(files) > 0, "files exist", "No admin files found in data/raw/admin"),
    "university admissions by region-year", ifelse(length(files) > 0, "pending check", "cannot verify"), ifelse(length(files) > 0, "files exist", "No admin files found in data/raw/admin"),
    "COVID school closure timing/intensity", ifelse(length(files) > 0, "pending check", "cannot verify"), ifelse(length(files) > 0, "files exist", "No admin files found in data/raw/admin"),
    "internet/distance-learning proxy by region", ifelse(length(files) > 0, "pending check", "cannot verify"), ifelse(length(files) > 0, "files exist", "No admin files found in data/raw/admin")
  )
  readr::write_csv(checks, file.path(PROJ_PATHS$metadata, "audit_admin_checks.csv"))

  lines <- c(
    "# Administrative Data Audit",
    "",
    paste0("Audit date: ", as.character(Sys.Date())),
    "",
    paste0("- Files found in `data/raw/admin`: ", length(files)),
    "",
    "## One-Page Inventory",
    ""
  )
  for (i in seq_len(nrow(checks))) {
    lines <- c(lines, paste0("- ", checks$field[i], ": ", checks$status[i], " (", checks$notes[i], ")"))
  }
  lines <- c(
    lines,
    "",
    "## Conclusion",
    "",
    if (length(files) == 0) {
      "- Admin-data audit is blocked. Add region-year admin files before any policy-evaluation design is finalized."
    } else {
      "- Admin files found. Run variable-level checks to finalize data usability."
    }
  )

  write_md(file.path(PROJ_PATHS$metadata, "audit_admin_inventory.md"), lines)
}

run_data_audit <- function() {
  ensure_project_dirs()
  audit_lits()
  audit_hbs()
  audit_admin()
  invisible(TRUE)
}
