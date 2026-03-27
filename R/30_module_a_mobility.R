safe_rank <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}

normalize_ed_level <- function(x) {
  x <- tolower(as.character(x))
  x <- dplyr::case_when(
    x %in% EDUCATION_LEVELS ~ x,
    stringr::str_detect(x, "tertiary|university|bachelor|master|phd") ~ "tertiary",
    stringr::str_detect(x, "post") ~ "post_secondary_non_tertiary",
    stringr::str_detect(x, "upper|secondary") ~ "upper_secondary",
    stringr::str_detect(x, "lower") ~ "lower_secondary",
    stringr::str_detect(x, "primary") ~ "primary",
    stringr::str_detect(x, "none|no formal") ~ "no_formal",
    TRUE ~ NA_character_
  )
  x
}

coerce_urban_group <- function(x) {
  x_chr <- tolower(as.character(x))
  dplyr::case_when(
    x_chr %in% c("1", "urban", "u", "true", "yes") ~ "urban",
    x_chr %in% c("0", "rural", "r", "false", "no") ~ "rural",
    TRUE ~ NA_character_
  )
}

coerce_gender_group <- function(x) {
  x_chr <- tolower(as.character(x))
  dplyr::case_when(
    x_chr %in% c("male", "m", "1", "man", "boy") ~ "male",
    x_chr %in% c("female", "f", "0", "woman", "girl") ~ "female",
    TRUE ~ NA_character_
  )
}

prepare_mobility_data <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble())
  }

  out <- df
  if (!("wave_year" %in% names(out))) out$wave_year <- NA_integer_
  if (!("region" %in% names(out))) out$region <- NA_character_
  if (!("cohort" %in% names(out))) out$cohort <- NA_character_
  if (!("urban" %in% names(out))) out$urban <- NA
  if (!("gender" %in% names(out))) out$gender <- NA_character_
  if (!("own_ed_level" %in% names(out))) out$own_ed_level <- NA_character_
  if (!("parent_ed_level" %in% names(out))) out$parent_ed_level <- NA_character_
  if (!("own_years_schooling" %in% names(out))) out$own_years_schooling <- NA_real_
  if (!("parent_years_schooling" %in% names(out))) out$parent_years_schooling <- NA_real_

  out <- out %>%
    dplyr::mutate(
      wave_year = suppressWarnings(as.integer(wave_year)),
      region = dplyr::na_if(as.character(region), ""),
      cohort = dplyr::na_if(as.character(cohort), ""),
      own_ed_level = normalize_ed_level(own_ed_level),
      parent_ed_level = normalize_ed_level(parent_ed_level),
      own_ed_rank = match(own_ed_level, EDUCATION_LEVELS),
      parent_ed_rank = match(parent_ed_level, EDUCATION_LEVELS),
      urban_group = coerce_urban_group(urban),
      gender_group = coerce_gender_group(gender),
      own_years_schooling = suppressWarnings(as.numeric(own_years_schooling)),
      parent_years_schooling = suppressWarnings(as.numeric(parent_years_schooling))
    )

  out
}

available_subgroup_vars <- function(df) {
  candidates <- c("urban_group", "gender_group", "region", "cohort")
  candidates[purrr::map_lgl(candidates, function(v) {
    v %in% names(df) && any(!is.na(df[[v]]) & as.character(df[[v]]) != "")
  })]
}

label_subgroup <- function(df, group_var = NULL) {
  if (nrow(df) == 0) {
    return(df)
  }

  if (is.null(group_var)) {
    return(df %>%
      dplyr::mutate(
        subgroup_type = "overall",
        subgroup_value = "all",
        .before = 1
      ))
  }

  subgroup_type <- dplyr::case_when(
    group_var == "urban_group" ~ "urban_rural",
    group_var == "gender_group" ~ "gender",
    TRUE ~ group_var
  )

  df %>%
    dplyr::mutate(
      subgroup_type = subgroup_type,
      subgroup_value = as.character(.data[[group_var]]),
      .before = 1
    ) %>%
    dplyr::select(-dplyr::all_of(group_var))
}

compute_rank_rank_slope <- function(df, group_var = NULL, min_n = 30L) {
  needed <- c("own_years_schooling", "parent_years_schooling", "wave_year")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  group_cols <- c("wave_year", if (!is.null(group_var)) group_var)
  tmp <- df %>%
    dplyr::filter(!is.na(own_years_schooling), !is.na(parent_years_schooling))
  if (!is.null(group_var)) {
    tmp <- tmp %>%
      dplyr::filter(!is.na(.data[[group_var]]), as.character(.data[[group_var]]) != "")
  }

  if (nrow(tmp) == 0) {
    return(tibble::tibble())
  }

  out <- tmp %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::group_modify(~{
      dat <- .x %>%
        dplyr::mutate(
          child_rank = safe_rank(own_years_schooling),
          parent_rank = safe_rank(parent_years_schooling)
        ) %>%
        dplyr::filter(!is.na(child_rank), !is.na(parent_rank))

      n <- nrow(dat)
      if (n < min_n) {
        return(tibble::tibble(metric = "rank_rank_slope", estimate = NA_real_, n = n, status = "small_n"))
      }

      fit <- stats::lm(child_rank ~ parent_rank, data = dat)
      tibble::tibble(
        metric = "rank_rank_slope",
        estimate = unname(stats::coef(fit)[["parent_rank"]]),
        n = n,
        status = "ok"
      )
    }) %>%
    dplyr::ungroup()

  label_subgroup(out, group_var)
}

compute_directional_rates <- function(df, group_var = NULL, min_n = 30L) {
  needed <- c("own_ed_rank", "parent_ed_rank", "wave_year")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  group_cols <- c("wave_year", if (!is.null(group_var)) group_var)
  tmp <- df %>%
    dplyr::filter(!is.na(own_ed_rank), !is.na(parent_ed_rank))
  if (!is.null(group_var)) {
    tmp <- tmp %>%
      dplyr::filter(!is.na(.data[[group_var]]), as.character(.data[[group_var]]) != "")
  }

  if (nrow(tmp) == 0) {
    return(tibble::tibble())
  }

  out <- tmp %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      upward_mobility_rate = mean(own_ed_rank > parent_ed_rank),
      downward_mobility_rate = mean(own_ed_rank < parent_ed_rank),
      persistence_probability = mean(own_ed_rank == parent_ed_rank),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(upward_mobility_rate, downward_mobility_rate, persistence_probability),
      names_to = "metric",
      values_to = "estimate"
    ) %>%
    dplyr::mutate(
      estimate = dplyr::if_else(n >= min_n, estimate, NA_real_),
      status = dplyr::if_else(n >= min_n, "ok", "small_n")
    )

  label_subgroup(out, group_var)
}

compute_transition_matrix <- function(df, min_n = 30L) {
  needed <- c("wave_year", "own_ed_level", "parent_ed_level")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::filter(!is.na(parent_ed_level), !is.na(own_ed_level)) %>%
    dplyr::count(wave_year, parent_ed_level, own_ed_level, name = "n") %>%
    dplyr::group_by(wave_year, parent_ed_level) %>%
    dplyr::mutate(
      n_parent_total = sum(n),
      share = dplyr::if_else(n_parent_total >= min_n, n / n_parent_total, NA_real_),
      status = dplyr::if_else(n_parent_total >= min_n, "ok", "small_n")
    ) %>%
    dplyr::ungroup()
}

compute_persistence_by_parent <- function(df, min_n = 30L) {
  needed <- c("wave_year", "own_ed_rank", "parent_ed_rank", "parent_ed_level")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::filter(!is.na(parent_ed_rank), !is.na(own_ed_rank), !is.na(parent_ed_level)) %>%
    dplyr::group_by(wave_year, parent_ed_level) %>%
    dplyr::summarise(
      metric = "persistence_probability",
      estimate = dplyr::if_else(dplyr::n() >= min_n, mean(own_ed_rank == parent_ed_rank), NA_real_),
      n = dplyr::n(),
      status = dplyr::if_else(dplyr::n() >= min_n, "ok", "small_n"),
      .groups = "drop"
    )
}

empty_module_a_result <- function(measure_spec, variable_lock) {
  list(
    measure_spec = measure_spec,
    variable_lock = variable_lock,
    core_metrics = tibble::tibble(
      subgroup_type = character(),
      subgroup_value = character(),
      wave_year = integer(),
      metric = character(),
      estimate = double(),
      n = integer(),
      status = character()
    ),
    subgroup_metrics = tibble::tibble(
      subgroup_type = character(),
      subgroup_value = character(),
      wave_year = integer(),
      metric = character(),
      estimate = double(),
      n = integer(),
      status = character()
    ),
    transition_matrix = tibble::tibble(
      wave_year = integer(),
      parent_ed_level = character(),
      own_ed_level = character(),
      n = integer(),
      n_parent_total = integer(),
      share = double(),
      status = character()
    ),
    persistence_by_parent = tibble::tibble(
      wave_year = integer(),
      parent_ed_level = character(),
      metric = character(),
      estimate = double(),
      n = integer(),
      status = character()
    )
  )
}

estimate_mobility_metrics <- function(df) {
  measure_spec <- build_mobility_measure_spec()
  variable_lock <- build_mobility_variable_lock()

  if (nrow(df) == 0) {
    return(empty_module_a_result(measure_spec, variable_lock))
  }

  dat <- prepare_mobility_data(df)
  if (nrow(dat) == 0) {
    return(empty_module_a_result(measure_spec, variable_lock))
  }

  core_metrics <- dplyr::bind_rows(
    compute_rank_rank_slope(dat, group_var = NULL),
    compute_directional_rates(dat, group_var = NULL)
  )

  subgroup_vars <- available_subgroup_vars(dat)
  subgroup_metrics <- purrr::map_dfr(subgroup_vars, function(gv) {
    dplyr::bind_rows(
      compute_rank_rank_slope(dat, group_var = gv),
      compute_directional_rates(dat, group_var = gv)
    )
  })

  list(
    measure_spec = measure_spec,
    variable_lock = variable_lock,
    core_metrics = core_metrics,
    subgroup_metrics = subgroup_metrics,
    transition_matrix = compute_transition_matrix(dat),
    persistence_by_parent = compute_persistence_by_parent(dat)
  )
}
