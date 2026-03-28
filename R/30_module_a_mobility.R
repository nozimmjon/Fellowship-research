safe_rank <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rank(x, ties.method = "average", na.last = "keep") / sum(!is.na(x))
}

weighted_rank <- function(x, w) {
  out <- rep(NA_real_, length(x))
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(out)
  }

  tmp <- tibble::tibble(idx = which(keep), x = x[keep], w = as.numeric(w[keep])) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(w_group = sum(w), idx = list(idx), .groups = "drop") %>%
    dplyr::arrange(x) %>%
    dplyr::mutate(
      w_cum_prev = dplyr::lag(cumsum(w_group), default = 0),
      rank_value = (w_cum_prev + 0.5 * w_group) / sum(w_group)
    )

  for (i in seq_len(nrow(tmp))) {
    out[unlist(tmp$idx[[i]])] <- tmp$rank_value[[i]]
  }
  out
}

weighted_share <- function(cond, w) {
  keep <- !is.na(cond) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(as.numeric(cond[keep]), w[keep])
}

weight_effective_n <- function(w) {
  w_num <- suppressWarnings(as.numeric(w))
  w_num <- w_num[!is.na(w_num) & w_num > 0]
  if (length(w_num) == 0) {
    return(NA_real_)
  }
  (sum(w_num) ^ 2) / sum(w_num ^ 2)
}

weighted_binary_summary <- function(cond, w, conf_level = 0.95) {
  keep <- !is.na(cond) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(list(
      estimate = NA_real_,
      std.error = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      effective_n = NA_real_
    ))
  }

  cond_num <- as.numeric(cond[keep])
  w_num <- as.numeric(w[keep])
  estimate <- stats::weighted.mean(cond_num, w_num)
  effective_n <- weight_effective_n(w_num)
  if (is.na(effective_n) || effective_n <= 1) {
    std.error <- NA_real_
  } else {
    std.error <- sqrt(max(estimate * (1 - estimate), 0) / effective_n)
  }
  alpha <- 1 - conf_level
  z_val <- stats::qnorm(1 - alpha / 2)
  ci_low <- if (is.na(std.error)) NA_real_ else max(0, estimate - z_val * std.error)
  ci_high <- if (is.na(std.error)) NA_real_ else min(1, estimate + z_val * std.error)

  list(
    estimate = estimate,
    std.error = std.error,
    ci_low = ci_low,
    ci_high = ci_high,
    effective_n = effective_n
  )
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
  if (!("sample_weight" %in% names(out))) out$sample_weight <- 1

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
      parent_years_schooling = suppressWarnings(as.numeric(parent_years_schooling)),
      sample_weight = suppressWarnings(as.numeric(sample_weight)),
      sample_weight = dplyr::if_else(is.na(sample_weight) | sample_weight <= 0, NA_real_, sample_weight)
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
  needed <- c("own_years_schooling", "parent_years_schooling", "wave_year", "sample_weight")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  group_cols <- c("wave_year", if (!is.null(group_var)) group_var)
  tmp <- df %>%
    dplyr::filter(!is.na(own_years_schooling), !is.na(parent_years_schooling), !is.na(sample_weight), sample_weight > 0)
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
          child_rank = weighted_rank(own_years_schooling, sample_weight),
          parent_rank = weighted_rank(parent_years_schooling, sample_weight)
        ) %>%
        dplyr::filter(!is.na(child_rank), !is.na(parent_rank))

      n <- nrow(dat)
      if (n < min_n) {
        return(tibble::tibble(
          metric = "rank_rank_slope",
          estimate = NA_real_,
          std.error = NA_real_,
          ci_low = NA_real_,
          ci_high = NA_real_,
          effective_n = weight_effective_n(dat$sample_weight),
          n = n,
          status = "small_n"
        ))
      }

      fit <- stats::lm(child_rank ~ parent_rank, data = dat, weights = sample_weight)
      fit_summary <- summary(fit)
      coef_table <- fit_summary$coefficients
      slope_estimate <- unname(stats::coef(fit)[["parent_rank"]])
      slope_se <- if ("parent_rank" %in% rownames(coef_table)) coef_table["parent_rank", "Std. Error"] else NA_real_
      alpha <- 0.05
      t_val <- if (!is.na(stats::df.residual(fit)) && stats::df.residual(fit) > 0) {
        stats::qt(1 - alpha / 2, df = stats::df.residual(fit))
      } else {
        stats::qnorm(1 - alpha / 2)
      }

      tibble::tibble(
        metric = "rank_rank_slope",
        estimate = slope_estimate,
        std.error = slope_se,
        ci_low = if (is.na(slope_se)) NA_real_ else slope_estimate - t_val * slope_se,
        ci_high = if (is.na(slope_se)) NA_real_ else slope_estimate + t_val * slope_se,
        effective_n = weight_effective_n(dat$sample_weight),
        n = n,
        status = "ok"
      )
    }) %>%
    dplyr::ungroup()

  label_subgroup(out, group_var)
}

compute_directional_rates <- function(df, group_var = NULL, min_n = 30L) {
  needed <- c("own_ed_rank", "parent_ed_rank", "wave_year", "sample_weight")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  group_cols <- c("wave_year", if (!is.null(group_var)) group_var)
  tmp <- df %>%
    dplyr::filter(!is.na(own_ed_rank), !is.na(parent_ed_rank), !is.na(sample_weight), sample_weight > 0)
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
      group_n <- nrow(.x)
      upward <- weighted_binary_summary(.x$own_ed_rank > .x$parent_ed_rank, .x$sample_weight)
      downward <- weighted_binary_summary(.x$own_ed_rank < .x$parent_ed_rank, .x$sample_weight)
      persistence <- weighted_binary_summary(.x$own_ed_rank == .x$parent_ed_rank, .x$sample_weight)
      group_status <- if (group_n >= min_n) "ok" else "small_n"

      tibble::tibble(
        metric = c("upward_mobility_rate", "downward_mobility_rate", "persistence_probability"),
        estimate = c(upward$estimate, downward$estimate, persistence$estimate),
        std.error = c(upward$std.error, downward$std.error, persistence$std.error),
        ci_low = c(upward$ci_low, downward$ci_low, persistence$ci_low),
        ci_high = c(upward$ci_high, downward$ci_high, persistence$ci_high),
        effective_n = c(upward$effective_n, downward$effective_n, persistence$effective_n),
        n = rep(group_n, 3),
        status = rep(group_status, 3)
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      estimate = dplyr::if_else(status == "ok", estimate, NA_real_),
      std.error = dplyr::if_else(status == "ok", std.error, NA_real_),
      ci_low = dplyr::if_else(status == "ok", ci_low, NA_real_),
      ci_high = dplyr::if_else(status == "ok", ci_high, NA_real_),
      effective_n = dplyr::if_else(status == "ok", effective_n, NA_real_)
    )

  label_subgroup(out, group_var)
}

compute_transition_matrix <- function(df, min_n = 30L) {
  needed <- c("wave_year", "own_ed_level", "parent_ed_level", "sample_weight")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::filter(!is.na(parent_ed_level), !is.na(own_ed_level), !is.na(sample_weight), sample_weight > 0) %>%
    dplyr::group_by(wave_year, parent_ed_level, own_ed_level) %>%
    dplyr::summarise(n = dplyr::n(), w_n = sum(sample_weight), .groups = "drop") %>%
    dplyr::group_by(wave_year, parent_ed_level) %>%
    dplyr::mutate(
      n_parent_total = sum(n),
      w_parent_total = sum(w_n),
      share = dplyr::if_else(n_parent_total >= min_n & w_parent_total > 0, w_n / w_parent_total, NA_real_),
      status = dplyr::if_else(n_parent_total >= min_n, "ok", "small_n")
    ) %>%
    dplyr::ungroup()
}

compute_persistence_by_parent <- function(df, min_n = 30L) {
  needed <- c("wave_year", "own_ed_rank", "parent_ed_rank", "parent_ed_level", "sample_weight")
  if (!all(needed %in% names(df)) || nrow(df) == 0) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::filter(!is.na(parent_ed_rank), !is.na(own_ed_rank), !is.na(parent_ed_level), !is.na(sample_weight), sample_weight > 0) %>%
    dplyr::group_by(wave_year, parent_ed_level) %>%
    dplyr::summarise(
      metric = "persistence_probability",
      estimate = dplyr::if_else(dplyr::n() >= min_n, weighted_share(own_ed_rank == parent_ed_rank, sample_weight), NA_real_),
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
      std.error = double(),
      ci_low = double(),
      ci_high = double(),
      effective_n = double(),
      n = integer(),
      status = character()
    ),
    subgroup_metrics = tibble::tibble(
      subgroup_type = character(),
      subgroup_value = character(),
      wave_year = integer(),
      metric = character(),
      estimate = double(),
      std.error = double(),
      ci_low = double(),
      ci_high = double(),
      effective_n = double(),
      n = integer(),
      status = character()
    ),
    transition_matrix = tibble::tibble(
      wave_year = integer(),
      parent_ed_level = character(),
      own_ed_level = character(),
      n = integer(),
      w_n = double(),
      n_parent_total = integer(),
      w_parent_total = double(),
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
