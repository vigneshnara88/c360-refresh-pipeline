# ============================================================
# 05_bolt_on_export.R
# Bolt new quarterly aggregates onto existing output and export CSVs
# ============================================================
#
# This is the only stage where dashboard filtering is applied.
# Changing Include in Dashboard? in the registry changes the dashboard-facing
# export, but does not stop the question from being processed into the full
# all-registry output.

if (!exists("dashboard_agg")) {
  dashboard_agg_path <- file.path(INTERMEDIATE_DIR, "04_dashboard_agg.rds")
  if (!file.exists(dashboard_agg_path)) {
    stop("dashboard_agg not found and 04_dashboard_agg.rds does not exist. Run 04 first.")
  }
  dashboard_agg <- readRDS(dashboard_agg_path)
}
if (!exists("processed_files_for_manifest")) {
  processed_manifest_path <- file.path(INTERMEDIATE_DIR, "03_processed_files_for_manifest.rds")
  processed_files_for_manifest <- if (file.exists(processed_manifest_path)) {
    readRDS(processed_manifest_path)
  } else {
    data.table::data.table()
  }
}
if (!exists("old_stat_sig_test_agg")) {
  test_agg_path <- file.path(INTERMEDIATE_DIR, "04_internal_test_agg.rds")
  old_stat_sig_test_agg <- if (file.exists(test_agg_path)) {
    readRDS(test_agg_path)
  } else {
    data.table::data.table()
  }
}
if (!exists("clean_col")) {
  source(file.path(PROJECT_DIR, "01_helpers.R"))
}

message("============================================================")
message("05_bolt_on_export.R")
message("New all-registry dashboard-period rows from 04: ", nrow(dashboard_agg))
message("Internal Week/Month trend-test rows from 04: ", nrow(old_stat_sig_test_agg))
message("============================================================")

load_existing_c360_csv <- function(path) {
  if (!file.exists(path)) {
    warning("Existing C360 CSV not found: ", path, ". Exporting new processed rows only.")
    return(data.table::data.table())
  }

  data.table::fread(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "NULL", "#NULL", "#NULL!")
  )
}

time_for_calc <- function(x) {
  x <- as.character(x)
  x[x == OUTPUT_YEAR_TIME_LABEL] <- "Year"
  x
}

normalise_time_for_output <- function(df) {
  df <- data.table::as.data.table(df)
  if ("Time" %in% names(df)) {
    df[, Time := as.character(Time)]
    df[Time == "Year", Time := OUTPUT_YEAR_TIME_LABEL]
  }
  df
}

coerce_key_columns_to_character <- function(df, keys) {
  df <- data.table::as.data.table(df)
  for (cc in intersect(keys, names(df))) {
    df[[cc]] <- as.character(df[[cc]])
  }
  df
}

remove_rows_matching_new <- function(existing, new_rows, keys) {
  existing <- data.table::as.data.table(existing)
  new_rows <- data.table::as.data.table(new_rows)

  keys <- keys[keys %in% names(existing) & keys %in% names(new_rows)]
  if (length(keys) == 0 || nrow(existing) == 0 || nrow(new_rows) == 0) {
    return(existing)
  }

  existing <- coerce_key_columns_to_character(existing, keys)
  new_rows <- coerce_key_columns_to_character(new_rows, keys)
  new_keys <- unique(new_rows[, ..keys])
  existing[!new_keys, on = keys]
}

previous_n_sd <- function(x, n) {
  x <- safe_as_numeric(x)
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (i <= 1) next
    vals <- tail(x[seq_len(i - 1)], n)
    vals <- vals[!is.na(vals)]
    if (length(vals) >= 2) {
      out[[i]] <- stats::sd(vals)
    }
  }
  out
}

previous_n_mean <- function(x, n) {
  x <- safe_as_numeric(x)
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (i <= 1) next
    vals <- tail(x[seq_len(i - 1)], n)
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      out[[i]] <- mean(vals)
    }
  }
  out
}

previous_n_abs_dev_mean <- function(x, baseline, n) {
  x <- safe_as_numeric(x)
  baseline <- safe_as_numeric(baseline)
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    if (i <= 1) next
    idx <- seq_len(i - 1)
    idx <- tail(idx, n)
    vals <- abs(x[idx] - baseline[idx])
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      out[[i]] <- mean(vals)
    }
  }
  out
}

c360_label <- function(name, default) {
  if (exists(name, inherits = TRUE)) {
    get(name, inherits = TRUE)
  } else {
    default
  }
}

legacy_consecutive_count <- function(x, type = c("decline", "improvement", "no_change")) {
  type <- match.arg(type)
  x <- safe_as_numeric(x)
  out <- rep(0L, length(x))

  for (i in seq_along(x)) {
    condition <- if (type == "decline") {
      !is.na(x[[i]]) && x[[i]] < 0
    } else if (type == "improvement") {
      !is.na(x[[i]]) && x[[i]] > 0
    } else {
      !is.na(x[[i]]) && x[[i]] == 0
    }

    out[[i]] <- if (condition) {
      if (i == 1L) 1L else out[[i - 1L]] + 1L
    } else {
      0L
    }
  }

  out
}

annual_baseline_abs_change_mean <- function(x, n) {
  x <- safe_as_numeric(x)
  out <- rep(NA_real_, length(x))
  changes <- x - data.table::shift(x)

  for (i in seq_along(x)) {
    if (i <= 1) next
    idx <- seq_len(i - 1L)
    vals <- abs(changes[idx])
    vals <- vals[!is.na(vals)]
    vals <- tail(vals, n)
    if (length(vals) > 0) {
      out[[i]] <- mean(vals)
    }
  }

  out
}

add_legacy_trend_count_fields <- function(df) {
  dt <- data.table::as.data.table(df)
  count_cols <- c(
    "Sensitivity Count - Consective Decline (Net-Sat)",
    "Sensitivity Count - Consective Improved (Net-Sat)",
    "Sensitivity Count - No Consecutive Changes (Net-Sat)",
    "Sensitivity Count - No Change (Net-Sat)",
    "Sensitivity Count - Improvement (Net-Sat)",
    "Sensitivity Count - Decline (Net-Sat)"
  )
  dt <- add_missing_columns(dt, count_cols)

  if (nrow(dt) == 0 || !all(c("Time", "Period", "Trend") %in% names(dt))) {
    return(dt)
  }

  dt[, .calc_time_count := time_for_calc(Time)]
  group_cols <- c("demo", "demography", "Tier1", "Tier2", "Tier3", ".calc_time_count", "Period")
  group_cols <- group_cols[group_cols %in% names(dt)]

  for (cc in count_cols) {
    dt[[cc]] <- 0
  }

  quarter_idx <- dt$.calc_time_count == "Quarter"
  if (any(quarter_idx, na.rm = TRUE)) {
    quarter_counts <- dt[
      quarter_idx,
      .(
        `Sensitivity Count - Consective Decline (Net-Sat)` =
          sum(Trend == c360_label("TREND_LABEL_CONSECUTIVE_DECLINE", "Consecutive Decline"), na.rm = TRUE),
        `Sensitivity Count - Consective Improved (Net-Sat)` =
          sum(Trend == c360_label("TREND_LABEL_CONSECUTIVE_IMPROVEMENT", "Consecutive Improvement"), na.rm = TRUE),
        `Sensitivity Count - No Consecutive Changes (Net-Sat)` =
          sum(Trend == c360_label("TREND_LABEL_NEITHER", "Neither"), na.rm = TRUE)
      ),
      by = group_cols
    ]
    quarter_count_cols <- setdiff(names(quarter_counts), group_cols)
    dt[quarter_counts, on = group_cols, (quarter_count_cols) := mget(paste0("i.", quarter_count_cols))]
  }

  annual_idx <- dt$.calc_time_count == "Year"
  if (any(annual_idx, na.rm = TRUE)) {
    annual_counts <- dt[
      annual_idx,
      .(
        `Sensitivity Count - No Change (Net-Sat)` =
          sum(Trend == c360_label("OLD_STAT_SIG_LABEL_NO_CHANGE", "No Significant & Substantive Change"), na.rm = TRUE),
        `Sensitivity Count - Improvement (Net-Sat)` =
          sum(Trend == c360_label("OLD_STAT_SIG_LABEL_IMPROVE", "Significant & Substantive Improvement"), na.rm = TRUE),
        `Sensitivity Count - Decline (Net-Sat)` =
          sum(Trend == c360_label("OLD_STAT_SIG_LABEL_DECLINE", "Significant & Substantive Decline"), na.rm = TRUE)
      ),
      by = group_cols
    ]
    annual_count_cols <- setdiff(names(annual_counts), group_cols)
    dt[annual_counts, on = group_cols, (annual_count_cols) := mget(paste0("i.", annual_count_cols))]
  }

  dt[, .calc_time_count := NULL]
  dt[]
}

legacy_dashboard_trend_label <- function(x) {
  x <- stringr::str_squish(as.character(x))
  dplyr::case_when(
    is.na(x) | x == "" | x == "NA" ~ NA_character_,
    x %in% c(
      "Significant Substantive Improvement",
      "Significant & Substantive Improvement"
    ) ~ "Significant & Substantive Improvement",
    x %in% c(
      "Significant Substantive Decline",
      "Significant & Substantive Decline"
    ) ~ "Significant & Substantive Decline",
    x %in% c(
      "Bridge Relative Improvement",
      "Substantive Improvement",
      "Significant Improvement",
      "Improvement",
      "Improved"
    ) ~ "Improved",
    x %in% c(
      "Bridge Relative Decline",
      "Substantive Decline",
      "Significant Decline",
      "Decline",
      "Worsen"
    ) ~ "Worsen",
    x %in% c(
      "No Flag",
      "Not Significant",
      "No Significant Difference",
      "No Change"
    ) ~ "No Change",
    x %in% c(
      "No Significant & Substantive Change",
      "Insufficient Baseline Data",
      "Consecutive Improvement",
      "Consecutive Decline",
      "Neither",
      "Not Applicable"
    ) ~ x,
    TRUE ~ x
  )
}

calculate_change_sensitivity_baseline <- function(df) {
  df <- data.table::as.data.table(df)
  if (nrow(df) == 0) {
    return(df)
  }

  df <- add_missing_columns(
    df,
    c("Agree", "Net_Satisfaction", "total.sample.n", "Time", "Period", "dataset", "variable", "unique", "demo", "demography")
  )

  numeric_cols <- c(
    "Agree", "Disagree", "Neutral", "Net_Satisfaction", "total.sample.n",
    "%Net-Satisfaction", "Overall %Net-Satisfaction", "Diff %Net-Sat (Demo - Overall)"
  )
  for (cc in intersect(numeric_cols, names(df))) {
    df[[cc]] <- safe_as_numeric(df[[cc]])
  }

  df[, .calc_time := time_for_calc(Time)]
  df[, .period_order := period_order_value(.calc_time, Period)]

  group_cols <- c("dataset", "variable", "unique", "demo", "demography", ".calc_time")
  group_cols <- group_cols[group_cols %in% names(df)]
  data.table::setorderv(df, c(group_cols, ".period_order"), na.last = TRUE)

  df[
    ,
    `Change (Agree)` := Agree - data.table::shift(Agree),
    by = group_cols
  ]
  df[
    ,
    `Change (Net-Satisfaction)` := Net_Satisfaction - data.table::shift(Net_Satisfaction),
    by = group_cols
  ]

  lookback_q <- PRECEDING_QUARTERS_FOR_SD %||% 2
  df[
    ,
    `SD (Preceding 2Q - Agree)` := previous_n_sd(Agree, lookback_q),
    by = group_cols
  ]
  df[
    ,
    `SD (Preceding 2Q - Net)` := previous_n_sd(Net_Satisfaction, lookback_q),
    by = group_cols
  ]

  lookback_y <- PRECEDING_YEARS_FOR_BASELINE %||% 5
  year_idx <- df$.calc_time == "Year"
  df[year_idx, `Annual Change (Net-Satisfaction)` := `Change (Net-Satisfaction)`]
  df[
    year_idx,
    `Baseline Reference (Preceding 5 Year Mean)` := previous_n_mean(Net_Satisfaction, lookback_y),
    by = group_cols
  ]
  df[
    year_idx,
    `Deviation from Reference` := Net_Satisfaction - `Baseline Reference (Preceding 5 Year Mean)`
  ]
  df[
    year_idx,
    `Baseline Deviation (Preceding 5 Year Absolute Mean)` := annual_baseline_abs_change_mean(Net_Satisfaction, lookback_y),
    by = group_cols
  ]

  df[
    ,
    .prev_change_net := data.table::shift(`Change (Net-Satisfaction)`),
    by = group_cols
  ]
  df[
    ,
    .prev_sd_net := data.table::shift(`SD (Preceding 2Q - Net)`),
    by = group_cols
  ]

  quarterly_idx <- df$.calc_time == "Quarter"
  df[
    quarterly_idx,
    Trend := dplyr::case_when(
      is.na(`Change (Net-Satisfaction)`) |
        is.na(.prev_change_net) |
        is.na(`SD (Preceding 2Q - Net)`) |
        is.na(.prev_sd_net) ~ c360_label("TREND_LABEL_NOT_APPLICABLE", "Not Applicable"),
      `Change (Net-Satisfaction)` < 0 &
        .prev_change_net < 0 &
        abs(`Change (Net-Satisfaction)`) > `SD (Preceding 2Q - Net)` &
        abs(.prev_change_net) > .prev_sd_net ~ c360_label("TREND_LABEL_CONSECUTIVE_DECLINE", "Consecutive Decline"),
      `Change (Net-Satisfaction)` > 0 &
        .prev_change_net > 0 &
        abs(`Change (Net-Satisfaction)`) > `SD (Preceding 2Q - Net)` &
        abs(.prev_change_net) > .prev_sd_net ~ c360_label("TREND_LABEL_CONSECUTIVE_IMPROVEMENT", "Consecutive Improvement"),
      TRUE ~ c360_label("TREND_LABEL_NEITHER", "Neither")
    )
  ]

  df[
    year_idx,
    Trend := dplyr::case_when(
      is.na(Net_Satisfaction) ~ c360_label("OLD_STAT_SIG_LABEL_NOT_APPLICABLE", "Not Applicable"),
      is.na(`Baseline Reference (Preceding 5 Year Mean)`) |
        is.na(`Baseline Deviation (Preceding 5 Year Absolute Mean)`) ~
        c360_label("OLD_STAT_SIG_LABEL_INSUFFICIENT", "Insufficient Baseline Data"),
      TRUE ~ c360_label("OLD_STAT_SIG_LABEL_NO_CHANGE", "No Significant & Substantive Change")
    )
  ]

  df[
    ,
    Sensitivity := dplyr::case_when(
      is.na(total.sample.n) ~ c360_label("SENSITIVITY_EXCLUDE", "Exclude"),
      total.sample.n < MIN_SAMPLE_N ~ c360_label("SENSITIVITY_EXCLUDE", "Exclude"),
      TRUE ~ c360_label("SENSITIVITY_INCLUDE", "Include")
    )
  ]
  df[, Trend := legacy_dashboard_trend_label(Trend)]
  df <- add_legacy_trend_count_fields(df)

  df[, c(".calc_time", ".period_order", ".prev_change_net", ".prev_sd_net") := NULL]
  df[]
}

add_ci_significance_fields <- function(df) {
  df <- data.table::as.data.table(df)
  if (nrow(df) == 0 || !isTRUE(ENABLE_CI_SIGNIFICANCE_OUTPUT)) {
    return(df)
  }

  df <- add_missing_columns(df, c(
    "Net_Sat_SE", "Net_Satisfaction", "Change (Net-Satisfaction)", "Time", "Period",
    "dataset", "variable", "unique", "demo", "demography"
  ))
  for (cc in c("Net_Sat_SE", "Net_Satisfaction", "Change (Net-Satisfaction)")) {
    df[[cc]] <- safe_as_numeric(df[[cc]])
  }

  df[, .calc_time := time_for_calc(Time)]
  df[, .period_order := period_order_value(.calc_time, Period)]

  group_cols <- c("dataset", "variable", "unique", "demo", "demography", ".calc_time")
  group_cols <- group_cols[group_cols %in% names(df)]
  data.table::setorderv(df, c(group_cols, ".period_order"), na.last = TRUE)

  df[
    ,
    Change_Net_Sat_SE := sqrt(Net_Sat_SE^2 + data.table::shift(Net_Sat_SE)^2),
    by = group_cols
  ]
  df[
    ,
    .previous_net_satisfaction_for_ci := data.table::shift(Net_Satisfaction),
    by = group_cols
  ]
  df[
    ,
    Change_Relative_To_Previous_Net_Sat := {
      denom <- abs(.previous_net_satisfaction_for_ci)
      data.table::fifelse(
        !is.na(denom) & denom > 0,
        abs(`Change (Net-Satisfaction)`) / denom,
        NA_real_
      )
    }
  ]
  df[
    ,
    .relative_bridge_threshold_abs := pmax(
      CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE,
      abs(.previous_net_satisfaction_for_ci) * CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD,
      na.rm = FALSE
    )
  ]
  df[
    ,
    .relative_bridge_flag := dplyr::case_when(
      !isTRUE(ENABLE_CI_MISSING_SE_RELATIVE_FALLBACK) ~ NA_character_,
      !is.na(Change_Net_Sat_SE) ~ NA_character_,
      is.na(`Change (Net-Satisfaction)`) | is.na(.previous_net_satisfaction_for_ci) ~ NA_character_,
      abs(`Change (Net-Satisfaction)`) < .relative_bridge_threshold_abs ~ NA_character_,
      `Change (Net-Satisfaction)` > 0 ~ "Bridge Relative Improvement",
      `Change (Net-Satisfaction)` < 0 ~ "Bridge Relative Decline",
      TRUE ~ NA_character_
    )
  ]
  df[, Change_Net_Sat_CI_Lower := `Change (Net-Satisfaction)` - 1.96 * Change_Net_Sat_SE]
  df[, Change_Net_Sat_CI_Upper := `Change (Net-Satisfaction)` + 1.96 * Change_Net_Sat_SE]
  df[
    ,
    Stat_Sig_CI := dplyr::case_when(
      is.na(Change_Net_Sat_CI_Lower) | is.na(Change_Net_Sat_CI_Upper) ~ "Not Applicable",
      Change_Net_Sat_CI_Lower > 0 ~ "Significant Improvement",
      Change_Net_Sat_CI_Upper < 0 ~ "Significant Decline",
      TRUE ~ "Not Significant"
    )
  ]
  df[
    ,
    Substantive_Change := dplyr::case_when(
      is.na(`Change (Net-Satisfaction)`) ~ "Not Applicable",
      abs(`Change (Net-Satisfaction)`) >= CI_SUBSTANTIVE_CHANGE_THRESHOLD ~ "Yes",
      TRUE ~ "No"
    )
  ]
  df[
    ,
    Change_Flag := dplyr::case_when(
      Stat_Sig_CI == "Significant Improvement" & Substantive_Change == "Yes" ~ "Significant Substantive Improvement",
      Stat_Sig_CI == "Significant Decline" & Substantive_Change == "Yes" ~ "Significant Substantive Decline",
      !is.na(.relative_bridge_flag) ~ .relative_bridge_flag,
      Stat_Sig_CI == "Not Applicable" ~ "Not Applicable",
      TRUE ~ "No Flag"
    )
  ]
  df[
    ,
    Change_Flag_Method := dplyr::case_when(
      Change_Flag %in% c("Bridge Relative Improvement", "Bridge Relative Decline") ~ "relative_missing_se_fallback",
      Change_Flag %in% c("Significant Substantive Improvement", "Significant Substantive Decline") ~ "ci",
      Stat_Sig_CI %in% c("Significant Improvement", "Significant Decline", "Not Significant") ~ "ci_no_dashboard_flag",
      Stat_Sig_CI == "Not Applicable" ~ "not_applicable",
      TRUE ~ NA_character_
    )
  ]

  df[, c(".calc_time", ".period_order", ".previous_net_satisfaction_for_ci", ".relative_bridge_threshold_abs", ".relative_bridge_flag") := NULL]
  df[]
}

extract_year_from_period <- function(period) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(period), "20[0-9]{2}")))
}

previous_quarter_label <- function(period) {
  period <- as.character(period)
  m <- stringr::str_match(period, "^(20[0-9]{2})\\s*Q\\s*([1-4])$")
  yy <- suppressWarnings(as.integer(m[, 2]))
  qq <- suppressWarnings(as.integer(m[, 3]))
  out <- rep(NA_character_, length(period))
  ok <- !is.na(yy) & !is.na(qq)
  prev_y <- yy
  prev_q <- qq - 1L
  prev_y[ok & prev_q == 0L] <- prev_y[ok & prev_q == 0L] - 1L
  prev_q[ok & prev_q == 0L] <- 4L
  out[ok] <- paste0(prev_y[ok], " Q", prev_q[ok])
  out
}

period_to_quarter_label <- function(time, period) {
  time <- time_for_calc(time)
  period <- as.character(period)
  out <- rep(NA_character_, length(period))

  quarter_idx <- time == "Quarter"
  out[quarter_idx] <- period[quarter_idx]

  month_idx <- time == "Month"
  if (any(month_idx, na.rm = TRUE)) {
    yy <- extract_year_from_period(period[month_idx])
    mon <- stringr::str_match(period[month_idx], "20[0-9]{2}\\s+([A-Za-z]{3})")[, 2]
    mm <- match(stringr::str_to_title(mon), month.abb)
    qq <- ceiling(mm / 3)
    ok <- !is.na(yy) & !is.na(qq)
    tmp <- rep(NA_character_, sum(month_idx, na.rm = TRUE))
    tmp[ok] <- paste0(yy[ok], " Q", qq[ok])
    out[month_idx] <- tmp
  }

  week_idx <- time == "Week"
  if (any(week_idx, na.rm = TRUE)) {
    yy <- extract_year_from_period(period[week_idx])
    ww <- suppressWarnings(as.integer(stringr::str_match(period[week_idx], "W\\s*([0-9]{1,2})")[, 2]))
    qq <- pmin(4L, pmax(1L, ceiling(ww / 13)))
    ok <- !is.na(yy) & !is.na(ww)
    tmp <- rep(NA_character_, sum(week_idx, na.rm = TRUE))
    tmp[ok] <- paste0(yy[ok], " Q", qq[ok])
    out[week_idx] <- tmp
  }

  out
}

legacy_t_test_label <- function(current_values, baseline_values, improve_label, decline_label, no_change_label, insufficient_label) {
  current_values <- safe_as_numeric(current_values)
  baseline_values <- safe_as_numeric(baseline_values)
  current_values <- current_values[!is.na(current_values)]
  baseline_values <- baseline_values[!is.na(baseline_values)]

  if (length(current_values) < 2 || length(baseline_values) < 2) {
    return(insufficient_label)
  }

  test_out <- tryCatch(
    stats::t.test(current_values, baseline_values),
    error = function(e) NULL
  )
  if (is.null(test_out) || is.na(test_out$p.value)) {
    return(insufficient_label)
  }

  current_mean <- mean(current_values, na.rm = TRUE)
  baseline_mean <- mean(baseline_values, na.rm = TRUE)
  p_cutoff <- OLD_STAT_SIG_PVALUE_CUTOFF %||% 0.10

  if (test_out$p.value < p_cutoff && current_mean > baseline_mean) {
    return(improve_label)
  }
  if (test_out$p.value < p_cutoff && current_mean < baseline_mean) {
    return(decline_label)
  }

  no_change_label
}

calculate_legacy_annual_stat_sig <- function(test_dt, test_period) {
  test_dt <- data.table::as.data.table(test_dt)
  if (nrow(test_dt) == 0 || !all(c("Time", "Period", "Net_Satisfaction") %in% names(test_dt))) {
    return(data.table::data.table())
  }

  test_dt <- data.table::copy(test_dt[time_for_calc(Time) == test_period])
  if (nrow(test_dt) == 0) {
    return(data.table::data.table())
  }

  key_cols <- c("dataset", "variable", "unique", "demo", "demography")
  key_cols <- key_cols[key_cols %in% names(test_dt)]
  test_dt[, Net_Satisfaction := safe_as_numeric(Net_Satisfaction)]
  test_dt[, period_year := extract_year_from_period(Period)]
  test_dt <- test_dt[!is.na(period_year)]
  if (nrow(test_dt) == 0) {
    return(data.table::data.table())
  }

  targets <- unique(test_dt[, c(key_cols, "period_year"), with = FALSE])
  lookback_years <- PRECEDING_YEARS_FOR_BASELINE %||% 5
  out <- vector("list", nrow(targets))

  for (i in seq_len(nrow(targets))) {
    target <- targets[i]
    group_rows <- rep(TRUE, nrow(test_dt))
    for (cc in key_cols) {
      group_rows <- group_rows & test_dt[[cc]] == target[[cc]]
    }
    yy <- target$period_year[[1]]
    group_dt <- test_dt[group_rows]
    out[[i]] <- data.table::data.table(
      target,
      test_period = test_period,
      old_stat_sig_label = legacy_t_test_label(
        current_values = group_dt[period_year == yy, Net_Satisfaction],
        baseline_values = group_dt[period_year >= yy - lookback_years & period_year <= yy - 1L, Net_Satisfaction],
        improve_label = c360_label("OLD_STAT_SIG_LABEL_IMPROVE", "Significant & Substantive Improvement"),
        decline_label = c360_label("OLD_STAT_SIG_LABEL_DECLINE", "Significant & Substantive Decline"),
        no_change_label = c360_label("OLD_STAT_SIG_LABEL_NO_CHANGE", "No Significant & Substantive Change"),
        insufficient_label = c360_label("OLD_STAT_SIG_LABEL_INSUFFICIENT", "Insufficient Baseline Data")
      )
    )
  }

  data.table::rbindlist(out, fill = TRUE)
}

combine_legacy_annual_stat_sig <- function(week_sig, month_sig) {
  key_cols <- c("dataset", "variable", "unique", "demo", "demography", "period_year")
  key_cols <- key_cols[key_cols %in% names(week_sig) | key_cols %in% names(month_sig)]
  if (nrow(week_sig) == 0 && nrow(month_sig) == 0) {
    return(data.table::data.table())
  }

  if (nrow(week_sig) > 0) {
    week_sig <- week_sig[, c(key_cols, "old_stat_sig_label"), with = FALSE]
    data.table::setnames(week_sig, "old_stat_sig_label", "Old_Stat_Sig_Week")
  }
  if (nrow(month_sig) > 0) {
    month_sig <- month_sig[, c(key_cols, "old_stat_sig_label"), with = FALSE]
    data.table::setnames(month_sig, "old_stat_sig_label", "Old_Stat_Sig_Month")
  }

  combined <- if (nrow(week_sig) == 0) {
    month_sig
  } else if (nrow(month_sig) == 0) {
    week_sig
  } else {
    merge(week_sig, month_sig, by = key_cols, all = TRUE)
  }

  no_change_label <- c360_label("OLD_STAT_SIG_LABEL_NO_CHANGE", "No Significant & Substantive Change")
  insufficient_label <- c360_label("OLD_STAT_SIG_LABEL_INSUFFICIENT", "Insufficient Baseline Data")
  combined[
    ,
    Old_Stat_Sig := dplyr::case_when(
      !is.na(Old_Stat_Sig_Week) &
        !(Old_Stat_Sig_Week %in% c(no_change_label, insufficient_label)) ~ Old_Stat_Sig_Week,
      !is.na(Old_Stat_Sig_Month) ~ Old_Stat_Sig_Month,
      TRUE ~ Old_Stat_Sig_Week
    )
  ]
  combined
}

calculate_legacy_quarterly_pog_sig <- function(test_dt, new_dt) {
  test_dt <- data.table::as.data.table(test_dt)
  new_dt <- data.table::as.data.table(new_dt)
  empty <- data.table::data.table()
  if (
    nrow(test_dt) == 0 || nrow(new_dt) == 0 ||
      !all(c("Time", "Period", "Net_Satisfaction") %in% names(test_dt)) ||
      !all(c("Time", "Period", "Tier1") %in% names(new_dt))
  ) {
    return(empty)
  }

  pog_label <- stringr::str_to_lower(stringr::str_squish("Perception of Govt"))
  pog_rows <- stringr::str_to_lower(stringr::str_squish(as.character(new_dt$Tier1))) == pog_label &
    time_for_calc(new_dt$Time) == "Quarter"
  if (!any(pog_rows, na.rm = TRUE)) {
    return(empty)
  }

  key_cols <- c("dataset", "variable", "unique", "demo", "demography")
  key_cols <- key_cols[key_cols %in% names(test_dt) & key_cols %in% names(new_dt)]
  target_rows <- unique(new_dt[pog_rows, c(key_cols, "Period", "Change (Net-Satisfaction)", "SD (Preceding 2Q - Net)"), with = FALSE])
  if (nrow(target_rows) == 0) {
    return(empty)
  }

  work <- data.table::copy(test_dt[time_for_calc(Time) == "Week"])
  if (nrow(work) == 0) {
    work <- data.table::copy(test_dt[time_for_calc(Time) == "Month"])
  }
  if (nrow(work) == 0) {
    return(empty)
  }

  work[, Net_Satisfaction := safe_as_numeric(Net_Satisfaction)]
  work[, quarter_period := period_to_quarter_label(Time, Period)]
  work <- work[!is.na(quarter_period)]
  if (nrow(work) == 0) {
    return(empty)
  }

  target_rows[, previous_quarter := previous_quarter_label(Period)]
  out <- vector("list", nrow(target_rows))
  for (i in seq_len(nrow(target_rows))) {
    target <- target_rows[i]
    group_rows <- rep(TRUE, nrow(work))
    for (cc in key_cols) {
      group_rows <- group_rows & work[[cc]] == target[[cc]]
    }
    group_dt <- work[group_rows]
    label <- legacy_t_test_label(
      current_values = group_dt[quarter_period == target$Period[[1]], Net_Satisfaction],
      baseline_values = group_dt[quarter_period == target$previous_quarter[[1]], Net_Satisfaction],
      improve_label = "Improved",
      decline_label = "Worsen",
      no_change_label = "No Significant Difference",
      insufficient_label = c360_label("OLD_STAT_SIG_LABEL_INSUFFICIENT", "Insufficient Baseline Data")
    )

    change_abs <- abs(safe_as_numeric(target[["Change (Net-Satisfaction)"]]))
    sd_net <- safe_as_numeric(target[["SD (Preceding 2Q - Net)"]])
    if (label %in% c("Improved", "Worsen") && !is.na(change_abs) && !is.na(sd_net) && change_abs < sd_net) {
      label <- "No Significant Difference"
    }

    out[[i]] <- data.table::data.table(target[, ..key_cols], Period = target$Period, Old_Quarter_Pog_Sig = label)
  }

  data.table::rbindlist(out, fill = TRUE)
}

add_legacy_stat_sig_to_new_update <- function(new_dt, test_dt) {
  new_dt <- data.table::as.data.table(new_dt)
  test_dt <- data.table::as.data.table(test_dt)
  if (nrow(new_dt) == 0 || nrow(test_dt) == 0) {
    return(new_dt)
  }

  key_cols <- c("dataset", "variable", "unique", "demo", "demography")
  key_cols <- key_cols[key_cols %in% names(new_dt) & key_cols %in% names(test_dt)]

  annual_rows <- time_for_calc(new_dt$Time) == "Year"
  if (any(annual_rows, na.rm = TRUE)) {
    week_sig <- calculate_legacy_annual_stat_sig(test_dt, "Week")
    month_sig <- calculate_legacy_annual_stat_sig(test_dt, "Month")
    annual_sig <- combine_legacy_annual_stat_sig(week_sig, month_sig)
    if (nrow(annual_sig) > 0) {
      new_dt[, period_year := extract_year_from_period(Period)]
      annual_sig <- annual_sig[, c(key_cols, "period_year", "Old_Stat_Sig"), with = FALSE]
      new_dt <- merge(new_dt, annual_sig, by = c(key_cols, "period_year"), all.x = TRUE, sort = FALSE)
      new_dt[annual_rows & !is.na(Old_Stat_Sig) & Old_Stat_Sig != "", Trend := Old_Stat_Sig]
      new_dt[, c("period_year", "Old_Stat_Sig") := NULL]
    }
  }

  quarter_sig <- calculate_legacy_quarterly_pog_sig(test_dt, new_dt)
  if (nrow(quarter_sig) > 0) {
    quarter_sig <- quarter_sig[, c(key_cols, "Period", "Old_Quarter_Pog_Sig"), with = FALSE]
    new_dt <- merge(new_dt, quarter_sig, by = c(key_cols, "Period"), all.x = TRUE, sort = FALSE)
    new_dt[!is.na(Old_Quarter_Pog_Sig) & Old_Quarter_Pog_Sig != "", Trend := Old_Quarter_Pog_Sig]
    new_dt[, Old_Quarter_Pog_Sig := NULL]
  }

  new_dt[, Trend := legacy_dashboard_trend_label(Trend)]
  add_legacy_trend_count_fields(new_dt)
}

target_quarter_export_guard <- function(df) {
  dt <- data.table::copy(data.table::as.data.table(df))
  empty_log <- data.table::data.table(
    target_quarter = character(),
    issue_type = character(),
    Time = character(),
    Period = character(),
    rows = integer(),
    action = character()
  )
  if (!target_quarter_enabled() || nrow(dt) == 0 || !all(c("Time", "Period") %in% names(dt))) {
    return(list(data = dt, log = empty_log))
  }

  target_quarter <- get("TARGET_UPDATE_QUARTER", inherits = TRUE)
  dt[, .calc_time := time_for_calc(Time)]
  off_target <- dt[.calc_time == "Quarter" & Period != target_quarter]
  log <- if (nrow(off_target) > 0) {
    off_target[
      ,
      .(
        target_quarter = target_quarter,
        issue_type = "current_update_quarter_outside_target_retained",
        rows = .N,
        action = "Retained because explicit source dates determine the true quarter. Review if unexpected."
      ),
      by = .(Time, Period)
    ]
  } else {
    dt[
      .calc_time == "Quarter",
      .(
        target_quarter = target_quarter,
        issue_type = "current_update_quarter_within_target",
        rows = .N,
        action = "Proceed."
      ),
      by = .(Time, Period)
    ]
  }
  if (nrow(log) == 0) log <- empty_log

  dt[, .calc_time := NULL]
  list(data = dt, log = log)
}

ci_latest_quarter_periods <- function(new_update) {
  if (!isTRUE(ENABLE_CI_SIGNIFICANCE_OUTPUT)) {
    return(character())
  }

  if (identical(SIGNIFICANCE_FLAGGING_APPROACH, "ci")) {
    return("__ALL__")
  }

  dt <- data.table::as.data.table(new_update)
  if (nrow(dt) == 0 || !all(c("Time", "Period") %in% names(dt))) {
    return(character())
  }

  q <- unique(dt[time_for_calc(Time) == "Quarter" & !is.na(Period) & Period != "", .(Period)])
  if (nrow(q) == 0) {
    return(character())
  }
  q[, period_order := period_order_value("Quarter", Period)]
  q <- q[!is.na(period_order)]
  if (nrow(q) == 0) {
    return(character())
  }
  q[period_order == max(period_order), Period]
}

apply_ci_flags_to_latest_rows <- function(df, new_update) {
  dt <- data.table::as.data.table(df)
  if (nrow(dt) == 0 || !isTRUE(ENABLE_CI_SIGNIFICANCE_OUTPUT) || !("Change_Flag" %in% names(dt))) {
    return(dt)
  }

  periods <- ci_latest_quarter_periods(new_update)
  if (length(periods) == 0) {
    return(dt)
  }

  if (identical(periods, "__ALL__")) {
    idx <- seq_len(nrow(dt))
  } else {
    keys <- REPLACE_KEYS %||% c("dataset", "unique", "demo", "demography", "Time", "Period")
    keys <- keys[keys %in% names(dt) & keys %in% names(new_update)]
    new_keys <- data.table::as.data.table(new_update)
    new_keys <- new_keys[time_for_calc(Time) == "Quarter" & Period %in% periods]
    if (length(keys) == 0 || nrow(new_keys) == 0) {
      return(dt)
    }
    new_keys <- unique(new_keys[, ..keys])
    idx <- dt[new_keys, on = keys, which = TRUE]
    idx <- idx[!is.na(idx)]
  }

  if (length(idx) == 0) {
    return(dt)
  }

  raw_change_flag <- stringr::str_squish(as.character(dt$Change_Flag[idx]))
  override_raw_flags <- c(
    "Significant Substantive Improvement",
    "Significant Substantive Decline",
    "Significant & Substantive Improvement",
    "Significant & Substantive Decline",
    "Bridge Relative Improvement",
    "Bridge Relative Decline"
  )
  override_idx <- idx[raw_change_flag %in% override_raw_flags]
  if (length(override_idx) > 0) {
    dt[override_idx, Trend := legacy_dashboard_trend_label(Change_Flag)]
    dt <- add_legacy_trend_count_fields(dt)
  }

  dt[]
}

build_input_reference <- function(processed_files_for_manifest) {
  processed_files_for_manifest <- data.table::as.data.table(processed_files_for_manifest)
  if (nrow(processed_files_for_manifest) == 0) {
    return(NA_character_)
  }

  paste(
    paste(
      processed_files_for_manifest$survey,
      basename(processed_files_for_manifest$file_path),
      processed_files_for_manifest$file_mtime,
      sep = ":"
    ),
    collapse = " | "
  )
}

round_output_fields <- function(df) {
  df <- data.table::as.data.table(df)

  one_dp_cols <- c(
    "Agree", "Disagree", "Neutral", "Net_Satisfaction",
    "Change (Agree)", "Change (Net-Satisfaction)",
    "Annual Change (Net-Satisfaction)",
    "Baseline Reference (Preceding 5 Year Mean)",
    "Deviation from Reference",
    "Baseline Deviation (Preceding 5 Year Absolute Mean)",
    "%Strongly Disagree", "%Disgree", "%Neutral", "%Agree",
    "%Strongly Agree", "%Net-Satisfaction",
    "Overall %Net-Satisfaction", "Diff %Net-Sat (Demo - Overall)",
    "Net_Sat_SE", "Net_Sat_CI_Lower", "Net_Sat_CI_Upper",
    "Change_Net_Sat_SE", "Change_Net_Sat_CI_Lower",
    "Change_Net_Sat_CI_Upper",
    "1", "2", "3", "4", "5", "total.sample.n", "effective_n"
  )
  three_dp_cols <- c(
    "SD (Preceding 2Q - Agree)", "SD (Preceding 2Q - Net)",
    "Std Dev within Demo", "f0", "f1", "f2a", "f2b", "f3", "f4",
    "b2b", "b3", "b4", "Polarisation_Index",
    "Change_Relative_To_Previous_Net_Sat"
  )
  integer_cols <- c(
    "unweighted_n", "respondent_n",
    "Sensitivity Count - Consective Decline (Net-Sat)",
    "Sensitivity Count - Consective Improved (Net-Sat)",
    "Sensitivity Count - No Consecutive Changes (Net-Sat)",
    "Sensitivity Count - No Change (Net-Sat)",
    "Sensitivity Count - Improvement (Net-Sat)",
    "Sensitivity Count - Decline (Net-Sat)"
  )

  for (cc in intersect(one_dp_cols, names(df))) {
    df[[cc]] <- round(safe_as_numeric(df[[cc]]), 1)
  }
  for (cc in intersect(three_dp_cols, names(df))) {
    df[[cc]] <- round(safe_as_numeric(df[[cc]]), 3)
  }
  for (cc in intersect(integer_cols, names(df))) {
    df[[cc]] <- round(safe_as_numeric(df[[cc]]), 0)
  }

  df
}

select_output_columns_exact <- function(df, cols) {
  df <- data.table::as.data.table(df)
  df <- add_missing_columns(df, cols)
  df[, ..cols]
}

make_legacy_tableau_shape <- function(df, cols) {
  df <- data.table::copy(data.table::as.data.table(df))
  df <- normalise_time_for_output(df)
  select_output_columns_exact(df, cols)
}

apply_registry_export_flags <- function(df, registry_dt) {
  dt <- data.table::copy(data.table::as.data.table(df))
  invisible(data.table::setalloccol(dt))
  if (nrow(dt) == 0) {
    return(dt)
  }

  dt <- add_missing_columns(dt, c("dataset", "unique", "include_in_dashboard", "Keyqn", "key_question_flag"))
  for (cc in c("dataset", "unique", "include_in_dashboard", "Keyqn", "key_question_flag")) {
    dt[[cc]] <- as.character(dt[[cc]])
  }

  registry_dt <- data.table::copy(data.table::as.data.table(registry_dt %||% data.table::data.table()))
  invisible(data.table::setalloccol(registry_dt))
  if (nrow(registry_dt) == 0 || !all(c("dataset", "unique") %in% names(registry_dt))) {
    dt[is.na(include_in_dashboard) | include_in_dashboard == "", include_in_dashboard := "Include"]
    dt[is.na(Keyqn) | Keyqn == "", Keyqn := "No"]
    dt[is.na(key_question_flag) | key_question_flag == "", key_question_flag := Keyqn]
    return(dt)
  }

  registry_dt <- add_missing_columns(registry_dt, c("include_in_dashboard", "Keyqn", "key_question_flag"))
  registry_dt[, .join_dataset := tolower(trimws(as.character(dataset)))]
  registry_dt[, .join_unique := trimws(as.character(unique))]
  registry_flags <- registry_dt[
    !is.na(.join_dataset) & .join_dataset != "" & !is.na(.join_unique) & .join_unique != "",
    .(
      registry_include_in_dashboard = if (any(is_dashboard_include(include_in_dashboard), na.rm = TRUE)) "Include" else "Exclude",
      registry_key_question_flag = if (any(is_key_yes(Keyqn) | is_key_yes(key_question_flag), na.rm = TRUE)) "Yes" else "No"
    ),
    by = .(.join_dataset, .join_unique)
  ]

  if (nrow(registry_flags) == 0) {
    dt[is.na(include_in_dashboard) | include_in_dashboard == "", include_in_dashboard := "Include"]
    dt[is.na(Keyqn) | Keyqn == "", Keyqn := "No"]
    dt[is.na(key_question_flag) | key_question_flag == "", key_question_flag := Keyqn]
    return(dt)
  }

  data.table::set(dt, j = ".row_order", value = seq_len(nrow(dt)))
  dt[, .join_dataset := tolower(trimws(as.character(dataset)))]
  dt[, .join_unique := trimws(as.character(unique))]
  dt <- merge(
    dt,
    registry_flags,
    by = c(".join_dataset", ".join_unique"),
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(dt, .row_order)

  matched_registry <- !is.na(dt$registry_include_in_dashboard)
  dt[matched_registry, include_in_dashboard := registry_include_in_dashboard]
  dt[matched_registry, Keyqn := registry_key_question_flag]
  dt[matched_registry, key_question_flag := registry_key_question_flag]
  dt[!matched_registry, include_in_dashboard := "Exclude"]
  dt[!matched_registry, Keyqn := "No"]
  dt[!matched_registry, key_question_flag := "No"]
  dt[is.na(Keyqn) | Keyqn == "", Keyqn := "No"]
  dt[is.na(key_question_flag) | key_question_flag == "", key_question_flag := Keyqn]

  dt[
    ,
    c(
      ".row_order", ".join_dataset", ".join_unique",
      "registry_include_in_dashboard", "registry_key_question_flag"
    ) := NULL
  ]
  dt
}

filter_external_tableau_rows <- function(df) {
  dt <- data.table::copy(data.table::as.data.table(df))
  if (nrow(dt) == 0) {
    return(dt)
  }

  dt <- dt[is_dashboard_include(include_in_dashboard)]

  excluded_demos <- tolower(EXTERNAL_TABLEAU_EXCLUDE_DEMOS %||% character())
  if (length(excluded_demos) > 0 && "demo" %in% names(dt)) {
    dt <- dt[!(tolower(trimws(as.character(demo))) %in% excluded_demos)]
  }

  excluded_demographies <- EXTERNAL_TABLEAU_EXCLUDE_DEMOGRAPHIES %||% character()
  if (length(excluded_demographies) > 0 && "demography" %in% names(dt)) {
    dt <- dt[!(trimws(as.character(demography)) %in% excluded_demographies)]
  }

  dt
}

tableau_export_log <- function(internal_wide, internal_trimmed, external_trimmed) {
  data.table::data.table(
    output = c(
      "internal",
      "internal_trimmed",
      "external_trimmed"
    ),
    path = c(
      INTERNAL_TABLEAU_WIDE_CSV,
      INTERNAL_TABLEAU_TRIMMED_CSV,
      EXTERNAL_TABLEAU_TRIMMED_CSV
    ),
    rows = c(
      nrow(internal_wide),
      nrow(internal_trimmed),
      nrow(external_trimmed)
    ),
    columns = c(
      ncol(internal_wide),
      ncol(internal_trimmed),
      ncol(external_trimmed)
    )
  )
}

schema_qa_row <- function(output_name, df, expected_cols, forbidden_cols = character()) {
  df <- data.table::as.data.table(df)
  data.table::data.table(
    output = output_name,
    rows = nrow(df),
    columns = ncol(df),
    unique_question_ids = if ("unique" %in% names(df)) data.table::uniqueN(df$unique, na.rm = TRUE) else NA_integer_,
    unique_question_text = if ("question" %in% names(df)) data.table::uniqueN(df$question, na.rm = TRUE) else NA_integer_,
    column_order_matches_expected = identical(names(df), expected_cols),
    missing_expected_columns = paste(setdiff(expected_cols, names(df)), collapse = " | "),
    extra_columns = paste(setdiff(names(df), expected_cols), collapse = " | "),
    forbidden_columns_present = paste(intersect(names(df), forbidden_cols), collapse = " | "),
    contains_year_time_label = if ("Time" %in% names(df)) any(df$Time == "Year", na.rm = TRUE) else NA,
    contains_annual_time_label = if ("Time" %in% names(df)) any(df$Time == OUTPUT_YEAR_TIME_LABEL, na.rm = TRUE) else NA,
    rows_2026_q1 = if ("Period" %in% names(df)) sum(df$Period == "2026 Q1", na.rm = TRUE) else NA_integer_
  )
}

if (!exists("load_manifest")) {
  load_manifest <- function(path) {
    if (!file.exists(path)) {
      return(data.table::data.table())
    }
    data.table::as.data.table(readRDS(path))
  }
}

existing_output <- load_existing_c360_csv(EXISTING_C360_CSV)
existing_output <- data.table::as.data.table(existing_output)

if (nrow(existing_output) > 0) {
  existing_output <- add_missing_columns(existing_output, c("include_in_dashboard", "Keyqn", "key_question_flag"))
  for (cc in c("include_in_dashboard", "Keyqn", "key_question_flag")) {
    existing_output[[cc]] <- as.character(existing_output[[cc]])
  }
  existing_output[is.na(include_in_dashboard) | include_in_dashboard == "", include_in_dashboard := "Include"]
  existing_output[is.na(Keyqn) | Keyqn == "", Keyqn := "No"]
  existing_output[is.na(key_question_flag) | key_question_flag == "", key_question_flag := Keyqn]
}

new_update <- data.table::as.data.table(dashboard_agg)
new_update <- normalise_time_for_output(new_update)
target_quarter_export_guard_result <- target_quarter_export_guard(new_update)
new_update <- target_quarter_export_guard_result$data
target_quarter_export_guard_log <- target_quarter_export_guard_result$log
write_log(
  target_quarter_export_guard_log,
  file.path(LOG_DIR, paste0(Sys.Date(), "_05_target_quarter_export_guard.csv"))
)

if (nrow(new_update) > 0) {
  input_reference <- build_input_reference(processed_files_for_manifest)
  for (cc in c("Last_Processed", "Input_Reference")) {
    if (!(cc %in% names(new_update))) {
      new_update[[cc]] <- NA_character_
    } else {
      new_update[[cc]] <- as.character(new_update[[cc]])
    }
  }
  new_update[, Last_Processed := format(Sys.time(), "%d/%m/%Y %H:%M")]
  new_update[, Input_Reference := input_reference]
}

replace_keys <- REPLACE_KEYS %||% c("dataset", "unique", "demo", "demography", "Time", "Period")
existing_output_rows_before_bolt_on <- nrow(existing_output)
new_output_rows_from_current_run <- nrow(new_update)
existing_for_context <- remove_rows_matching_new(existing_output, new_update, replace_keys)
existing_output_rows_replaced <- existing_output_rows_before_bolt_on - nrow(existing_for_context)

if (nrow(existing_for_context) > 0) {
  existing_for_context[, .is_new_update := FALSE]
}
if (nrow(new_update) > 0) {
  new_update[, .is_new_update := TRUE]
}

calc_context <- data.table::rbindlist(
  list(existing_for_context, new_update),
  fill = TRUE
)

if (nrow(calc_context) > 0) {
  calc_context <- calculate_change_sensitivity_baseline(calc_context)
  new_update_enriched <- calc_context[.is_new_update == TRUE]
  new_update_enriched <- add_legacy_stat_sig_to_new_update(new_update_enriched, old_stat_sig_test_agg)
} else {
  new_update_enriched <- data.table::data.table()
}

existing_kept <- remove_rows_matching_new(existing_output, new_update_enriched, replace_keys)
final_output <- data.table::rbindlist(
  list(existing_kept, new_update_enriched),
  fill = TRUE
)
drop_internal_cols <- intersect(c(".is_new_update"), names(final_output))
if (length(drop_internal_cols) > 0) {
  final_output[, (drop_internal_cols) := NULL]
}
final_output <- apply_registry_export_flags(
  final_output,
  if (exists("registry")) registry else data.table::data.table()
)
net_output_row_change <- nrow(final_output) - existing_output_rows_before_bolt_on

if (nrow(final_output) > 0 && all(c("Time", "Period") %in% names(final_output))) {
  final_output[, .period_order_export := period_order_value(time_for_calc(Time), Period)]
  sort_cols <- intersect(
    c("dataset", "Time", ".period_order_export", "demo", "demography", "variable", "unique"),
    names(final_output)
  )
  data.table::setorderv(final_output, sort_cols, na.last = TRUE)
  final_output[, .period_order_export := NULL]
}

final_output <- round_output_fields(final_output)
final_output <- add_ci_significance_fields(final_output)
final_output <- apply_ci_flags_to_latest_rows(final_output, new_update)
final_output <- round_output_fields(final_output)

full_output_columns <- if (isTRUE(ENABLE_CI_SIGNIFICANCE_OUTPUT)) {
  unique(c(FULL_INTERNAL_COLUMNS, CI_SIGNIFICANCE_COLUMNS))
} else {
  FULL_INTERNAL_COLUMNS
}

full_output <- select_output_columns_exact(final_output, full_output_columns)
internal_tableau_wide <- make_legacy_tableau_shape(final_output, LEGACY_WIDE_COLUMNS)
internal_tableau_trimmed <- make_legacy_tableau_shape(
  internal_tableau_wide,
  LEGACY_TRIMMED_COLUMNS
)

external_tableau_source <- filter_external_tableau_rows(final_output)
external_tableau_trimmed <- make_legacy_tableau_shape(
  external_tableau_source,
  LEGACY_TRIMMED_COLUMNS
)

# Keep the older dashboard-included filename as a compatibility alias, but make
# it the same legacy-trimmed shape used by the external Tableau workbook.
dashboard_output <- external_tableau_trimmed

key_question_output <- final_output[is_key_yes(Keyqn) | is_key_yes(key_question_flag)]
key_question_output <- select_output_columns_exact(key_question_output, FULL_INTERNAL_COLUMNS)

data.table::fwrite(full_output, FULL_OUTPUT_CSV, na = "")
data.table::fwrite(dashboard_output, DASHBOARD_OUTPUT_CSV, na = "")
data.table::fwrite(key_question_output, KEY_QUESTION_OUTPUT_CSV, na = "")
data.table::fwrite(internal_tableau_wide, INTERNAL_TABLEAU_WIDE_CSV, na = "")
data.table::fwrite(internal_tableau_trimmed, INTERNAL_TABLEAU_TRIMMED_CSV, na = "")
data.table::fwrite(external_tableau_trimmed, EXTERNAL_TABLEAU_TRIMMED_CSV, na = "")

tableau_exports <- tableau_export_log(
  internal_tableau_wide,
  internal_tableau_trimmed,
  external_tableau_trimmed
)

manifest_existing <- load_manifest(MANIFEST_PATH)
if (nrow(processed_files_for_manifest) > 0) {
  manifest_updated <- data.table::rbindlist(
    list(manifest_existing, processed_files_for_manifest),
    fill = TRUE
  )
  if ("file_path" %in% names(manifest_updated)) {
    data.table::setorder(manifest_updated, file_path, processed_at)
    manifest_updated <- manifest_updated[!duplicated(file_path, fromLast = TRUE)]
  }
  saveRDS(manifest_updated, MANIFEST_PATH)
} else if (!file.exists(MANIFEST_PATH)) {
  saveRDS(manifest_existing, MANIFEST_PATH)
}

unique_n_col <- function(df, col) {
  df <- data.table::as.data.table(df)
  if (!(col %in% names(df))) {
    return(NA_integer_)
  }
  data.table::uniqueN(df[[col]], na.rm = TRUE)
}

output_summary <- data.table::data.table(
  output = c(
    "bolt_on_row_accounting",
    "full_all_registry",
    "dashboard_included_legacy_trimmed_alias",
    "key_questions",
    "internal",
    "internal_trimmed",
    "external_trimmed"
  ),
  path = c(
    FULL_OUTPUT_CSV,
    FULL_OUTPUT_CSV,
    DASHBOARD_OUTPUT_CSV,
    KEY_QUESTION_OUTPUT_CSV,
    INTERNAL_TABLEAU_WIDE_CSV,
    INTERNAL_TABLEAU_TRIMMED_CSV,
    EXTERNAL_TABLEAU_TRIMMED_CSV
  ),
  rows = c(
    nrow(final_output),
    nrow(full_output),
    nrow(dashboard_output),
    nrow(key_question_output),
    nrow(internal_tableau_wide),
    nrow(internal_tableau_trimmed),
    nrow(external_tableau_trimmed)
  ),
  columns = c(
    ncol(final_output),
    ncol(full_output),
    ncol(dashboard_output),
    ncol(key_question_output),
    ncol(internal_tableau_wide),
    ncol(internal_tableau_trimmed),
    ncol(external_tableau_trimmed)
  ),
  unique_question_ids = c(
    unique_n_col(final_output, "unique"),
    unique_n_col(full_output, "unique"),
    unique_n_col(dashboard_output, "unique"),
    unique_n_col(key_question_output, "unique"),
    unique_n_col(internal_tableau_wide, "unique"),
    unique_n_col(internal_tableau_trimmed, "unique"),
    unique_n_col(external_tableau_trimmed, "unique")
  ),
  unique_question_text = c(
    unique_n_col(final_output, "question"),
    unique_n_col(full_output, "question"),
    unique_n_col(dashboard_output, "question"),
    unique_n_col(key_question_output, "question"),
    unique_n_col(internal_tableau_wide, "question"),
    unique_n_col(internal_tableau_trimmed, "question"),
    unique_n_col(external_tableau_trimmed, "question")
  ),
  rows_2026_q1 = c(
    sum(final_output$Period == "2026 Q1", na.rm = TRUE),
    sum(full_output$Period == "2026 Q1", na.rm = TRUE),
    sum(dashboard_output$Period == "2026 Q1", na.rm = TRUE),
    sum(key_question_output$Period == "2026 Q1", na.rm = TRUE),
    sum(internal_tableau_wide$Period == "2026 Q1", na.rm = TRUE),
    sum(internal_tableau_trimmed$Period == "2026 Q1", na.rm = TRUE),
    sum(external_tableau_trimmed$Period == "2026 Q1", na.rm = TRUE)
  )
)

bolt_on_row_accounting <- data.table::data.table(
  existing_output_rows_before_bolt_on = existing_output_rows_before_bolt_on,
  existing_output_rows_replaced = existing_output_rows_replaced,
  new_output_rows_from_current_run = new_output_rows_from_current_run,
  final_output_rows_after_bolt_on = nrow(final_output),
  net_output_row_change = net_output_row_change,
  replacement_key = paste(replace_keys, collapse = " | ")
)

final_output_dt <- data.table::as.data.table(final_output)

registry_dt_for_qa <- if (exists("registry")) data.table::as.data.table(registry) else data.table::data.table()
registry_unique_count <- if ("unique" %in% names(registry_dt_for_qa)) {
  data.table::uniqueN(registry_dt_for_qa$unique, na.rm = TRUE)
} else {
  NA_integer_
}
registry_dashboard_count <- if (all(c("unique", "include_in_dashboard") %in% names(registry_dt_for_qa))) {
  data.table::uniqueN(registry_dt_for_qa[is_dashboard_include(include_in_dashboard), unique], na.rm = TRUE)
} else {
  NA_integer_
}
registry_key_count <- if (all(c("unique", "Keyqn") %in% names(registry_dt_for_qa))) {
  data.table::uniqueN(registry_dt_for_qa[is_key_yes(Keyqn), unique], na.rm = TRUE)
} else {
  NA_integer_
}

key_dashboard_crosscheck <- final_output_dt[
  ,
  .(
    registry_rows = nrow(registry_dt_for_qa),
    registry_unique_questions = registry_unique_count,
    registry_dashboard_included_questions = registry_dashboard_count,
    registry_key_questions = registry_key_count,
    processed_questions = data.table::uniqueN(unique, na.rm = TRUE),
    key_questions = data.table::uniqueN(unique[is_key_yes(Keyqn) | is_key_yes(key_question_flag)]),
    dashboard_included_questions = data.table::uniqueN(unique[is_dashboard_include(include_in_dashboard)]),
    external_trimmed_questions = data.table::uniqueN(external_tableau_trimmed$unique, na.rm = TRUE),
    old_external_reference_questions = 157L,
    external_trimmed_difference_from_old_157 =
      data.table::uniqueN(external_tableau_trimmed$unique, na.rm = TRUE) - 157L,
    key_but_dashboard_excluded = data.table::uniqueN(unique[
      (is_key_yes(Keyqn) | is_key_yes(key_question_flag)) & !is_dashboard_include(include_in_dashboard)
    ])
  )
]

dashboard_include_ids <- unique(final_output_dt[is_dashboard_include(include_in_dashboard), unique])
external_trimmed_ids <- unique(external_tableau_trimmed$unique)
key_excluded_ids <- unique(final_output_dt[
  (is_key_yes(Keyqn) | is_key_yes(key_question_flag)) & !is_dashboard_include(include_in_dashboard),
  unique
])

membership_qa <- data.table::rbindlist(
  list(
    unique(final_output_dt[
      unique %in% key_excluded_ids,
      .(
        issue = "key_question_excluded_from_dashboard",
        unique, question, dataset, variable, Keyqn, key_question_flag,
        include_in_dashboard
      )
    ]),
    unique(final_output_dt[
      unique %in% setdiff(dashboard_include_ids, external_trimmed_ids),
      .(
        issue = "dashboard_included_missing_from_external_trimmed",
        unique, question, dataset, variable, Keyqn, key_question_flag,
        include_in_dashboard
      )
    ]),
    unique(external_tableau_trimmed[
      unique %in% setdiff(external_trimmed_ids, dashboard_include_ids),
      .(
        issue = "external_trimmed_extra_not_dashboard_included",
        unique, question, dataset, variable, Keyqn,
        key_question_flag = NA_character_,
        include_in_dashboard = NA_character_
      )
    ])
  ),
  fill = TRUE
)

schema_qa <- data.table::rbindlist(
  list(
    schema_qa_row("full_all_registry", full_output, full_output_columns),
    schema_qa_row("internal", internal_tableau_wide, LEGACY_WIDE_COLUMNS, c(CI_OUTPUT_COLUMNS, CI_SIGNIFICANCE_COLUMNS)),
    schema_qa_row(
      "internal_trimmed",
      internal_tableau_trimmed,
      LEGACY_TRIMMED_COLUMNS,
      c(OPI_AND_COUNT_COLUMNS, CI_OUTPUT_COLUMNS, CI_SIGNIFICANCE_COLUMNS)
    ),
    schema_qa_row(
      "external_trimmed",
      external_tableau_trimmed,
      LEGACY_TRIMMED_COLUMNS,
      c(OPI_AND_COUNT_COLUMNS, CI_OUTPUT_COLUMNS, CI_SIGNIFICANCE_COLUMNS)
    )
  ),
  fill = TRUE
)

schema_checks <- data.table::data.table(
  check = c(
    "internal_wide_column_order",
    "internal_trimmed_column_order",
    "external_trimmed_column_order",
    "internal_wide_keeps_opi_and_counts",
    "internal_trimmed_removes_opi_and_counts",
    "external_trimmed_removes_opi_and_counts",
    "internal_wide_has_no_ci_fields",
    "internal_trimmed_has_no_ci_fields",
    "external_trimmed_has_no_ci_fields",
    "legacy_disgree_typo_preserved",
    "tableau_outputs_do_not_use_year_time_label",
    "external_trimmed_question_count_vs_old_157",
    "external_trimmed_2026_q1_rows"
  ),
  status = c(
    ifelse(identical(names(internal_tableau_wide), LEGACY_WIDE_COLUMNS), "PASS", "FAIL"),
    ifelse(identical(names(internal_tableau_trimmed), LEGACY_TRIMMED_COLUMNS), "PASS", "FAIL"),
    ifelse(identical(names(external_tableau_trimmed), LEGACY_TRIMMED_COLUMNS), "PASS", "FAIL"),
    ifelse(all(OPI_AND_COUNT_COLUMNS %in% names(internal_tableau_wide)), "PASS", "FAIL"),
    ifelse(length(intersect(names(internal_tableau_trimmed), OPI_AND_COUNT_COLUMNS)) == 0, "PASS", "FAIL"),
    ifelse(length(intersect(names(external_tableau_trimmed), OPI_AND_COUNT_COLUMNS)) == 0, "PASS", "FAIL"),
    ifelse(length(intersect(names(internal_tableau_wide), CI_OUTPUT_COLUMNS)) == 0, "PASS", "FAIL"),
    ifelse(length(intersect(names(internal_tableau_trimmed), CI_OUTPUT_COLUMNS)) == 0, "PASS", "FAIL"),
    ifelse(length(intersect(names(external_tableau_trimmed), CI_OUTPUT_COLUMNS)) == 0, "PASS", "FAIL"),
    ifelse("%Disgree" %in% names(external_tableau_trimmed), "PASS", "FAIL"),
    ifelse(
      !any(internal_tableau_wide$Time == "Year", na.rm = TRUE) &&
        !any(internal_tableau_trimmed$Time == "Year", na.rm = TRUE) &&
        !any(external_tableau_trimmed$Time == "Year", na.rm = TRUE),
      "PASS",
      "FAIL"
    ),
    ifelse(data.table::uniqueN(external_tableau_trimmed$unique, na.rm = TRUE) == 157L, "INFO", "WARN"),
    ifelse(sum(external_tableau_trimmed$Period == "2026 Q1", na.rm = TRUE) > 0, "INFO", "INFO")
  ),
  detail = c(
    paste("columns:", ncol(internal_tableau_wide)),
    paste("columns:", ncol(internal_tableau_trimmed)),
    paste("columns:", ncol(external_tableau_trimmed)),
    paste("present:", paste(intersect(names(internal_tableau_wide), OPI_AND_COUNT_COLUMNS), collapse = " | ")),
    paste("present:", paste(intersect(names(internal_tableau_trimmed), OPI_AND_COUNT_COLUMNS), collapse = " | ")),
    paste("present:", paste(intersect(names(external_tableau_trimmed), OPI_AND_COUNT_COLUMNS), collapse = " | ")),
    paste("present:", paste(intersect(names(internal_tableau_wide), CI_OUTPUT_COLUMNS), collapse = " | ")),
    paste("present:", paste(intersect(names(internal_tableau_trimmed), CI_OUTPUT_COLUMNS), collapse = " | ")),
    paste("present:", paste(intersect(names(external_tableau_trimmed), CI_OUTPUT_COLUMNS), collapse = " | ")),
    "External TWB references %Disgree, not %Disagree.",
    "Year is normalised to Annual in Tableau-facing exports.",
    paste("external trimmed unique questions:", data.table::uniqueN(external_tableau_trimmed$unique, na.rm = TRUE)),
    paste("external trimmed 2026 Q1 rows:", sum(external_tableau_trimmed$Period == "2026 Q1", na.rm = TRUE))
  )
)

schema_qa <- data.table::rbindlist(
  list(
    schema_qa,
    data.table::data.table(
      output = paste0("CHECK: ", schema_checks$check),
      rows = NA_integer_,
      columns = NA_integer_,
      unique_question_ids = NA_integer_,
      unique_question_text = NA_integer_,
      column_order_matches_expected = schema_checks$status,
      missing_expected_columns = schema_checks$detail,
      extra_columns = "",
      forbidden_columns_present = "",
      contains_year_time_label = NA,
      contains_annual_time_label = NA,
      rows_2026_q1 = NA_integer_
    )
  ),
  fill = TRUE
)

pgp_cap_check <- data.table::data.table()
if (exists("response_long") && nrow(response_long) > 0) {
  response_dt <- data.table::as.data.table(response_long)
  pgp_cap_check <- response_dt[
    dataset == "pgp" & !is.na(date) & date > PGP_WEIGHT_CAP_AFTER_DATE,
    .(
      pgp_weight_cap_after_date = as.character(PGP_WEIGHT_CAP_AFTER_DATE),
      pgp_weight_cap_limit = PGP_WEIGHT_CAP_VALUE,
      pgp_post_31_aug_2025_rows = .N,
      max_weight = ifelse(.N > 0, max(weight, na.rm = TRUE), NA_real_),
      rows_above_cap = sum(weight > PGP_WEIGHT_CAP_VALUE, na.rm = TRUE)
    )
  ]
}

write_log(output_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_05_output_summary.csv")))
write_log(bolt_on_row_accounting, file.path(LOG_DIR, paste0(Sys.Date(), "_05_bolt_on_row_accounting.csv")))
write_log(key_dashboard_crosscheck, file.path(LOG_DIR, paste0(Sys.Date(), "_05_key_dashboard_crosscheck.csv")))
write_log(membership_qa, file.path(LOG_DIR, paste0(Sys.Date(), "_05_tableau_membership_qa.csv")))
write_log(schema_qa, file.path(LOG_DIR, paste0(Sys.Date(), "_05_tableau_schema_qa.csv")))
write_log(tableau_exports, file.path(LOG_DIR, paste0(Sys.Date(), "_05_tableau_exports.csv")))
write_log(pgp_cap_check, file.path(LOG_DIR, paste0(Sys.Date(), "_05_pgp_weight_cap_check.csv")))

message("Full all-registry CSV saved to: ", FULL_OUTPUT_CSV)
message("Dashboard-included legacy-trimmed compatibility CSV saved to: ", DASHBOARD_OUTPUT_CSV)
message("Key-question CSV saved to: ", KEY_QUESTION_OUTPUT_CSV)
message("Internal Tableau CSV saved to: ", INTERNAL_TABLEAU_WIDE_CSV)
message("Internal Tableau trimmed CSV saved to: ", INTERNAL_TABLEAU_TRIMMED_CSV)
message("External Tableau trimmed CSV saved to: ", EXTERNAL_TABLEAU_TRIMMED_CSV)
message("Full rows: ", nrow(full_output))
message("New output rows from current run: ", new_output_rows_from_current_run)
message("Existing output rows replaced by bolt-on key: ", existing_output_rows_replaced)
message("Net cumulative output row change: ", net_output_row_change)
message("Internal Tableau rows: ", nrow(internal_tableau_wide))
message("External Tableau trimmed rows: ", nrow(external_tableau_trimmed))
message("Key-question rows: ", nrow(key_question_output))
message("05_bolt_on_export.R completed.")
