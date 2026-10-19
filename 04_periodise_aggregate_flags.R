# ============================================================
# 04_periodise_aggregate_flags.R
# Periodise response-level data and aggregate all registry questions
# ============================================================
#
# Corrected logic:
# - Aggregate every processed registry question in response_long.
# - Do not filter by Include in Dashboard? here.
# - Carry Key Question?, Include in Dashboard?, Direction Coding, and Remarks
#   through to the aggregate so export can filter safely later.

if (!exists("response_long")) {
  response_long_path <- file.path(INTERMEDIATE_DIR, "03_response_long.rds")
  if (!file.exists(response_long_path)) {
    stop("response_long not found and 03_response_long.rds does not exist. Run 03 first.")
  }
  response_long <- readRDS(response_long_path)
}
if (!exists("clean_col")) {
  source(file.path(PROJECT_DIR, "01_helpers.R"))
}

message("============================================================")
message("04_periodise_aggregate_flags.R")
message("Response rows: ", nrow(response_long))
message("============================================================")

empty_aggregate_output <- function() {
  cols <- unique(c(
    FULL_INTERNAL_COLUMNS,
    "respondent_n",
    "source_file_name"
  ))
  out <- data.table::data.table(matrix(nrow = 0, ncol = length(cols)))
  names(out) <- cols
  out
}

build_period_tasks <- function(response_long) {
  if (nrow(response_long) == 0) {
    return(data.table::data.table())
  }

  dt <- data.table::as.data.table(response_long)
  period_map <- c(
    Quarter = "quarter_period",
    Year = "year_period",
    Month = "month_period",
    Week = "week_period"
  )

  period_tasks <- list()
  for (survey_id in unique(dt$dataset)) {
    levels <- period_levels_by_survey[[survey_id]] %||% c("Quarter", "Year")
    for (level in levels) {
      period_col <- period_map[[level]]
      if (is.null(period_col) || !(period_col %in% names(dt))) {
        next
      }
      period_tasks[[length(period_tasks) + 1]] <- data.table::data.table(
        survey = survey_id,
        level = level,
        period_col = period_col
      )
    }
  }

  if (length(period_tasks) == 0) {
    return(data.table::data.table())
  }

  period_tasks <- data.table::rbindlist(period_tasks, fill = TRUE)
  if (nrow(period_tasks) == 0) {
    return(data.table::data.table())
  }

  period_tasks[]
}

quarterly_survey_ids <- function() {
  names(SURVEY_REFRESH_CADENCE)[SURVEY_REFRESH_CADENCE == "quarterly"]
}

empty_quarterly_annual_coverage_log <- function() {
  data.table::data.table(
    dataset = character(),
    year_period = character(),
    quarters_present = character(),
    quarter_count = integer(),
    response_rows = integer(),
    respondents = integer(),
    annual_allowed = logical(),
    annual_action = character()
  )
}

build_quarterly_annual_coverage_log <- function(response_long) {
  if (nrow(response_long) == 0) {
    return(empty_quarterly_annual_coverage_log())
  }

  dt <- data.table::as.data.table(response_long)
  needed <- c("dataset", "year_period", "quarter_period", "score_1_5", "respondent_id")
  if (!all(needed %in% names(dt))) {
    return(empty_quarterly_annual_coverage_log())
  }

  dt <- dt[dataset %in% quarterly_survey_ids()]
  if (nrow(dt) == 0) {
    return(empty_quarterly_annual_coverage_log())
  }

  dt[, score_1_5 := safe_as_numeric(score_1_5)]
  dt <- dt[
    score_1_5 %in% 1:5 &
      !is.na(year_period) & stringr::str_trim(as.character(year_period)) != "" &
      !is.na(quarter_period) & stringr::str_trim(as.character(quarter_period)) != ""
  ]
  if (nrow(dt) == 0) {
    return(empty_quarterly_annual_coverage_log())
  }

  q_match <- stringr::str_match(as.character(dt$quarter_period), "^(20[0-9]{2})\\s*Q\\s*([1-4])$")
  dt[, quarter_year := q_match[, 2]]
  dt[, quarter_num := suppressWarnings(as.integer(q_match[, 3]))]
  dt <- dt[!is.na(quarter_num) & as.character(year_period) == quarter_year]
  if (nrow(dt) == 0) {
    return(empty_quarterly_annual_coverage_log())
  }

  out <- dt[
    ,
    .(
      quarters_present = paste0("Q", sort(unique(quarter_num)), collapse = ","),
      quarter_count = data.table::uniqueN(quarter_num),
      response_rows = .N,
      respondents = data.table::uniqueN(respondent_id)
    ),
    by = .(dataset, year_period = as.character(year_period))
  ][order(dataset, year_period)]

  out[, annual_allowed := quarter_count == 4L]
  out[
    ,
    annual_action := data.table::fifelse(
      annual_allowed,
      "annual_created_full_year",
      "annual_suppressed_partial_year"
    )
  ]
  out[]
}

suppress_partial_quarterly_annual_rows <- function(agg, coverage_log) {
  agg <- data.table::as.data.table(agg)
  if (nrow(agg) == 0 || !all(c("dataset", "Time", "Period") %in% names(agg))) {
    return(agg)
  }

  q_surveys <- quarterly_survey_ids()
  if (length(q_surveys) == 0) {
    return(agg)
  }

  year_idx <- agg$dataset %in% q_surveys & agg$Time == "Year"
  if (!any(year_idx, na.rm = TRUE)) {
    return(agg)
  }

  allowed_keys <- character()
  if (nrow(coverage_log) > 0 && all(c("dataset", "year_period", "annual_allowed") %in% names(coverage_log))) {
    allowed <- coverage_log[annual_allowed == TRUE]
    allowed_keys <- paste(allowed$dataset, allowed$year_period, sep = "\r")
  }

  row_keys <- paste(agg$dataset, agg$Period, sep = "\r")
  keep_idx <- !year_idx | row_keys %in% allowed_keys
  agg[keep_idx]
}

aggregate_c360 <- function(response_long) {
  if (nrow(response_long) == 0) {
    return(empty_aggregate_output())
  }

  dt <- data.table::as.data.table(response_long)
  period_tasks <- build_period_tasks(dt)
  if (nrow(period_tasks) == 0) {
    return(empty_aggregate_output())
  }

  message("Period slices to aggregate without materialising full periodised response table: ", nrow(period_tasks))

  needed_cols <- unique(c(
    "dataset", "variable", "demo", "demography", "question", "unique",
    "dataset_label", "update_frequency", "normal_refresh_mode", "source_folder",
    "Tier1", "Tier2", "Tier3", "Keyqn", "key_question_flag",
    "include_in_dashboard", "direction", "Direction Coding", "Remarks",
    "Notes", "score_1_5", "weight", "respondent_id", "source_file",
    "source_file_name", "source_file_mtime", period_tasks$period_col
  ))
  needed_cols <- needed_cols[needed_cols %in% names(dt)]
  dt <- dt[, ..needed_cols]

  dt[, score_1_5 := safe_as_numeric(score_1_5)]
  dt <- dt[score_1_5 %in% 1:5]

  if (nrow(dt) == 0) {
    return(empty_aggregate_output())
  }

  dt[, weight := safe_as_numeric(weight)]
  dt[is.na(weight) | weight <= 0, weight := 1]

  aggregate_one_chunk <- function(chunk_dt) {
    if (nrow(chunk_dt) == 0) {
      return(empty_aggregate_output())
    }

    group_cols <- c(
      "dataset", "variable", "demo", "demography", "question", "unique",
      "dataset_label", "update_frequency", "normal_refresh_mode", "source_folder",
      "Tier1", "Tier2", "Tier3", "Keyqn", "key_question_flag",
      "include_in_dashboard", "direction", "Direction Coding", "Remarks",
      "Notes", "Time", "Period"
    )
    group_cols <- group_cols[group_cols %in% names(chunk_dt)]

    agg <- chunk_dt[
      ,
      .(
        `1` = sum(weight[score_1_5 == 1], na.rm = TRUE),
        `2` = sum(weight[score_1_5 == 2], na.rm = TRUE),
        `3` = sum(weight[score_1_5 == 3], na.rm = TRUE),
        `4` = sum(weight[score_1_5 == 4], na.rm = TRUE),
        `5` = sum(weight[score_1_5 == 5], na.rm = TRUE),
        total.sample.n = sum(weight, na.rm = TRUE),
        unweighted_n = .N,
        respondent_n = data.table::uniqueN(respondent_id),
        effective_n = {
          sw <- sum(weight, na.rm = TRUE)
          sw2 <- sum(weight^2, na.rm = TRUE)
          ifelse(sw2 > 0, sw^2 / sw2, NA_real_)
        },
        source_file = paste(sort(unique(source_file)), collapse = " | "),
        source_file_name = paste(sort(unique(source_file_name)), collapse = " | "),
        source_file_mtime = paste(sort(unique(source_file_mtime)), collapse = " | ")
      ),
      by = group_cols
    ]

    score_cols <- as.character(1:5)
    agg[, total_scored_weight := rowSums(.SD, na.rm = TRUE), .SDcols = score_cols]
    agg[total_scored_weight <= 0, total_scored_weight := NA_real_]

    agg[, Agree := (`4` + `5`) / total_scored_weight * 100]
    agg[, Disagree := (`1` + `2`) / total_scored_weight * 100]
    agg[, Neutral := `3` / total_scored_weight * 100]
    agg[, Net_Satisfaction := Agree - Disagree]

    suppress_idx <- agg$unweighted_n < MIN_SAMPLE_N | agg$total.sample.n < MIN_SAMPLE_N
    suppress_idx[is.na(suppress_idx)] <- TRUE
    suppress_cols <- c(score_cols, "Agree", "Disagree", "Neutral", "Net_Satisfaction")
    for (cc in suppress_cols) {
      agg[[cc]][suppress_idx] <- NA_real_
    }

    agg[, total_scored_weight := NULL]
    agg[]
  }

  agg_list <- c360_progress_lapply(
    nrow(period_tasks),
    "Aggregating period slice",
    function(i) {
      survey_id <- period_tasks$survey[[i]]
      level <- period_tasks$level[[i]]
      period_col <- period_tasks$period_col[[i]]

      chunk_dt <- dt[dt[["dataset"]] == survey_id]
      chunk_dt[, Time := level]
      chunk_dt[, Period := as.character(get(period_col))]
      chunk_dt <- chunk_dt[!is.na(Period) & stringr::str_trim(Period) != ""]

      agg <- aggregate_one_chunk(chunk_dt)
      rm(chunk_dt)
      invisible(gc())
      agg
    }
  )

  data.table::rbindlist(agg_list, fill = TRUE)
}

add_polarisation <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  score_cols <- as.character(1:5)
  for (cc in score_cols) {
    if (!(cc %in% names(df))) df[[cc]] <- NA_real_
    df[[cc]] <- safe_as_numeric(df[[cc]])
  }

  n_total <- df$`1` + df$`2` + df$`3` + df$`4` + df$`5`
  denom <- n_total^2 - n_total
  denom[is.na(denom) | denom <= 0] <- NA_real_

  df$f0 <- (
    (df$`1` * df$`1` - df$`1`) +
      (df$`2` * df$`2` - df$`2`) +
      (df$`3` * df$`3` - df$`3`) +
      (df$`4` * df$`4` - df$`4`) +
      (df$`5` * df$`5` - df$`5`)
  ) / denom * 100

  df$f1 <- 2 * (
    df$`1` * df$`2` +
      df$`2` * df$`3` +
      df$`3` * df$`4` +
      df$`4` * df$`5`
  ) / denom * 100

  df$f2a <- 2 * (df$`1` * df$`3` + df$`3` * df$`5`) / denom * 100
  df$f2b <- 2 * (df$`2` * df$`4`) / denom * 100
  df$f3 <- 2 * (df$`1` * df$`4` + df$`2` * df$`5`) / denom * 100
  df$f4 <- 2 * (df$`1` * df$`5`) / denom * 100

  df$b2b <- 1.07
  df$b3 <- 1.35
  df$b4 <- 2 * (n_total - 1) / n_total
  df$b4[is.na(n_total) | n_total <= 0] <- NA_real_

  df$Polarisation_Index <- df$b2b * df$f2b + df$b3 * df$f3 + df$b4 * df$f4
  df
}

add_percentage_and_ci_fields <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  df <- data.table::as.data.table(df)
  score_cols <- as.character(1:5)
  for (cc in score_cols) df[[cc]] <- safe_as_numeric(df[[cc]])

  df[, `%Strongly Disagree` := `1` / total.sample.n * 100]
  df[, `%Disgree` := `2` / total.sample.n * 100]
  df[, `%Neutral` := `3` / total.sample.n * 100]
  df[, `%Agree` := `4` / total.sample.n * 100]
  df[, `%Strongly Agree` := `5` / total.sample.n * 100]
  df[, `%Net-Satisfaction` := (`4` + `5` - `1` - `2`) / total.sample.n * 100]

  # Future CI-based flagging path:
  # these fields are retained in the rich analyst/debug output. The current
  # Tableau exports still use legacy cut/sensitivity fields unless
  # C360_SIGNIFICANCE_FLAGGING_APPROACH is set to "ci" in 00_config.R.
  p_agree <- (df$`4` + df$`5`) / df$total.sample.n
  p_disagree <- (df$`1` + df$`2`) / df$total.sample.n
  p_net <- p_agree - p_disagree
  n_eff <- safe_as_numeric(df$effective_n)
  variance <- (p_agree + p_disagree - p_net^2) / n_eff
  variance[variance < 0 | is.na(n_eff) | n_eff <= 1] <- NA_real_

  df[, Net_Sat_SE := sqrt(variance) * 100]
  df[, Net_Sat_CI_Lower := Net_Satisfaction - 1.96 * Net_Sat_SE]
  df[, Net_Sat_CI_Upper := Net_Satisfaction + 1.96 * Net_Sat_SE]

  df
}

add_overall_gap_fields <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  df <- data.table::as.data.table(df)
  overall <- unique(df[
    demo == "overall" & demography == "overall",
    .(
      dataset, variable, unique, Time, Period,
      `Overall %Net-Satisfaction` = `%Net-Satisfaction`
    )
  ])

  df <- merge(
    df,
    overall,
    by = c("dataset", "variable", "unique", "Time", "Period"),
    all.x = TRUE,
    sort = FALSE
  )

  df[, `Diff %Net-Sat (Demo - Overall)` := `%Net-Satisfaction` - `Overall %Net-Satisfaction`]
  df[
    ,
    `Std Dev within Demo` := {
      val <- stats::sd(`%Net-Satisfaction`, na.rm = TRUE)
      ifelse(is.nan(val), NA_real_, val)
    },
    by = .(dataset, variable, unique, demo, Time, Period)
  ]

  df
}

add_05_placeholder_fields <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  df <- data.table::as.data.table(df)

  placeholder_numeric <- c(
    "Sensitivity Count - Consective Decline (Net-Sat)",
    "Sensitivity Count - Consective Improved (Net-Sat)",
    "Sensitivity Count - No Consecutive Changes (Net-Sat)",
    "Change (Agree)",
    "Change (Net-Satisfaction)",
    "SD (Preceding 2Q - Agree)",
    "SD (Preceding 2Q - Net)",
    "Annual Change (Net-Satisfaction)",
    "Baseline Reference (Preceding 5 Year Mean)",
    "Deviation from Reference",
    "Baseline Deviation (Preceding 5 Year Absolute Mean)",
    "Sensitivity Count - No Change (Net-Sat)",
    "Sensitivity Count - Improvement (Net-Sat)",
    "Sensitivity Count - Decline (Net-Sat)"
  )

  for (cc in placeholder_numeric) {
    if (!(cc %in% names(df))) df[[cc]] <- NA_real_
  }
  if (!("Trend" %in% names(df))) df$Trend <- NA_character_
  if (!("Sensitivity" %in% names(df))) df$Sensitivity <- NA_character_

  df
}

normalise_aggregate_columns <- function(df) {
  if (nrow(df) == 0) {
    return(empty_aggregate_output())
  }

  df <- data.table::as.data.table(df)
  df <- add_missing_columns(df, FULL_INTERNAL_COLUMNS)
  data.table::setcolorder(df, unique(c(FULL_INTERNAL_COLUMNS, names(df))))
  df[]
}

all_registry_agg <- aggregate_c360(response_long)
all_registry_agg <- add_polarisation(all_registry_agg)
all_registry_agg <- add_percentage_and_ci_fields(all_registry_agg)
all_registry_agg <- add_overall_gap_fields(all_registry_agg)
all_registry_agg <- add_05_placeholder_fields(all_registry_agg)
all_registry_agg <- normalise_aggregate_columns(all_registry_agg)

quarterly_annual_coverage <- build_quarterly_annual_coverage_log(response_long)
all_registry_agg <- suppress_partial_quarterly_annual_rows(all_registry_agg, quarterly_annual_coverage)

dashboard_agg <- all_registry_agg[Time %in% DASHBOARD_OUTPUT_PERIODS]
internal_test_agg <- all_registry_agg[Time %in% INTERNAL_TEST_PERIODS]
old_stat_sig_test_agg <- internal_test_agg

saveRDS(all_registry_agg, file.path(INTERMEDIATE_DIR, "04_all_registry_agg.rds"))
saveRDS(dashboard_agg, file.path(INTERMEDIATE_DIR, "04_dashboard_agg.rds"))
saveRDS(internal_test_agg, file.path(INTERMEDIATE_DIR, "04_internal_test_agg.rds"))

aggregate_summary <- if (nrow(all_registry_agg) > 0) {
  all_registry_agg[
    ,
    .(
      aggregate_rows = .N,
      questions = data.table::uniqueN(unique),
      dashboard_included_questions = data.table::uniqueN(unique[include_in_dashboard == "Include"]),
      key_questions = data.table::uniqueN(unique[Keyqn == "Yes"])
    ),
    by = .(dataset, Time)
  ][order(dataset, Time)]
} else {
  data.table::data.table()
}

write_log(quarterly_annual_coverage, file.path(LOG_DIR, paste0(Sys.Date(), "_04_quarterly_annual_coverage.csv")))
write_log(aggregate_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_04_aggregate_summary.csv")))

message("All-registry aggregate rows: ", nrow(all_registry_agg))
message("Dashboard-period aggregate rows before dashboard filter: ", nrow(dashboard_agg))
message("Internal Week/Month aggregate rows: ", nrow(internal_test_agg))
message("04_periodise_aggregate_flags.R completed.")
