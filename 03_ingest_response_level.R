# ============================================================
# 03_ingest_response_level.R
# Ingest registry-matched raw questions into response-level long data
# ============================================================
#
# Corrected logic:
# - Read each selected raw file.
# - Standardise raw column names and apply historical patches before matching.
# - Pull all registry rows for that survey.
# - Pivot only questions that are both in the registry and present in raw data.
# - Do not filter by Key Question?, Include in Dashboard?, or Sensitivity.

if (!exists("files_to_process")) {
  stop("files_to_process not found. Run 02_registry_and_file_discovery.R first.")
}
if (!exists("registry")) {
  stop("registry not found. Run 02_registry_and_file_discovery.R first.")
}
if (!exists("clean_col")) {
  source(file.path(PROJECT_DIR, "01_helpers.R"))
}

message("============================================================")
message("03_ingest_response_level.R")
message("Files to process: ", nrow(files_to_process))
message("============================================================")

empty_response_long <- function() {
  data.table::data.table(
    dataset = character(),
    source_file = character(),
    source_file_name = character(),
    source_file_mtime = character(),
    source_file_size = numeric(),
    respondent_id = character(),
    dataset_label = character(),
    update_frequency = character(),
    normal_refresh_mode = character(),
    source_folder = character(),
    date = as.Date(character()),
    date_source = character(),
    date_source_column = character(),
    year = integer(),
    quarter = integer(),
    month = integer(),
    week = integer(),
    quarter_period = character(),
    year_period = character(),
    month_period = character(),
    week_period = character(),
    weight = numeric(),
    demo = character(),
    demography = character(),
    variable = character(),
    raw_question_column = character(),
    raw_question_column_clean = character(),
    unique = character(),
    question = character(),
    Tier1 = character(),
    Tier2 = character(),
    Tier3 = character(),
    Keyqn = character(),
    key_question_flag = character(),
    include_in_dashboard = character(),
    direction = character(),
    `Direction Coding` = character(),
    Remarks = character(),
    Notes = character(),
    scale_type = character(),
    valid_from = as.Date(character()),
    valid_to = as.Date(character()),
    raw_value = character(),
    score_1_5 = numeric()
  )
}

c360_qn_df_demo_columns <- function() {
  c(
    "pmet",
    "marital",
    "evermarried",
    "overall",
    "educ_group2",
    "lbsc",
    "race",
    "age",
    "demographic_group",
    "vulnerable_group",
    "age2",
    "gender",
    "educ",
    "housing",
    "educ_group",
    "ageXeduc_group",
    "employment_status",
    "Occupation.Type",
    "occupation",
    "housing_group",
    "household_income",
    "income",
    "income2",
    "household_income_q",
    "age_40up",
    "degree_type",
    "sector"
  )
}

c360_qn_df_columns <- function() {
  demo_cols <- c360_qn_df_demo_columns()
  c(
    "id",
    as.vector(rbind(demo_cols, paste0(demo_cols, "_weight"))),
    "survey",
    "year",
    "variable",
    "value",
    "Tier1",
    "Tier2",
    "Tier3",
    "Keyqn",
    "question",
    "quarter_period"
  )
}

empty_c360_qn_df <- function() {
  out <- data.table::as.data.table(as.list(stats::setNames(
    rep(list(character()), length(c360_qn_df_columns())),
    c360_qn_df_columns()
  )))
  for (cc in grep("_weight$", names(out), value = TRUE)) {
    out[[cc]] <- numeric()
  }
  out$value <- numeric()
  out$year <- integer()
  out
}

empty_file_log <- function() {
  data.table::data.table(
    survey = character(),
    file_name = character(),
    file_path = character(),
    status = character(),
    raw_rows = integer(),
    registry_questions = integer(),
    matched_questions = integer(),
    missing_questions = integer(),
    response_rows = integer(),
    invalid_score_rows = integer(),
    pgp_weight_cap_after_date = character(),
    pgp_weight_cap_limit = numeric(),
    pgp_post_cap_rows_capped = integer(),
    pgp_post_cap_rows_above_limit = integer(),
    message = character()
  )
}

empty_period_guard_log <- function() {
  data.table::data.table(
    survey = character(),
    file_name = character(),
    file_path = character(),
    target_quarter = character(),
    issue_type = character(),
    date_source = character(),
    date_source_column = character(),
    quarter_period = character(),
    rows = integer(),
    min_date = as.Date(character()),
    max_date = as.Date(character()),
    action = character()
  )
}

respondent_key_detail_columns <- function() {
  c(
    "dataset", "source_file_name", "respondent_id", "date", "date_source",
    "date_source_column", "quarter_period", "weight", "demo", "demography",
    "variable", "unique", "question", "Keyqn", "include_in_dashboard",
    "raw_question_column_clean", "raw_value", "score_1_5"
  )
}

make_respondent_key_details <- function(response_long) {
  dt <- data.table::as.data.table(response_long)
  cols <- respondent_key_detail_columns()
  dt <- add_missing_columns(dt, cols)
  dt[, ..cols]
}

format_c360_qn_df_id <- function(raw_id, survey, year, row_id) {
  raw_id <- as.character(raw_id)
  row_id <- as.character(row_id)
  blank_id <- is.na(raw_id) | stringr::str_trim(raw_id) == ""
  raw_id[blank_id] <- row_id[blank_id]

  survey <- rep(as.character(survey), length.out = length(raw_id))
  survey[is.na(survey) | stringr::str_trim(survey) == ""] <- "unknown_survey"

  year_chr <- as.character(suppressWarnings(as.integer(year)))
  year_chr <- rep(year_chr, length.out = length(raw_id))
  year_chr[is.na(year_chr) | year_chr == "NA"] <- "unknown_year"

  survey_year_prefix <- paste0(survey, "_", year_chr, "_")
  year_survey_prefix <- paste0(year_chr, "_", survey, "_")
  already_c360_id <- startsWith(raw_id, survey_year_prefix) | startsWith(raw_id, year_survey_prefix)

  style <- if (exists("C360_QN_DF_ID_STYLE", inherits = TRUE)) {
    get("C360_QN_DF_ID_STYLE", inherits = TRUE)
  } else {
    "survey_year_id"
  }

  out <- if (identical(style, "source")) {
    raw_id
  } else if (identical(style, "year_survey_id")) {
    paste(year_chr, survey, raw_id, sep = "_")
  } else {
    paste(survey, year_chr, raw_id, sep = "_")
  }
  out[already_c360_id] <- raw_id[already_c360_id]
  out
}

normalise_c360_qn_df_for_export <- function(df) {
  df <- data.table::as.data.table(df)
  cols <- c360_qn_df_columns()
  df <- add_missing_columns(df, cols)
  if ("id" %in% names(df)) df[, id := as.character(id)]
  if ("survey" %in% names(df)) df[, survey := as.character(survey)]
  if ("year" %in% names(df)) df[, year := suppressWarnings(as.integer(year))]
  if ("variable" %in% names(df)) df[, variable := as.character(variable)]
  if ("value" %in% names(df)) df[, value := safe_as_numeric(value)]
  if ("quarter_period" %in% names(df)) df[, quarter_period := as.character(quarter_period)]
  df[, ..cols]
}

load_existing_c360_qn_df <- function(path) {
  path <- as.character(path %||% "")
  path <- path[[1]]
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(empty_c360_qn_df())
  }

  out <- data.table::fread(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A", "NULL", "#NULL", "#NULL!")
  )
  normalise_c360_qn_df_for_export(out)
}

bolt_on_c360_qn_df <- function(existing, new_rows) {
  existing <- normalise_c360_qn_df_for_export(existing)
  new_rows <- normalise_c360_qn_df_for_export(new_rows)
  keys <- c("survey", "year", "quarter_period", "id", "variable")

  if (nrow(existing) == 0 || nrow(new_rows) == 0) {
    return(list(
      data = data.table::rbindlist(list(existing, new_rows), fill = TRUE),
      rows_replaced = 0L
    ))
  }

  existing_match <- data.table::copy(existing)
  new_match <- data.table::copy(new_rows)
  for (cc in keys) {
    existing_match[[cc]] <- as.character(existing_match[[cc]])
    new_match[[cc]] <- as.character(new_match[[cc]])
  }

  new_keys <- unique(new_match[, ..keys])
  matched_existing_rows <- existing_match[new_keys, on = keys, which = TRUE, nomatch = 0L]
  keep_existing_idx <- rep(TRUE, nrow(existing))
  keep_existing_idx[matched_existing_rows] <- FALSE

  existing_for_context <- existing[keep_existing_idx]
  rows_replaced <- nrow(existing) - nrow(existing_for_context)
  list(
    data = data.table::rbindlist(list(existing_for_context, new_rows), fill = TRUE),
    rows_replaced = rows_replaced
  )
}

apply_target_quarter_guard <- function(raw, survey_id, file_name, path) {
  if (!target_quarter_enabled()) {
    attr(raw, "period_guard_log") <- empty_period_guard_log()
    return(raw)
  }

  target_quarter <- get("TARGET_UPDATE_QUARTER", inherits = TRUE)
  bounds <- target_quarter_bounds()
  raw <- data.table::as.data.table(raw)
  raw <- add_missing_columns(raw, c(".date", ".date_source", ".date_source_column", ".quarter_period"))
  raw[, .date := as.Date(.date)]
  raw[, .date_source := as.character(.date_source)]
  raw[, .date_source_column := as.character(.date_source_column)]
  raw[, .quarter_period := as.character(.quarter_period)]

  explicit_sources <- c("date_column", "month_year_columns")
  explicit_idx <- raw$.date_source %in% explicit_sources
  implicit_idx <- !explicit_idx

  logs <- list()
  if (any(explicit_idx)) {
    off_target <- raw[
      explicit_idx &
        !is.na(.date) &
        (.date < bounds$start | .date > bounds$end)
    ]
    if (nrow(off_target) > 0) {
      logs[[length(logs) + 1]] <- off_target[
        ,
        .(
          survey = survey_id,
          file_name = file_name,
          file_path = path,
          target_quarter = target_quarter,
          issue_type = "explicit_source_date_outside_target_quarter",
          rows = .N,
          min_date = min(.date, na.rm = TRUE),
          max_date = max(.date, na.rm = TRUE),
          action = "Explicit source dates were retained under their derived quarter. Review only if unexpected."
        ),
        by = .(date_source = .date_source, date_source_column = .date_source_column, quarter_period = .quarter_period)
      ]
    } else {
      logs[[length(logs) + 1]] <- raw[
        explicit_idx,
        .(
          survey = survey_id,
          file_name = file_name,
          file_path = path,
          target_quarter = target_quarter,
          issue_type = "explicit_source_dates_within_target_quarter",
          rows = .N,
          min_date = if (all(is.na(.date))) as.Date(NA) else min(.date, na.rm = TRUE),
          max_date = if (all(is.na(.date))) as.Date(NA) else max(.date, na.rm = TRUE),
          action = "Proceed."
        ),
        by = .(date_source = .date_source, date_source_column = .date_source_column, quarter_period = .quarter_period)
      ]
    }
  }

  if (any(implicit_idx)) {
    logs[[length(logs) + 1]] <- raw[
      implicit_idx,
      .(
        survey = survey_id,
        file_name = file_name,
        file_path = path,
        target_quarter = target_quarter,
        issue_type = "implicit_period_set_to_target_quarter",
        rows = .N,
        min_date = if (all(is.na(.date))) as.Date(NA) else min(.date, na.rm = TRUE),
        max_date = if (all(is.na(.date))) as.Date(NA) else max(.date, na.rm = TRUE),
        action = "Filename or missing period was treated as analyst-declared target quarter."
      ),
      by = .(date_source = .date_source, date_source_column = .date_source_column, quarter_period = .quarter_period)
    ]

    raw[implicit_idx, `:=`(
      .date = bounds$start,
      .date_source = "target_quarter",
      .date_source_column = target_quarter,
      .year = bounds$year,
      .quarter_num = bounds$quarter,
      .month_num = lubridate::month(bounds$start),
      .week_num = lubridate::isoweek(bounds$start),
      .year_period = as.character(bounds$year),
      .quarter_period = target_quarter,
      .month_period = paste(bounds$year, month.abb[lubridate::month(bounds$start)]),
      .week_period = sprintf("%s W%02d", bounds$year, lubridate::isoweek(bounds$start))
    )]
  }

  period_guard_log <- data.table::rbindlist(logs, fill = TRUE)
  if (nrow(period_guard_log) == 0) {
    period_guard_log <- empty_period_guard_log()
  }
  out <- as.data.frame(raw)
  attr(out, "period_guard_log") <- period_guard_log
  out
}

prepare_registry_for_ingest <- function(reg_s) {
  keep_cols <- c(
    "raw_question_column_clean", "raw_question_column", "variable", "unique",
    "question", "dataset_label", "update_frequency", "normal_refresh_mode",
    "source_folder", "Tier1", "Tier2", "Tier3", "Keyqn", "key_question_flag",
    "include_in_dashboard", "direction", "Direction Coding", "Remarks",
    "Notes", "scale_type", "valid_from", "valid_to"
  )
  reg_s <- add_missing_columns(data.table::as.data.table(reg_s), keep_cols)
  unique(reg_s[, ..keep_cols])
}

source_for_c360_qn_df_demo <- function(raw, demo_col) {
  demo_clean <- clean_col(demo_col)
  candidates <- unique(c(
    demo_clean,
    DEMO_COL_SYNONYMS[[demo_clean]] %||% character(),
    switch(
      demo_col,
      "Occupation.Type" = c("occupation_type", "occupation"),
      "ageXeduc_group" = c("agexeduc_group"),
      "educ_group" = c("educ_group", "educ_group2"),
      "housing_group" = c("housing_group", "housing2"),
      "income2" = c("income2", "income"),
      character()
    )
  ))
  find_first_existing_col(raw, candidates)
}

build_c360_qn_df_for_file <- function(raw, reg_meta, available_questions, survey_id) {
  if (length(available_questions) == 0 || nrow(raw) == 0) {
    return(empty_c360_qn_df())
  }

  if (!(".c360_row_id" %in% names(raw))) {
    raw$.c360_row_id <- seq_len(nrow(raw))
  }
  for (cc in c(".respondent_id", ".date", ".year", ".quarter_period", ".weight")) {
    if (!(cc %in% names(raw))) raw[[cc]] <- NA
  }

  respondent_dt <- data.table::data.table(
    .c360_row_id = raw$.c360_row_id,
    id = format_c360_qn_df_id(raw$.respondent_id, survey_id, raw$.year, raw$.c360_row_id),
    survey = survey_id,
    year = as.integer(raw$.year),
    quarter_period = as.character(raw$.quarter_period)
  )

  for (demo_col in c360_qn_df_demo_columns()) {
    source_col <- source_for_c360_qn_df_demo(raw, demo_col)
    demo_clean <- clean_col(demo_col)
    values <- if (!is.na(source_col)) {
      normalise_demo_value(demo_clean, raw[[source_col]])
    } else if (demo_clean == "overall") {
      rep("overall", nrow(raw))
    } else {
      rep(NA_character_, nrow(raw))
    }
    respondent_dt[[demo_col]] <- as.character(values)
    respondent_dt[[paste0(demo_col, "_weight")]] <- safe_as_numeric(raw$.weight)
  }

  question_dt <- data.table::as.data.table(raw[, c(".c360_row_id", available_questions), drop = FALSE])
  for (cc in available_questions) {
    question_dt[[cc]] <- as.character(question_dt[[cc]])
  }
  long_dt <- data.table::melt(
    question_dt,
    id.vars = ".c360_row_id",
    measure.vars = available_questions,
    variable.name = "raw_question_column_clean",
    value.name = "raw_value",
    variable.factor = FALSE
  )
  long_dt[, raw_question_column_clean := as.character(raw_question_column_clean)]
  long_dt <- merge(
    long_dt,
    reg_meta,
    by = "raw_question_column_clean",
    all.x = TRUE,
    sort = FALSE,
    allow.cartesian = TRUE
  )

  if (".date" %in% names(raw) && all(c("valid_from", "valid_to") %in% names(long_dt))) {
    row_date <- as.Date(raw$.date[match(long_dt$.c360_row_id, raw$.c360_row_id)])
    keep_valid_window <- is.na(row_date) |
      ((is.na(long_dt$valid_from) | long_dt$valid_from <= row_date) &
        (is.na(long_dt$valid_to) | long_dt$valid_to >= row_date))
    keep_valid_window[is.na(keep_valid_window)] <- TRUE
    long_dt <- long_dt[keep_valid_window]
  }
  if (nrow(long_dt) == 0) {
    return(empty_c360_qn_df())
  }

  long_dt[, value := standardise_score_col(raw_value, scale_type[1]), by = .(raw_question_column_clean, unique)]
  long_dt[, value := apply_direction_coding(value, direction)]
  long_dt <- long_dt[!is.na(value)]
  if (nrow(long_dt) == 0) {
    return(empty_c360_qn_df())
  }

  out <- merge(
    long_dt[, .(.c360_row_id, variable, value, Tier1, Tier2, Tier3, Keyqn, question)],
    respondent_dt,
    by = ".c360_row_id",
    all.x = TRUE,
    sort = FALSE
  )

  out <- add_missing_columns(out, c360_qn_df_columns())
  out[, c360_qn_df_columns(), with = FALSE]
}

process_one_file <- function(file_row, registry) {
  survey_id <- normalise_survey_id(file_row$survey[[1]])
  path <- as.character(file_row$file_path[[1]])
  file_name <- basename(path)

  file_log <- data.table::data.table(
    survey = survey_id,
    file_name = file_name,
    file_path = path,
    status = "started",
    raw_rows = NA_integer_,
    registry_questions = NA_integer_,
    matched_questions = NA_integer_,
    missing_questions = NA_integer_,
    response_rows = NA_integer_,
    invalid_score_rows = NA_integer_,
    pgp_weight_cap_after_date = NA_character_,
    pgp_weight_cap_limit = NA_real_,
    pgp_post_cap_rows_capped = NA_integer_,
    pgp_post_cap_rows_above_limit = NA_integer_,
    message = NA_character_
  )
  period_guard_log <- empty_period_guard_log()

  raw <- tryCatch(
    read_any_file(path),
    error = function(e) {
      file_log$status <- "error_read_file"
      file_log$message <- e$message
      warning("Could not read file: ", file_name, " | ", e$message)
      NULL
    }
  )

  if (is.null(raw)) {
    return(list(
      response_long = empty_response_long(),
      file_log = file_log,
      question_log = data.table::data.table(),
      invalid_score_log = data.table::data.table(),
      name_log = data.table::data.table(),
      c360_qn_df = empty_c360_qn_df(),
      period_guard_log = period_guard_log
    ))
  }

  raw <- standardise_names(as.data.frame(raw))
  name_log <- data.table::as.data.table(attr(raw, "name_map"))
  dropped_name_log <- data.table::as.data.table(attr(raw, "dropped_name_map"))
  if (nrow(name_log) > 0) {
    name_log[, `:=`(survey = survey_id, file_name = file_name, file_path = path, reason = "kept")]
  }
  if (nrow(dropped_name_log) > 0) {
    dropped_name_log[, `:=`(survey = survey_id, file_name = file_name, file_path = path)]
    name_log <- data.table::rbindlist(list(name_log, dropped_name_log), fill = TRUE)
  }

  file_log$raw_rows <- nrow(raw)

  raw <- tryCatch(
    derive_date_fields(raw, survey_id, path),
    error = function(e) {
      warning("Could not derive date fields for ", file_name, ". Continuing with NA dates. Error: ", e$message)
      raw$.date <- as.Date(NA)
      raw$.date_source <- "unparsed"
      raw$.date_source_column <- NA_character_
      raw$.year <- NA_integer_
      raw$.quarter_num <- NA_integer_
      raw$.month_num <- NA_integer_
      raw$.week_num <- NA_integer_
      raw$.quarter_period <- NA_character_
      raw$.year_period <- NA_character_
      raw$.month_period <- NA_character_
      raw$.week_period <- NA_character_
      raw
    }
  )
  raw <- apply_target_quarter_guard(raw, survey_id, file_name, path)
  period_guard_log <- attr(raw, "period_guard_log") %||% empty_period_guard_log()

  raw <- apply_survey_patches(raw, survey_id)
  raw <- add_demo_columns(raw)
  raw$.respondent_id <- get_respondent_id(raw, survey_id, file_name)
  raw$.c360_row_id <- seq_len(nrow(raw))
  weight_vector <- get_weight_vector(raw, survey_id)
  file_log$pgp_weight_cap_after_date <- attr(weight_vector, "pgp_weight_cap_after_date") %||% NA_character_
  file_log$pgp_weight_cap_limit <- attr(weight_vector, "pgp_weight_cap_limit") %||% NA_real_
  file_log$pgp_post_cap_rows_capped <- attr(weight_vector, "pgp_post_cap_rows_capped") %||% 0L
  file_log$pgp_post_cap_rows_above_limit <- attr(weight_vector, "pgp_post_cap_rows_above_limit") %||% 0L
  raw$.weight <- weight_vector

  if (MISSING_WEIGHT_ACTION == "drop") {
    raw <- raw[!is.na(raw$.weight), , drop = FALSE]
  }

  reg_s <- filter_registry_for_survey(registry, survey_id)
  if (nrow(reg_s) == 0) {
    file_log$status <- "skipped_no_registry"
    file_log$registry_questions <- 0L
    file_log$matched_questions <- 0L
    file_log$missing_questions <- 0L
    file_log$response_rows <- 0L
    file_log$invalid_score_rows <- 0L
    file_log$message <- "No registry rows for this survey."
    return(list(
      response_long = empty_response_long(),
      file_log = file_log,
      question_log = data.table::data.table(),
      invalid_score_log = data.table::data.table(),
      name_log = name_log,
      c360_qn_df = empty_c360_qn_df(),
      period_guard_log = period_guard_log
    ))
  }

  if (".date" %in% names(raw)) {
    reg_s <- filter_registry_for_file_dates(reg_s, raw$.date)
  }
  raw <- apply_registry_question_label_aliases(raw, reg_s)
  label_alias_log <- data.table::as.data.table(attr(raw, "registry_question_label_alias_log"))
  if (nrow(label_alias_log) > 0) {
    label_alias_log[, `:=`(
      survey = survey_id,
      file_name = file_name,
      file_path = path,
      original_name = source_original_name,
      clean_name = target_clean_name
    )]
    name_log <- data.table::rbindlist(list(name_log, label_alias_log), fill = TRUE)
  }

  file_log$registry_questions <- nrow(reg_s)
  if (nrow(reg_s) == 0) {
    file_log$status <- "skipped_no_valid_registry_for_date"
    file_log$matched_questions <- 0L
    file_log$missing_questions <- 0L
    file_log$response_rows <- 0L
    file_log$invalid_score_rows <- 0L
    file_log$message <- "Registry rows exist but none are valid for this file date range."
    return(list(
      response_long = empty_response_long(),
      file_log = file_log,
      question_log = data.table::data.table(),
      invalid_score_log = data.table::data.table(),
      name_log = name_log,
      c360_qn_df = empty_c360_qn_df(),
      period_guard_log = period_guard_log
    ))
  }

  reg_s[, available_in_file := raw_question_column_clean %in% names(raw)]
  question_cols <- unique(reg_s$raw_question_column_clean)
  available_questions <- unique(reg_s[available_in_file == TRUE, raw_question_column_clean])

  file_log$matched_questions <- sum(reg_s$available_in_file, na.rm = TRUE)
  file_log$missing_questions <- sum(!reg_s$available_in_file, na.rm = TRUE)

  question_log <- reg_s[, .(
    survey = survey_id,
    file_name = file_name,
    file_path = path,
    dataset,
    variable,
    unique,
    question,
    raw_question_column,
    raw_question_column_clean,
    available_in_file
  )]
  if (length(available_questions) == 0) {
    file_log$status <- "skipped_no_available_registry_questions"
    file_log$response_rows <- 0L
    file_log$invalid_score_rows <- 0L
    file_log$message <- "No registry question columns were found in the raw file."
    return(list(
      response_long = empty_response_long(),
      file_log = file_log,
      question_log = question_log,
      invalid_score_log = data.table::data.table(),
      name_log = name_log,
      c360_qn_df = empty_c360_qn_df(),
      period_guard_log = period_guard_log
    ))
  }

  demo_cols <- intersect(clean_col(DEMO_CUTS), names(raw))
  demo_cols <- demo_cols[
    vapply(
      demo_cols,
      function(cc) any(!is.na(raw[[cc]]) & stringr::str_trim(as.character(raw[[cc]])) != ""),
      logical(1)
    )
  ]
  if (length(demo_cols) == 0) {
    file_log$status <- "skipped_no_demo_cols"
    file_log$response_rows <- 0L
    file_log$invalid_score_rows <- 0L
    file_log$message <- "No demographic columns available after demo cleaning."
    return(list(
      response_long = empty_response_long(),
      file_log = file_log,
      question_log = question_log,
      invalid_score_log = data.table::data.table(),
      name_log = name_log,
      c360_qn_df = empty_c360_qn_df(),
      period_guard_log = period_guard_log
    ))
  }

  base_cols <- c(
    ".respondent_id", ".date", ".date_source", ".date_source_column",
    ".year", ".quarter_num", ".month_num",
    ".week_num", ".quarter_period", ".year_period", ".month_period",
    ".week_period", ".weight"
  )

  for (cc in base_cols) {
    if (!(cc %in% names(raw))) raw[[cc]] <- NA
  }

  keep_cols <- unique(c(base_cols, demo_cols, available_questions))
  dt <- data.table::as.data.table(raw[, keep_cols, drop = FALSE])
  reg_meta <- prepare_registry_for_ingest(reg_s)
  c360_qn_df <- if (isTRUE(WRITE_LEGACY_RESPONDENT_QN_DF)) {
    build_c360_qn_df_for_file(raw, reg_meta, available_questions, survey_id)
  } else {
    empty_c360_qn_df()
  }

  demo_long_list <- list()
  invalid_score_list <- list()

  for (demo_col in demo_cols) {
    demo_dt <- dt[, c(base_cols, demo_col, available_questions), with = FALSE]
    for (cc in available_questions) {
      demo_dt[[cc]] <- as.character(demo_dt[[cc]])
    }
    data.table::setnames(demo_dt, demo_col, ".demography_raw")
    demo_dt[, demo := demo_col]
    demo_dt[, demography := normalise_demo_value(demo_col, .demography_raw)]
    demo_dt <- demo_dt[!is.na(demography) & stringr::str_trim(as.character(demography)) != ""]

    if (nrow(demo_dt) == 0) {
      next
    }

    long_dt <- data.table::melt(
      demo_dt,
      id.vars = c(base_cols, "demo", "demography"),
      measure.vars = available_questions,
      variable.name = "raw_question_column_clean",
      value.name = "raw_value",
      variable.factor = FALSE
    )

    long_dt[, raw_question_column_clean := as.character(raw_question_column_clean)]
    long_dt <- merge(
      long_dt,
      reg_meta,
      by = "raw_question_column_clean",
      all.x = TRUE,
      sort = FALSE,
      allow.cartesian = TRUE
    )

    if (all(c(".date", "valid_from", "valid_to") %in% names(long_dt))) {
      row_date <- as.Date(long_dt$.date)
      keep_valid_window <- is.na(row_date) |
        ((is.na(long_dt$valid_from) | long_dt$valid_from <= row_date) &
          (is.na(long_dt$valid_to) | long_dt$valid_to >= row_date))
      keep_valid_window[is.na(keep_valid_window)] <- TRUE
      long_dt <- long_dt[keep_valid_window]
    }

    if (nrow(long_dt) == 0) {
      next
    }

    long_dt[, `:=`(survey = survey_id, file_name = file_name, file_path = path)]
    long_dt[, score_1_5 := standardise_score_col(raw_value, scale_type[1]), by = .(raw_question_column_clean, unique)]
    long_dt[, score_1_5 := apply_direction_coding(score_1_5, direction)]

    invalid_log <- long_dt[
      is.na(score_1_5),
      .(
        invalid_score_rows = .N,
        nonmissing_raw_values = sum(!is_missing_label(raw_value), na.rm = TRUE),
        raw_value_examples = paste(
          head(unique(as.character(raw_value[!is_missing_label(raw_value)])), 8),
          collapse = " | "
        )
      ),
      by = .(survey, file_name, file_path, demo, raw_question_column_clean, variable, unique, question, scale_type, direction)
    ]

    if (nrow(invalid_log) > 0) {
      invalid_score_list[[length(invalid_score_list) + 1]] <- invalid_log
    }

    long_dt <- long_dt[!is.na(score_1_5)]

    if (nrow(long_dt) == 0) {
      next
    }

    long_dt[, `:=`(
      dataset = survey_id,
      source_file = path,
      source_file_name = file_name,
      source_file_mtime = as.character(file_row$file_mtime[[1]]),
      source_file_size = as.numeric(file_row$file_size[[1]]),
      respondent_id = as.character(.respondent_id),
      date = as.Date(.date),
      date_source = as.character(.date_source),
      date_source_column = as.character(.date_source_column),
      year = as.integer(.year),
      quarter = as.integer(.quarter_num),
      month = as.integer(.month_num),
      week = as.integer(.week_num),
      quarter_period = as.character(.quarter_period),
      year_period = as.character(.year_period),
      month_period = as.character(.month_period),
      week_period = as.character(.week_period),
      weight = safe_as_numeric(.weight),
      raw_value = as.character(raw_value)
    )]

    keep_response_cols <- names(empty_response_long())
    long_dt <- add_missing_columns(long_dt, keep_response_cols)
    demo_long_list[[length(demo_long_list) + 1]] <- long_dt[, ..keep_response_cols]
  }

  response_long <- data.table::rbindlist(demo_long_list, fill = TRUE)
  invalid_score_log <- data.table::rbindlist(invalid_score_list, fill = TRUE)

  file_log$status <- "processed"
  file_log$response_rows <- nrow(response_long)
  file_log$invalid_score_rows <- if (nrow(invalid_score_log) > 0) {
    sum(invalid_score_log$invalid_score_rows, na.rm = TRUE)
  } else {
    0L
  }
  file_log$message <- "Processed registry-matched questions."

  list(
    response_long = response_long,
    file_log = file_log,
    question_log = question_log,
    invalid_score_log = invalid_score_log,
    name_log = name_log,
    c360_qn_df = c360_qn_df,
    period_guard_log = period_guard_log
  )
}

if (nrow(files_to_process) == 0) {
  response_long <- empty_response_long()
  c360_qn_df_respondent <- empty_c360_qn_df()
  file_ingest_log <- empty_file_log()
  ingest_question_log <- data.table::data.table()
  invalid_score_log <- data.table::data.table()
  raw_name_log <- data.table::data.table()
  period_guard_log <- empty_period_guard_log()
  processed_files_for_manifest <- data.table::data.table()
} else {
  results <- c360_progress_lapply(
    nrow(files_to_process),
    "Ingesting raw file",
    function(i) process_one_file(files_to_process[i], registry)
  )

  response_long <- data.table::rbindlist(lapply(results, `[[`, "response_long"), fill = TRUE)
  c360_qn_df_respondent <- if (isTRUE(WRITE_LEGACY_RESPONDENT_QN_DF)) {
    data.table::rbindlist(lapply(results, `[[`, "c360_qn_df"), fill = TRUE)
  } else {
    empty_c360_qn_df()
  }
  file_ingest_log <- data.table::rbindlist(lapply(results, `[[`, "file_log"), fill = TRUE)
  ingest_question_log <- data.table::rbindlist(lapply(results, `[[`, "question_log"), fill = TRUE)
  invalid_score_log <- data.table::rbindlist(lapply(results, `[[`, "invalid_score_log"), fill = TRUE)
  raw_name_log <- data.table::rbindlist(lapply(results, `[[`, "name_log"), fill = TRUE)
  period_guard_log <- data.table::rbindlist(
    lapply(results, function(x) x$period_guard_log %||% empty_period_guard_log()),
    fill = TRUE
  )

  processed_files_for_manifest <- file_ingest_log[
    status == "processed",
    .(
      survey,
      file_name,
      file_path,
      file_mtime = files_to_process$file_mtime[match(file_path, files_to_process$file_path)],
      file_size = files_to_process$file_size[match(file_path, files_to_process$file_path)],
      processed_at = as.character(Sys.time()),
      response_rows
    )
  ]
}

write_log(period_guard_log, file.path(LOG_DIR, paste0(Sys.Date(), "_03_period_guard_log.csv")))

saveRDS(response_long, file.path(INTERMEDIATE_DIR, "03_response_long.rds"))
saveRDS(processed_files_for_manifest, file.path(INTERMEDIATE_DIR, "03_processed_files_for_manifest.rds"))

respondent_key_details <- make_respondent_key_details(response_long)
respondent_level_current_run_csv <- file.path(INTERMEDIATE_DIR, "03_response_key_details_current_run.csv")
data.table::fwrite(respondent_key_details, respondent_level_current_run_csv, na = "")

respondent_level_exports <- data.table::data.table(
  output = "current_run_key_details",
  period = "all",
  rows = nrow(respondent_key_details),
  path = respondent_level_current_run_csv
)

if (isTRUE(WRITE_LEGACY_RESPONDENT_QN_DF)) {
  c360_qn_df_current_run_csv <- if (exists("C360_QN_DF_CURRENT_RUN_CSV")) {
    C360_QN_DF_CURRENT_RUN_CSV
  } else {
    file.path(
      INTERMEDIATE_DIR,
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_respondent_level_c360_current_run.csv")
    )
  }
  c360_qn_df_current_run <- normalise_c360_qn_df_for_export(c360_qn_df_respondent)
  data.table::fwrite(
    c360_qn_df_current_run,
    c360_qn_df_current_run_csv,
    na = ""
  )

  existing_c360_qn_df_path <- if (exists("EXISTING_C360_QN_DF_CSV")) {
    EXISTING_C360_QN_DF_CSV
  } else {
    file.path(DATA_DIR, "c360_qn_df.csv")
  }
  existing_c360_qn_df <- load_existing_c360_qn_df(existing_c360_qn_df_path)
  c360_qn_df_bolt_on <- bolt_on_c360_qn_df(existing_c360_qn_df, c360_qn_df_current_run)
  c360_qn_df_ytd_updated <- normalise_c360_qn_df_for_export(c360_qn_df_bolt_on$data)
  c360_qn_df_ytd_updated_csv <- if (exists("C360_QN_DF_YTD_UPDATED_CSV")) {
    C360_QN_DF_YTD_UPDATED_CSV
  } else {
    file.path(
      INTERMEDIATE_DIR,
      paste0(format(Sys.time(), "%Y-%m-%d_%H%M"), "_respondent_level_c360.csv")
    )
  }
  data.table::fwrite(c360_qn_df_ytd_updated, c360_qn_df_ytd_updated_csv, na = "")

  respondent_level_exports <- data.table::rbindlist(
    list(
      respondent_level_exports,
      data.table::data.table(
        output = "legacy_c360_qn_df_current_run",
        period = "all",
        rows = nrow(c360_qn_df_current_run),
        path = c360_qn_df_current_run_csv
      ),
      data.table::data.table(
        output = "legacy_c360_qn_df_ytd_updated",
        period = "all",
        rows = nrow(c360_qn_df_ytd_updated),
        path = c360_qn_df_ytd_updated_csv,
        existing_path = existing_c360_qn_df_path,
        existing_rows = nrow(existing_c360_qn_df),
        current_run_rows = nrow(c360_qn_df_current_run),
        rows_replaced = c360_qn_df_bolt_on$rows_replaced
      )
    ),
    fill = TRUE
  )
} else {
  respondent_level_exports <- data.table::rbindlist(
    list(
      respondent_level_exports,
      data.table::data.table(
        output = "legacy_c360_qn_df_skipped",
        period = "all",
        rows = 0L,
        path = "",
        message = "Skipped because C360_WRITE_LEGACY_RESPONDENT_QN_DF is FALSE. Enable to collate respondent-question rows, bolt them onto c360_qn_df.csv, and write the dated respondent_level_c360.csv file."
      )
    ),
    fill = TRUE
  )
}

respondent_level_latest_quarter_csv <- NA_character_
respondent_level_latest_quarter_period <- NA_character_
if (nrow(response_long) > 0 && "quarter_period" %in% names(response_long)) {
  quarter_candidates <- unique(response_long[
    !is.na(quarter_period) & stringr::str_trim(as.character(quarter_period)) != "",
    .(quarter_period)
  ])

  if (nrow(quarter_candidates) > 0) {
    quarter_candidates[, period_order := period_order_value("Quarter", quarter_period)]
    data.table::setorder(quarter_candidates, period_order)
    respondent_level_latest_quarter_period <- quarter_candidates$quarter_period[[nrow(quarter_candidates)]]

    latest_quarter_response_long <- respondent_key_details[quarter_period == respondent_level_latest_quarter_period]
    safe_period_name <- stringr::str_replace_all(respondent_level_latest_quarter_period, "[^A-Za-z0-9]+", "_")
    safe_period_name <- stringr::str_replace_all(safe_period_name, "^_|_$", "")
    respondent_level_latest_quarter_csv <- file.path(
      INTERMEDIATE_DIR,
      paste0("03_response_key_details_", safe_period_name, ".csv")
    )
    data.table::fwrite(latest_quarter_response_long, respondent_level_latest_quarter_csv, na = "")

    respondent_level_exports <- data.table::rbindlist(
      list(
        respondent_level_exports,
        data.table::data.table(
          output = "latest_quarter_key_details",
          period = respondent_level_latest_quarter_period,
          rows = nrow(latest_quarter_response_long),
          path = respondent_level_latest_quarter_csv
        )
      ),
      fill = TRUE
    )
  }
}

write_log(file_ingest_log, file.path(LOG_DIR, paste0(Sys.Date(), "_03_file_ingest_log.csv")))
write_log(ingest_question_log, file.path(LOG_DIR, paste0(Sys.Date(), "_03_question_ingest_log.csv")))
write_log(invalid_score_log, file.path(LOG_DIR, paste0(Sys.Date(), "_03_invalid_score_rows.csv")))
write_log(raw_name_log, file.path(LOG_DIR, paste0(Sys.Date(), "_03_raw_name_standardisation_log.csv")))
write_log(respondent_level_exports, file.path(LOG_DIR, paste0(Sys.Date(), "_03_respondent_level_exports.csv")))

response_summary <- if (nrow(response_long) > 0) {
  response_long[
    ,
    .(
      response_rows = .N,
      questions = data.table::uniqueN(unique),
      raw_variables = data.table::uniqueN(variable),
      respondents = data.table::uniqueN(respondent_id),
      periods = data.table::uniqueN(quarter_period)
    ),
    by = dataset
  ][order(dataset)]
} else {
  data.table::data.table()
}

write_log(response_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_03_response_summary.csv")))

message("Response-level rows: ", nrow(response_long))
message("Respondent-level current-run CSV saved to: ", respondent_level_current_run_csv)
if (!is.na(respondent_level_latest_quarter_csv)) {
  message(
    "Respondent-level latest-quarter CSV saved to: ",
    respondent_level_latest_quarter_csv,
    " (",
    respondent_level_latest_quarter_period,
    ")"
  )
}
message("Files processed: ", sum(file_ingest_log$status == "processed", na.rm = TRUE))
message("Invalid/missing score rows logged: ", nrow(invalid_score_log))
message("03_ingest_response_level.R completed.")
