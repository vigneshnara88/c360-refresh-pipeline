# ============================================================
# 06_run_pipeline.R
# Master runner for the C360 registry-driven quarterly pipeline
# ============================================================

run_start_time <- Sys.time()

message("============================================================")
message("C360 registry-driven pipeline started")
message("Start time: ", format(run_start_time, "%Y-%m-%d %H:%M:%S"))
message("============================================================")

initial_working_dir <- getwd()
config_path <- file.path(initial_working_dir, "00_config.R")

if (!file.exists(config_path)) {
  stop(
    "Cannot find 00_config.R in current working directory:\n",
    initial_working_dir,
    "\n\nSet your working directory to the pipeline folder, then run:\n",
    "source('06_run_pipeline.R')"
  )
}

run_step <- function(step_name, step_path) {
  if (!file.exists(step_path)) {
    stop("Missing pipeline script: ", step_path)
  }

  message("")
  message("------------------------------------------------------------")
  message("Running ", step_name)
  message("Script: ", step_path)
  message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  message("------------------------------------------------------------")

  step_start <- Sys.time()
  source(step_path, local = FALSE)
  invisible(gc())
  step_end <- Sys.time()
  step_duration <- as.numeric(difftime(step_end, step_start, units = "mins"))

  message("Completed ", step_name)
  message("Duration: ", round(step_duration, 2), " minutes")
}

run_step("00_config.R", config_path)
setwd(PROJECT_DIR)

write_analyst_run_summary <- function(stage) {
  collapse_counts <- function(dt, group_col, value_col = NULL) {
    dt <- data.table::as.data.table(dt)
    if (nrow(dt) == 0 || !(group_col %in% names(dt))) {
      return("")
    }
    if (is.null(value_col)) {
      counts <- dt[, .N, by = c(group_col)]
      paste(paste(counts[[group_col]], counts$N, sep = "="), collapse = "; ")
    } else {
      counts <- dt[, .(value = sum(get(value_col), na.rm = TRUE)), by = c(group_col)]
      paste(paste(counts[[group_col]], counts$value, sep = "="), collapse = "; ")
    }
  }

  metric <- function(name, value) {
    data.table::data.table(metric = name, value = as.character(value %||% ""))
  }

  registry_dt <- if (exists("registry")) data.table::as.data.table(registry) else data.table::data.table()
  file_index_dt <- if (exists("file_index")) data.table::as.data.table(file_index) else data.table::data.table()
  files_to_process_dt <- if (exists("files_to_process")) data.table::as.data.table(files_to_process) else data.table::data.table()
  match_dt <- if (exists("question_match_log")) data.table::as.data.table(question_match_log) else data.table::data.table()
  drift_dt <- if (exists("question_drift_review")) data.table::as.data.table(question_drift_review) else data.table::data.table()
  ingest_dt <- if (exists("file_ingest_log")) data.table::as.data.table(file_ingest_log) else data.table::data.table()
  invalid_dt <- if (exists("invalid_score_log")) data.table::as.data.table(invalid_score_log) else data.table::data.table()
  period_guard_dt <- if (exists("period_guard_log")) data.table::as.data.table(period_guard_log) else data.table::data.table()

  matched_questions <- if (nrow(match_dt) > 0 && "available_in_file" %in% names(match_dt)) {
    sum(match_dt$available_in_file, na.rm = TRUE)
  } else if (exists("ingest_question_log") && nrow(ingest_question_log) > 0) {
    sum(ingest_question_log$available_in_file, na.rm = TRUE)
  } else {
    NA_integer_
  }

  missing_questions <- if (nrow(match_dt) > 0 && "available_in_file" %in% names(match_dt)) {
    sum(!match_dt$available_in_file, na.rm = TRUE)
  } else {
    NA_integer_
  }

  summary <- data.table::rbindlist(
    list(
      metric("stage", stage),
      metric("run_mode", RUN_MODE),
      metric("preflight_only", RUN_PREFLIGHT_ONLY),
      metric("full_rebuild", FULL_REBUILD),
      metric("surveys_to_run", paste(SURVEYS_TO_RUN, collapse = ", ")),
      metric("target_update_quarter", if (exists("TARGET_UPDATE_QUARTER") && !is.na(TARGET_UPDATE_QUARTER)) TARGET_UPDATE_QUARTER else ""),
      metric("run_output_dir", if (exists("RUN_OUTPUT_DIR")) RUN_OUTPUT_DIR else ""),
      metric("exports_dir", if (exists("OUTPUT_DIR")) OUTPUT_DIR else ""),
      metric("registry_rows_loaded", nrow(registry_dt)),
      metric("registry_rows_by_dataset", collapse_counts(registry_dt, "dataset")),
      metric("dashboard_include_registry_rows", if (nrow(registry_dt) > 0) sum(registry_dt$include_in_dashboard == "Include", na.rm = TRUE) else NA_integer_),
      metric("key_question_registry_rows", if (nrow(registry_dt) > 0) sum(registry_dt$Keyqn == "Yes", na.rm = TRUE) else NA_integer_),
      metric("direction_check_registry_rows", if (nrow(registry_dt) > 0) sum(registry_dt$direction == "Check", na.rm = TRUE) else NA_integer_),
      metric("files_discovered", nrow(file_index_dt)),
      metric("files_discovered_by_dataset", collapse_counts(file_index_dt, "survey")),
      metric("files_selected_for_processing", nrow(files_to_process_dt)),
      metric("files_selected_by_dataset", collapse_counts(files_to_process_dt, "survey")),
      metric("registry_question_matches", matched_questions),
      metric("registry_question_missing", missing_questions),
      metric("drift_review_rows", nrow(drift_dt)),
      metric("files_processed", if (nrow(ingest_dt) > 0) sum(ingest_dt$status == "processed", na.rm = TRUE) else 0L),
      metric("response_rows", if (exists("response_long")) nrow(response_long) else NA_integer_),
      metric("invalid_score_log_rows", nrow(invalid_dt)),
      metric(
        "target_quarter_guard_issues",
        if (nrow(period_guard_dt) > 0) {
          sum(period_guard_dt$issue_type == "explicit_source_date_outside_target_quarter", na.rm = TRUE)
        } else {
          0L
        }
      ),
      metric("full_output_rows", if (exists("full_output")) nrow(full_output) else NA_integer_),
      metric("dashboard_output_rows", if (exists("dashboard_output")) nrow(dashboard_output) else NA_integer_),
      metric("key_question_output_rows", if (exists("key_question_output")) nrow(key_question_output) else NA_integer_),
      metric("internal_rows", if (exists("internal_tableau_wide")) nrow(internal_tableau_wide) else NA_integer_),
      metric("internal_trimmed_rows", if (exists("internal_tableau_trimmed")) nrow(internal_tableau_trimmed) else NA_integer_),
      metric("external_trimmed_rows", if (exists("external_tableau_trimmed")) nrow(external_tableau_trimmed) else NA_integer_),
      metric("full_output_csv", if (exists("FULL_OUTPUT_CSV")) FULL_OUTPUT_CSV else ""),
      metric("dashboard_output_csv", if (exists("DASHBOARD_OUTPUT_CSV")) DASHBOARD_OUTPUT_CSV else ""),
      metric("key_question_output_csv", if (exists("KEY_QUESTION_OUTPUT_CSV")) KEY_QUESTION_OUTPUT_CSV else ""),
      metric("internal_csv", if (exists("INTERNAL_TABLEAU_WIDE_CSV")) INTERNAL_TABLEAU_WIDE_CSV else ""),
      metric("internal_trimmed_csv", if (exists("INTERNAL_TABLEAU_TRIMMED_CSV")) INTERNAL_TABLEAU_TRIMMED_CSV else ""),
      metric("external_trimmed_csv", if (exists("EXTERNAL_TABLEAU_TRIMMED_CSV")) EXTERNAL_TABLEAU_TRIMMED_CSV else ""),
      metric("logs_dir", LOG_DIR)
    ),
    fill = TRUE
  )

  path <- file.path(LOG_DIR, paste0(Sys.Date(), "_ANALYST_RUN_SUMMARY.csv"))
  write_log(summary, path)
  message("Analyst run summary saved to: ", path)
  invisible(summary)
}

write_actionable_qa_outputs <- function(stage) {
  as_dt <- function(name) {
    if (exists(name, inherits = TRUE)) {
      data.table::as.data.table(get(name, inherits = TRUE))
    } else {
      data.table::data.table()
    }
  }

  metric <- function(name, value, action = "") {
    data.table::data.table(
      stage = stage,
      metric = name,
      value = as.character(value %||% ""),
      action = action
    )
  }

  registry_dt <- as_dt("registry")
  files_to_process_dt <- as_dt("files_to_process")
  match_dt <- as_dt("question_match_log")
  drift_dt <- as_dt("question_drift_review")
  unmatched_raw_dt <- as_dt("unmatched_raw_question_review")
  source_raw_dt <- as_dt("source_raw_question_match_log")
  ingest_dt <- as_dt("file_ingest_log")
  invalid_dt <- as_dt("invalid_score_log")
  response_dt <- as_dt("response_long")
  full_dt <- as_dt("full_output")
  respondent_level_exports_dt <- as_dt("respondent_level_exports")
  period_guard_dt <- as_dt("period_guard_log")

  keep_relevant_issue_rows <- function(dt) {
    dt <- data.table::as.data.table(dt)
    if (nrow(dt) == 0) {
      return(dt)
    }

    if ("survey" %in% names(dt)) {
      dt <- dt[tolower(trimws(as.character(survey))) %in% tolower(trimws(as.character(SURVEYS_TO_RUN)))]
    }

    if (
      nrow(files_to_process_dt) > 0 &&
        all(c("file_path") %in% names(dt)) &&
        "file_path" %in% names(files_to_process_dt)
    ) {
      dt <- dt[file_path %in% files_to_process_dt$file_path]
    } else if (
      nrow(files_to_process_dt) > 0 &&
        all(c("file_name", "survey") %in% names(dt)) &&
        all(c("file_name", "survey") %in% names(files_to_process_dt))
    ) {
      relevant_file_keys <- unique(paste(files_to_process_dt$survey, files_to_process_dt$file_name, sep = "||"))
      dt <- dt[paste(survey, file_name, sep = "||") %in% relevant_file_keys]
    }

    dt
  }

  match_dt <- keep_relevant_issue_rows(match_dt)
  drift_dt <- keep_relevant_issue_rows(drift_dt)
  unmatched_raw_dt <- keep_relevant_issue_rows(unmatched_raw_dt)
  source_raw_dt <- keep_relevant_issue_rows(source_raw_dt)
  invalid_dt <- keep_relevant_issue_rows(invalid_dt)

  get_scalar <- function(name, default = NA_integer_) {
    if (exists(name, inherits = TRUE)) {
      value <- get(name, inherits = TRUE)
      if (length(value) > 0) {
        return(value[[1]])
      }
    }
    default
  }

  matched_file_pairs <- if (nrow(match_dt) > 0 && "available_in_file" %in% names(match_dt)) {
    sum(match_dt$available_in_file, na.rm = TRUE)
  } else if (exists("ingest_question_log") && nrow(ingest_question_log) > 0) {
    sum(ingest_question_log$available_in_file, na.rm = TRUE)
  } else {
    NA_integer_
  }

  missing_file_pairs <- if (nrow(match_dt) > 0 && "available_in_file" %in% names(match_dt)) {
    sum(!match_dt$available_in_file, na.rm = TRUE)
  } else {
    NA_integer_
  }
  drift_dt <- add_missing_columns(drift_dt, c("include_in_dashboard", "Keyqn"))
  missing_registry_questions <- if (nrow(drift_dt) > 0) nrow(drift_dt) else 0L
  missing_dashboard_registry_questions <- if (nrow(drift_dt) > 0) {
    sum(drift_dt$include_in_dashboard == "Include", na.rm = TRUE)
  } else {
    0L
  }

  nonmissing_invalid <- if (nrow(invalid_dt) > 0 && "nonmissing_raw_values" %in% names(invalid_dt)) {
    sum(invalid_dt$nonmissing_raw_values, na.rm = TRUE)
  } else {
    0L
  }

  source_raw_question_columns <- if (nrow(source_raw_dt) > 0 && "raw_column_clean" %in% names(source_raw_dt)) {
    data.table::uniqueN(source_raw_dt$raw_column_clean, na.rm = TRUE)
  } else {
    0L
  }
  source_raw_questions_mapped <- if (
    nrow(source_raw_dt) > 0 &&
      all(c("mapped_to_registry", "raw_column_clean") %in% names(source_raw_dt))
  ) {
    data.table::uniqueN(source_raw_dt[mapped_to_registry == TRUE, raw_column_clean], na.rm = TRUE)
  } else {
    0L
  }

  summary <- data.table::rbindlist(
    list(
      metric("run_mode", RUN_MODE),
      metric("surveys_to_run", paste(SURVEYS_TO_RUN, collapse = ", ")),
      metric("target_update_quarter", if (exists("TARGET_UPDATE_QUARTER") && !is.na(TARGET_UPDATE_QUARTER)) TARGET_UPDATE_QUARTER else ""),
      metric("run_output_dir", if (exists("RUN_OUTPUT_DIR")) RUN_OUTPUT_DIR else ""),
      metric("exports_dir", if (exists("OUTPUT_DIR")) OUTPUT_DIR else ""),
      metric("qc_run_dir", if (exists("QC_RUN_DIR")) QC_RUN_DIR else ""),
      metric("registry_rows_loaded", nrow(registry_dt)),
      metric("files_selected_for_processing", nrow(files_to_process_dt), "If zero when new data were expected, inspect the manifest and data/<survey> folders."),
      metric("files_processed", if (nrow(ingest_dt) > 0) sum(ingest_dt$status == "processed", na.rm = TRUE) else 0L),
      metric("raw_respondent_rows_read", if (nrow(ingest_dt) > 0) sum(ingest_dt$raw_rows, na.rm = TRUE) else NA_integer_),
      metric("respondent_level_rows_created", if (nrow(response_dt) > 0) nrow(response_dt) else if (exists("response_long")) 0L else NA_integer_),
      metric("respondent_key_details_current_run_csv", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "current_run_key_details", path][1] else ""),
      metric("respondent_level_latest_quarter", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "latest_quarter_key_details", period][1] else ""),
      metric("respondent_level_latest_quarter_rows", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "latest_quarter_key_details", rows][1] else NA_integer_),
      metric("respondent_key_details_latest_quarter_csv", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "latest_quarter_key_details", path][1] else ""),
      metric("c360_qn_df_current_run_csv", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "legacy_c360_qn_df_current_run", path][1] else ""),
      metric("c360_qn_df_ytd_updated_rows", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "legacy_c360_qn_df_ytd_updated", rows][1] else NA_integer_),
      metric("c360_qn_df_ytd_updated_csv", if (nrow(respondent_level_exports_dt) > 0) respondent_level_exports_dt[output == "legacy_c360_qn_df_ytd_updated", path][1] else ""),
      metric(
        "target_quarter_guard_issues",
        if (nrow(period_guard_dt) > 0) {
          sum(period_guard_dt$issue_type == "explicit_source_date_outside_target_quarter", na.rm = TRUE)
        } else {
          0L
        }
      ),
      metric("matched_registry_question_file_pairs", matched_file_pairs),
      metric("missing_registry_question_file_pairs", missing_file_pairs, "Review QA_3_question_review.csv when non-zero."),
      metric("missing_dashboard_registry_questions_flagged", missing_dashboard_registry_questions, "Dashboard-included registry questions missing from selected raw files; these block ingestion by default."),
      metric("missing_registry_questions_flagged", missing_registry_questions, "Registry-listed questions not found in the selected raw files."),
      metric("source_raw_question_columns", source_raw_question_columns),
      metric("source_raw_question_columns_mapped_to_registry", source_raw_questions_mapped),
      metric("new_questions_detected_not_in_registry", nrow(unmatched_raw_dt), "Review QA_4_new_questions_detected.csv after missing registry questions are resolved. These source columns are not ingested unless added to the registry."),
      metric("nonmissing_invalid_score_values", nonmissing_invalid, "If non-zero, inspect 03_invalid_score_rows and confirm scale/label mapping."),
      metric("existing_output_rows_before_bolt_on", get_scalar("existing_output_rows_before_bolt_on")),
      metric("existing_output_rows_replaced", get_scalar("existing_output_rows_replaced"), "Rows matching the new update key were removed before current-run rows were appended."),
      metric("new_output_rows_from_current_run", get_scalar("new_output_rows_from_current_run"), "These are aggregate output rows generated from the current run, not raw respondent rows."),
      metric("net_output_row_change", get_scalar("net_output_row_change")),
      metric("full_output_rows", if (nrow(full_dt) > 0) nrow(full_dt) else if (exists("full_output")) 0L else NA_integer_),
      metric("full_output_2026_q1_rows", if (nrow(full_dt) > 0 && "Period" %in% names(full_dt)) sum(full_dt$Period == "2026 Q1", na.rm = TRUE) else NA_integer_)
    ),
    fill = TRUE
  )

  qa1_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_1_run_summary.csv"))
  write_log(summary, qa1_path)
  qc_dir <- if (exists("QC_RUN_DIR")) QC_RUN_DIR else file.path(OUTPUT_DIR, "qc", as.character(Sys.Date()))
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
  old_clean_qc_files <- list.files(qc_dir, pattern = "^QA_.*\\.csv$", full.names = TRUE)
  if (length(old_clean_qc_files) > 0) {
    unlink(old_clean_qc_files)
  }
  qa1_qc_path <- file.path(qc_dir, "QA_1_run_summary.csv")
  write_log(summary, qa1_qc_path)

  if (nrow(ingest_dt) > 0) {
    survey_file_summary <- data.table::copy(ingest_dt)
    survey_file_summary[, match_rate := ifelse(registry_questions > 0, matched_questions / registry_questions, NA_real_)]
  } else if (nrow(match_dt) > 0) {
    survey_file_summary <- match_dt[
      ,
      .(
        status = "preflight_match_checked",
        raw_rows = NA_integer_,
        registry_questions = .N,
        matched_questions = sum(available_in_file, na.rm = TRUE),
        missing_questions = sum(!available_in_file, na.rm = TRUE),
        response_rows = NA_integer_,
        invalid_score_rows = NA_integer_,
        message = "Preflight only: no response rows created."
      ),
      by = .(survey, file_name, file_path)
    ]
    survey_file_summary[, match_rate := ifelse(registry_questions > 0, matched_questions / registry_questions, NA_real_)]
  } else {
    survey_file_summary <- data.table::data.table(
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
      message = character(),
      match_rate = numeric()
    )
  }

  if (nrow(files_to_process_dt) > 0 && "file_path" %in% names(survey_file_summary)) {
    file_reasons <- files_to_process_dt[, .(file_path, selection_reason = reason)]
    survey_file_summary <- merge(
      survey_file_summary,
      file_reasons,
      by = "file_path",
      all.x = TRUE,
      sort = FALSE
    )
  }

  if (nrow(source_raw_dt) > 0 && all(c("file_path", "mapped_to_registry", "raw_column_clean") %in% names(source_raw_dt))) {
    source_question_counts <- source_raw_dt[
      ,
      .(
        source_raw_question_columns = data.table::uniqueN(raw_column_clean, na.rm = TRUE),
        source_raw_question_columns_mapped = data.table::uniqueN(raw_column_clean[mapped_to_registry == TRUE], na.rm = TRUE),
        source_raw_question_columns_unmatched = data.table::uniqueN(raw_column_clean[mapped_to_registry == FALSE], na.rm = TRUE)
      ),
      by = .(file_path)
    ]
    survey_file_summary <- merge(
      survey_file_summary,
      source_question_counts,
      by = "file_path",
      all.x = TRUE,
      sort = FALSE
    )
  }

  survey_file_summary <- add_missing_columns(
    survey_file_summary,
    c(
      "survey", "file_name", "file_path", "selection_reason", "status",
      "raw_rows", "registry_questions", "matched_questions",
      "missing_questions", "match_rate", "response_rows",
      "invalid_score_rows", "pgp_weight_cap_after_date",
      "pgp_weight_cap_limit", "pgp_post_cap_rows_capped",
      "pgp_post_cap_rows_above_limit", "source_raw_question_columns",
      "source_raw_question_columns_mapped",
      "source_raw_question_columns_unmatched", "message"
    )
  )
  survey_file_summary <- survey_file_summary[
    ,
    .(
      survey, file_name, selection_reason, status, raw_rows,
      registry_questions, matched_questions, missing_questions,
      match_rate = round(safe_as_numeric(match_rate), 3),
      source_raw_question_columns,
      source_raw_question_columns_mapped,
      source_raw_question_columns_unmatched,
      response_rows, invalid_score_rows,
      pgp_weight_cap_after_date,
      pgp_weight_cap_limit,
      pgp_post_cap_rows_capped,
      pgp_post_cap_rows_above_limit,
      message, file_path
    )
  ][order(survey, file_name)]

  qa2_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_2_survey_file_summary.csv"))
  write_log(survey_file_summary, qa2_path)
  qa2_qc_path <- file.path(qc_dir, "QA_2_survey_file_summary.csv")
  write_log(survey_file_summary, qa2_qc_path)

  collapse_unique_values <- function(x, n = 8) {
    values <- unique(as.character(x))
    values <- values[!is.na(values) & stringr::str_trim(values) != ""]
    paste(head(values, n), collapse = " | ")
  }

  collapse_pipe_examples <- function(x, n = 8) {
    values <- unlist(strsplit(paste(as.character(x), collapse = " | "), "\\s*\\|\\s*"))
    values <- unique(values[!is.na(values) & stringr::str_trim(values) != ""])
    paste(head(values, n), collapse = " | ")
  }

  new_question_family <- function(x) {
    x <- stringr::str_to_lower(as.character(x))
    family <- stringr::str_match(x, "^([a-z]+[0-9]+[a-z]*)(?:[_-].*)?$")[, 2]
    family[is.na(family) | family == ""] <- x[is.na(family) | family == ""]
    family
  }

  nearest_distance_min <- function(x) {
    vals <- unlist(strsplit(as.character(x), "\\s*\\|\\s*"))
    vals <- suppressWarnings(as.numeric(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) NA_real_ else min(vals)
  }

  fixed_contains <- function(haystack, needle) {
    haystack <- as.character(haystack %||% "")
    needle <- as.character(needle %||% "")
    if (length(needle) == 0 || is.na(needle) || stringr::str_trim(needle) == "") {
      return(FALSE)
    }
    grepl(needle, haystack, fixed = TRUE)
  }

  reconciliation_noise_column <- function(x) {
    x <- clean_col(x)
    grepl("^(d[0-9]+[a-z]?(?:_[a-z0-9]+)?|postalcode|qtr|q4_(months|years))$", x)
  }

  no_selected_files <- nrow(files_to_process_dt) == 0

  missing_registry_questions <- if (nrow(drift_dt) > 0) {
    drift_dt <- add_missing_columns(drift_dt, c("include_in_dashboard", "Keyqn"))
    drift_dt[, .(
      issue_type = "registry_question_missing_from_raw_file",
      survey,
      file_name,
      variable,
      unique,
      include_in_dashboard,
      Keyqn,
      registry_question = registry_question,
      registry_raw_question_column,
      best_raw_column_candidates,
      best_raw_column_distances,
      best_question_text_candidates,
      best_question_text_distances,
      review_action = "Registry-listed question was not found in the raw file. Check whether the source renamed/removed it; update the registry only after owner confirmation."
    )]
  } else if (no_selected_files) {
    data.table::data.table(
      issue_type = "not_checked_no_files_selected",
      survey = NA_character_,
      file_name = NA_character_,
      variable = NA_character_,
      unique = NA_character_,
      include_in_dashboard = NA_character_,
      Keyqn = NA_character_,
      registry_question = NA_character_,
      registry_raw_question_column = NA_character_,
      best_raw_column_candidates = NA_character_,
      best_raw_column_distances = NA_character_,
      best_question_text_candidates = NA_character_,
      best_question_text_distances = NA_character_,
      review_action = "No files were selected, so registry-question presence was not checked."
    )
  } else {
    data.table::data.table(
      issue_type = "none",
      survey = NA_character_,
      file_name = NA_character_,
      variable = NA_character_,
      unique = NA_character_,
      include_in_dashboard = NA_character_,
      Keyqn = NA_character_,
      registry_question = NA_character_,
      registry_raw_question_column = NA_character_,
      best_raw_column_candidates = NA_character_,
      best_raw_column_distances = NA_character_,
      best_question_text_candidates = NA_character_,
      best_question_text_distances = NA_character_,
      review_action = "No registry-listed questions were missing from the selected raw files."
    )
  }

  qa3_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_3_missing_registry_questions.csv"))
  write_log(missing_registry_questions, qa3_path)

  missing_dashboard_registry_questions <- if (
    nrow(missing_registry_questions) > 0 &&
      all(c("issue_type", "include_in_dashboard") %in% names(missing_registry_questions))
  ) {
    out <- missing_registry_questions[
      issue_type == "registry_question_missing_from_raw_file" &
        include_in_dashboard == "Include"
    ]
    if (nrow(out) > 0) {
      out
    } else {
      data.table::data.table(
        issue_type = "none",
        review_action = "No dashboard-included registry questions were missing from the selected raw files."
      )
    }
  } else {
    data.table::data.table(
      issue_type = "none",
      review_action = "No dashboard-included registry questions were missing from the selected raw files."
    )
  }

  new_questions_detail <- if (nrow(unmatched_raw_dt) > 0) {
    out <- unmatched_raw_dt[, .(
      issue_type = "new_question_detected_not_in_registry",
      survey,
      file_name,
      raw_column,
      source_column_family = new_question_family(raw_column),
      nearest_registry_variables,
      nearest_registry_variable_distances,
      nearest_registry_questions,
      nearest_registry_question_distances,
      review_action = "Source question-like column is not in the registry and was not ingested. Analyst should confirm whether to add it to the registry or document exclusion."
    )]
    out[, nearest_question_distance_min := vapply(nearest_registry_question_distances, nearest_distance_min, numeric(1))]
    out[, nearest_variable_distance_min := vapply(nearest_registry_variable_distances, nearest_distance_min, numeric(1))]
    out[, review_priority := data.table::fcase(
      !is.na(nearest_question_distance_min) & nearest_question_distance_min <= 10, "high_near_existing_question",
      !is.na(nearest_variable_distance_min) & nearest_variable_distance_min <= 3, "medium_near_existing_variable",
      default = "low_new_or_out_of_scope_module"
    )]
    out[, columns_in_same_family := .N, by = .(survey, file_name, source_column_family)]
    out[, review_priority_rank := match(
      review_priority,
      c("high_near_existing_question", "medium_near_existing_variable", "low_new_or_out_of_scope_module")
    )]
    data.table::setorder(out, review_priority_rank, survey, file_name, source_column_family, raw_column)
    out[, review_order := seq_len(.N)]
    out[
      order(review_order),
      .(
        review_order,
        review_priority,
        issue_type,
        survey,
        file_name,
        source_column_family,
        columns_in_same_family,
        raw_column,
        nearest_registry_variables,
        nearest_registry_variable_distances,
        nearest_registry_questions,
        nearest_registry_question_distances,
        review_action
      )
    ]
  } else {
    data.table::data.table()
  }

  qa4_detail_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_4_new_questions_detected_detail.csv"))
  write_log(new_questions_detail, qa4_detail_path)

  new_questions_detected <- if (nrow(new_questions_detail) > 0) {
    new_questions_detail
  } else if (no_selected_files) {
    data.table::data.table(
      review_order = NA_integer_,
      review_priority = "not_checked",
      issue_type = "not_checked_no_files_selected",
      survey = NA_character_,
      file_name = NA_character_,
      source_column_family = NA_character_,
      columns_in_same_family = 0L,
      raw_column = NA_character_,
      nearest_registry_variables = NA_character_,
      nearest_registry_variable_distances = NA_character_,
      nearest_registry_questions = NA_character_,
      nearest_registry_question_distances = NA_character_,
      review_action = "No files were selected, so new source questions were not checked."
    )
  } else {
    data.table::data.table(
      review_order = NA_integer_,
      review_priority = "ok",
      issue_type = "none",
      survey = NA_character_,
      file_name = NA_character_,
      source_column_family = NA_character_,
      columns_in_same_family = 0L,
      raw_column = NA_character_,
      nearest_registry_variables = NA_character_,
      nearest_registry_variable_distances = NA_character_,
      nearest_registry_questions = NA_character_,
      nearest_registry_question_distances = NA_character_,
      review_action = "No source question-like columns were detected outside the registry."
    )
  }

  qa4_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_4_new_questions_detected.csv"))
  write_log(new_questions_detected, qa4_path)

  reconciliation_candidates <- if (
    nrow(missing_registry_questions) > 0 &&
      nrow(new_questions_detail) > 0 &&
      !all(missing_registry_questions$issue_type == "none") &&
      !all(grepl("^not_checked", missing_registry_questions$issue_type))
  ) {
    missing_dt <- missing_registry_questions[
      issue_type == "registry_question_missing_from_raw_file"
    ]
    new_dt <- new_questions_detail[
      issue_type == "new_question_detected_not_in_registry"
    ]

    pairs <- merge(
      missing_dt,
      new_dt,
      by = c("survey", "file_name"),
      allow.cartesian = TRUE,
      suffixes = c("_registry", "_source")
    )

    if (nrow(pairs) > 0) {
      pairs[, source_column_is_missing_raw_candidate := mapply(
        fixed_contains,
        best_raw_column_candidates,
        raw_column
      )]
      pairs[, source_column_is_missing_wording_candidate := mapply(
        fixed_contains,
        best_question_text_candidates,
        raw_column
      )]
      pairs[, detected_source_points_to_missing_unique := mapply(
        fixed_contains,
        nearest_registry_questions,
        unique
      )]
      pairs[, detected_source_points_to_missing_variable := mapply(
        fixed_contains,
        nearest_registry_variables,
        registry_raw_question_column
      )]
      pairs[, match_basis := vapply(
        seq_len(.N),
        function(i) {
          basis <- c(
            if (source_column_is_missing_raw_candidate[[i]]) "source_column_is_missing_raw_candidate" else NA_character_,
            if (source_column_is_missing_wording_candidate[[i]]) "source_column_is_missing_wording_candidate" else NA_character_,
            if (detected_source_points_to_missing_unique[[i]]) "detected_source_points_to_missing_unique" else NA_character_,
            if (detected_source_points_to_missing_variable[[i]]) "detected_source_points_to_missing_variable" else NA_character_
          )
          paste(basis[!is.na(basis)], collapse = " | ")
        },
        character(1)
      )]
      pairs <- pairs[stringr::str_trim(match_basis) != ""]
    }

    if (nrow(pairs) > 0) {
      pairs[, is_noise_candidate := vapply(raw_column, reconciliation_noise_column, logical(1))]
      pairs <- pairs[is_noise_candidate == FALSE]
    }

    if (nrow(pairs) > 0) {
      pairs[, evidence_score :=
        3L * as.integer(source_column_is_missing_wording_candidate) +
        3L * as.integer(detected_source_points_to_missing_unique) +
        2L * as.integer(source_column_is_missing_raw_candidate) +
        2L * as.integer(detected_source_points_to_missing_variable)
      ]
      pairs[, code_distance := mapply(
        function(a, b) {
          as.integer(utils::adist(clean_col(a), clean_col(b))[[1]])
        },
        registry_raw_question_column,
        raw_column
      )]
      pairs[, reconciliation_priority := data.table::fcase(
        source_column_is_missing_wording_candidate |
          (source_column_is_missing_raw_candidate & detected_source_points_to_missing_unique) |
          (source_column_is_missing_raw_candidate & detected_source_points_to_missing_variable) |
          (detected_source_points_to_missing_unique & detected_source_points_to_missing_variable),
        "high_possible_logical_match",
        source_column_is_missing_raw_candidate |
          detected_source_points_to_missing_unique |
          detected_source_points_to_missing_variable,
        "medium_possible_code_or_wording_match",
        default = "review"
      )]
      pairs[, priority_rank := match(
        reconciliation_priority,
        c("high_possible_logical_match", "medium_possible_code_or_wording_match", "review")
      )]
      data.table::setorder(
        pairs,
        survey,
        file_name,
        unique,
        priority_rank,
        -evidence_score,
        code_distance,
        raw_column
      )
      top_n <- if (exists("QA_RECONCILIATION_TOP_N_PER_MISSING")) {
        QA_RECONCILIATION_TOP_N_PER_MISSING
      } else {
        3L
      }
      top_n <- as.integer(top_n %||% 3L)
      if (is.na(top_n) || top_n < 1L) {
        top_n <- 3L
      }
      pairs[, candidate_rank := seq_len(.N), by = .(survey, file_name, unique)]
      pairs <- pairs[candidate_rank <= top_n]
      data.table::setorder(pairs, priority_rank, survey, file_name, variable, candidate_rank)
      pairs[, review_order := seq_len(.N)]
      pairs[
        ,
        .(
          review_order,
          candidate_rank,
          reconciliation_priority,
          match_basis,
          evidence_score,
          code_distance,
          survey,
          file_name,
          registry_variable = variable,
          registry_unique = unique,
          include_in_dashboard,
          Keyqn,
          registry_raw_question_column,
          registry_question,
          detected_raw_column = raw_column,
          detected_source_column_family = source_column_family,
          detected_review_priority = review_priority,
          missing_best_raw_column_candidates = best_raw_column_candidates,
          missing_best_question_text_candidates = best_question_text_candidates,
          detected_nearest_registry_variables = nearest_registry_variables,
          detected_nearest_registry_questions = nearest_registry_questions,
          review_action = "If source owner confirms this is the same question, update the registry raw column/remarks. If it is new, add a registry row only if C360 should ingest it. If out of scope, document exclusion."
        )
      ]
    } else {
      data.table::data.table()
    }
  } else {
    data.table::data.table()
  }

  if (nrow(reconciliation_candidates) == 0) {
    reconciliation_candidates <- data.table::data.table(
      review_order = NA_integer_,
      candidate_rank = NA_integer_,
      reconciliation_priority = "none",
      match_basis = NA_character_,
      evidence_score = NA_integer_,
      code_distance = NA_integer_,
      survey = NA_character_,
      file_name = NA_character_,
      registry_variable = NA_character_,
      registry_unique = NA_character_,
      include_in_dashboard = NA_character_,
      Keyqn = NA_character_,
      registry_raw_question_column = NA_character_,
      registry_question = NA_character_,
      detected_raw_column = NA_character_,
      detected_source_column_family = NA_character_,
      detected_review_priority = NA_character_,
      missing_best_raw_column_candidates = NA_character_,
      missing_best_question_text_candidates = NA_character_,
      detected_nearest_registry_variables = NA_character_,
      detected_nearest_registry_questions = NA_character_,
      review_action = "No likely missing-registry/new-source reconciliation candidates were found."
    )
  }

  qa6_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_6_question_reconciliation_candidates.csv"))
  write_log(reconciliation_candidates, qa6_path)

  invalid_score_values <- if (nrow(invalid_dt) > 0 && "nonmissing_raw_values" %in% names(invalid_dt)) {
    invalid_action <- invalid_dt[nonmissing_raw_values > 0]
    if (nrow(invalid_action) > 0) {
      invalid_action <- add_missing_columns(invalid_action, c("scale_type", "raw_value_examples"))
      invalid_action[, .(
        issue_type = "nonmissing_raw_values_not_scored",
        invalid_score_rows = sum(invalid_score_rows, na.rm = TRUE),
        nonmissing_raw_values = sum(nonmissing_raw_values, na.rm = TRUE),
        raw_value_examples = collapse_pipe_examples(raw_value_examples, 10),
        review_action = "Confirm whether the raw labels belong to a supported scale, then update scale metadata or scoring rules."
      ), by = .(
        survey,
        file_name,
        variable,
        unique,
        question,
        raw_column = raw_question_column_clean,
        scale_type
      )]
    } else {
      data.table::data.table()
    }
  } else {
    data.table::data.table()
  }
  if (nrow(invalid_score_values) == 0 && stage %in% c("preflight", "pre_ingestion", "no_files_selected")) {
    invalid_score_values <- data.table::data.table(
      issue_type = paste0("not_checked_", stage),
      survey = NA_character_,
      file_name = NA_character_,
      variable = NA_character_,
      unique = NA_character_,
      question = NA_character_,
      raw_column = NA_character_,
      invalid_score_rows = NA_integer_,
      nonmissing_raw_values = NA_integer_,
      raw_value_examples = NA_character_,
      scale_type = NA_character_,
      review_action = "Invalid score values are checked only during full ingestion, not during this stage."
    )
  } else if (nrow(invalid_score_values) == 0) {
    invalid_score_values <- data.table::data.table(
      issue_type = "none",
      survey = NA_character_,
      file_name = NA_character_,
      variable = NA_character_,
      unique = NA_character_,
      question = NA_character_,
      raw_column = NA_character_,
      invalid_score_rows = 0L,
      nonmissing_raw_values = 0L,
      raw_value_examples = NA_character_,
      scale_type = NA_character_,
      review_action = "No nonmissing raw values failed score conversion."
    )
  }

  qa5_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_5_invalid_score_values.csv"))
  write_log(invalid_score_values, qa5_path)

  question_review_parts <- list()
  if (nrow(missing_registry_questions) > 0 &&
    any(missing_registry_questions$issue_type == "registry_question_missing_from_raw_file")) {
    question_review_parts[[length(question_review_parts) + 1]] <- missing_registry_questions[
      issue_type == "registry_question_missing_from_raw_file",
      .(
        review_priority = data.table::fcase(
          include_in_dashboard == "Include", "1_blocking_missing_dashboard_question",
          default = "2_missing_non_dashboard_registry_question"
        ),
        review_type = "missing_registry_question",
        survey,
        file_name,
        registry_variable = variable,
        registry_unique = unique,
        include_in_dashboard,
        Keyqn,
        registry_raw_question_column,
        registry_question,
        detected_raw_column = NA_character_,
        source_column_family = NA_character_,
        match_evidence = paste0(
          "raw candidates: ", best_raw_column_candidates,
          " || wording candidates: ", best_question_text_candidates
        ),
        nearest_registry_variables = NA_character_,
        nearest_registry_questions = NA_character_,
        raw_value_examples = NA_character_,
        scale_type = NA_character_,
        detail_log = qa3_path,
        review_action
      )
    ]
  }

  if (nrow(reconciliation_candidates) > 0 &&
    !all(reconciliation_candidates$reconciliation_priority == "none")) {
    question_review_parts[[length(question_review_parts) + 1]] <- reconciliation_candidates[
      reconciliation_priority != "none",
      .(
        review_priority = "3_possible_reword_or_code_change",
        review_type = "reconciliation_candidate",
        survey,
        file_name,
        registry_variable,
        registry_unique,
        include_in_dashboard,
        Keyqn,
        registry_raw_question_column,
        registry_question,
        detected_raw_column,
        source_column_family = detected_source_column_family,
        match_evidence = match_basis,
        nearest_registry_variables = detected_nearest_registry_variables,
        nearest_registry_questions = detected_nearest_registry_questions,
        raw_value_examples = NA_character_,
        scale_type = NA_character_,
        detail_log = qa6_path,
        review_action
      )
    ]
  }

  if (nrow(invalid_score_values) > 0 &&
    any(invalid_score_values$issue_type == "nonmissing_raw_values_not_scored")) {
    question_review_parts[[length(question_review_parts) + 1]] <- invalid_score_values[
      issue_type == "nonmissing_raw_values_not_scored",
      .(
        review_priority = "4_score_mapping_review",
        review_type = "invalid_score_values",
        survey,
        file_name,
        registry_variable = variable,
        registry_unique = unique,
        include_in_dashboard = NA_character_,
        Keyqn = NA_character_,
        registry_raw_question_column = raw_column,
        registry_question = question,
        detected_raw_column = raw_column,
        source_column_family = NA_character_,
        match_evidence = paste0("nonmissing raw values not scored: ", nonmissing_raw_values),
        nearest_registry_variables = NA_character_,
        nearest_registry_questions = NA_character_,
        raw_value_examples,
        scale_type,
        detail_log = qa5_path,
        review_action
      )
    ]
  }

  question_review <- if (length(question_review_parts) > 0) {
    out <- data.table::rbindlist(question_review_parts, fill = TRUE)
    data.table::setorder(out, review_priority, survey, file_name, registry_unique, detected_raw_column)
    out[, review_order := seq_len(.N)]
    out[, c("review_order", setdiff(names(out), "review_order")), with = FALSE]
  } else {
    data.table::data.table(
      review_order = NA_integer_,
      review_priority = "ok",
      review_type = "none",
      survey = NA_character_,
      file_name = NA_character_,
      registry_variable = NA_character_,
      registry_unique = NA_character_,
      include_in_dashboard = NA_character_,
      Keyqn = NA_character_,
      registry_raw_question_column = NA_character_,
      registry_question = NA_character_,
      detected_raw_column = NA_character_,
      source_column_family = NA_character_,
      match_evidence = NA_character_,
      nearest_registry_variables = NA_character_,
      nearest_registry_questions = NA_character_,
      raw_value_examples = NA_character_,
      scale_type = NA_character_,
      detail_log = NA_character_,
      review_action = "No question-level review items were found for this stage."
    )
  }

  qa_question_review_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_3_question_review.csv"))
  write_log(question_review, qa_question_review_path)
  qa_question_review_qc_path <- file.path(qc_dir, "QA_3_question_review.csv")
  write_log(question_review, qa_question_review_qc_path)

  triage_row <- function(priority, qc_file, issue_dt, severity, what_it_means, analyst_action, count_col = NULL) {
    issue_dt <- data.table::as.data.table(issue_dt)
    not_checked <- nrow(issue_dt) > 0 &&
      "issue_type" %in% names(issue_dt) &&
      all(grepl("^not_checked", as.character(issue_dt$issue_type)))
    has_issues <- nrow(issue_dt) > 0 &&
      "issue_type" %in% names(issue_dt) &&
      !all(as.character(issue_dt$issue_type) %in% c("none")) &&
      !not_checked
    issue_count <- if (!has_issues) {
      0L
    } else if (!is.null(count_col) && count_col %in% names(issue_dt)) {
      sum(safe_as_numeric(issue_dt[[count_col]]), na.rm = TRUE)
    } else {
      nrow(issue_dt)
    }

    data.table::data.table(
      priority = priority,
      qc_file = qc_file,
      severity = if (has_issues) severity else if (not_checked) "not_checked" else "ok",
      issue_count = issue_count,
      what_it_means = what_it_means,
      analyst_action = if (has_issues) analyst_action else if (not_checked) issue_dt$review_action[[1]] else "No action needed."
    )
  }

  triage <- data.table::rbindlist(
    list(
      triage_row(
        1,
        "QA_3_question_review.csv",
        missing_dashboard_registry_questions,
        "high",
        "Dashboard-included registry questions expected by C360 were not found in the selected raw files.",
        "Filter review_type = missing_registry_question and include_in_dashboard = Include. Resolve before ingestion/export."
      ),
      triage_row(
        2,
        "QA_3_question_review.csv",
        missing_registry_questions,
        "review",
        "All registry questions expected by C360 but not found in the selected raw files, including non-dashboard questions.",
        "Filter review_type = missing_registry_question. Non-dashboard missing questions remain visible but do not stop ingestion by default."
      ),
      triage_row(
        3,
        "QA_3_question_review.csv",
        reconciliation_candidates,
        "high",
        "Best likely links between missing registry questions and detected source questions.",
        "Filter review_type = reconciliation_candidate. Use these top candidate pairs only after confirming the source column is the same question."
      ),
      triage_row(
        4,
        "QA_3_question_review.csv",
        invalid_score_values,
        "medium",
        "Mapped registry questions have non-missing raw values that cannot be scored.",
        "Filter review_type = invalid_score_values. Review raw_value_examples and update scale metadata/scoring only if valid.",
        "nonmissing_raw_values"
      ),
      triage_row(
        5,
        basename(qa4_path),
        new_questions_detected,
        "review",
        "The raw source contains question-like columns that are not in the registry and were not ingested.",
        "Use as a robustness list after missing registry questions are resolved; this list does not block ingestion by default."
      )
    ),
    fill = TRUE
  )
  qa0_path <- file.path(LOG_DIR, paste0(Sys.Date(), "_QA_0_analyst_triage.csv"))
  write_log(triage, qa0_path)
  qa0_qc_path <- file.path(qc_dir, "QA_0_analyst_triage.csv")
  write_log(triage, qa0_qc_path)

  message("Analyst triage saved to: ", qa0_path)
  message("Actionable QA summary saved to: ", qa1_path)
  message("Survey/file QA summary saved to: ", qa2_path)
  message("Question review QC saved to: ", qa_question_review_path)
  message("Detailed missing/new/reconciliation/score logs saved under: ", LOG_DIR)
  message("Clean analyst QC folder saved to: ", qc_dir)

  invisible(list(
    run_summary = summary,
    triage = triage,
    survey_file_summary = survey_file_summary,
    missing_registry_questions = missing_registry_questions,
    missing_dashboard_registry_questions = missing_dashboard_registry_questions,
    new_questions_detected = new_questions_detected,
    reconciliation_candidates = reconciliation_candidates,
    invalid_score_values = invalid_score_values
  ))
}

qc_issue_count <- function(dt) {
  dt <- data.table::as.data.table(dt)
  if (nrow(dt) == 0 || !("issue_type" %in% names(dt))) {
    return(0L)
  }
  sum(!as.character(dt$issue_type) %in% c("none") & !grepl("^not_checked", as.character(dt$issue_type)), na.rm = TRUE)
}

pipeline_scripts <- c("01_helpers.R", "02_registry_and_file_discovery.R")

for (script in pipeline_scripts) {
  run_step(script, file.path(PROJECT_DIR, script))
}

if (isTRUE(RUN_PREFLIGHT_ONLY)) {
  run_end_time <- Sys.time()
  run_duration <- as.numeric(difftime(run_end_time, run_start_time, units = "mins"))
  analyst_summary <- write_analyst_run_summary("preflight")
  actionable_qa <- write_actionable_qa_outputs("preflight")
  run_summary <- data.table::data.table(
    run_start_time = as.character(run_start_time),
    run_end_time = as.character(run_end_time),
    run_duration_minutes = run_duration,
    run_mode = RUN_MODE,
    preflight_only = TRUE,
    surveys_to_run = paste(SURVEYS_TO_RUN, collapse = ", "),
    registry_rows_loaded = if (exists("registry")) nrow(registry) else NA_integer_,
    files_discovered = if (exists("file_index")) nrow(file_index) else NA_integer_,
    files_selected_for_processing = if (exists("files_to_process")) nrow(files_to_process) else NA_integer_,
    question_drift_review_rows = if (exists("question_drift_review")) nrow(question_drift_review) else NA_integer_,
    logs_dir = LOG_DIR
  )
  write_log(run_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_06_run_summary.csv")))

  message("")
  message("============================================================")
  message("C360 preflight completed. No ingestion/export files were overwritten.")
  message("Duration: ", round(run_duration, 2), " minutes")
  message("Registry rows loaded: ", run_summary$registry_rows_loaded)
  message("Files selected for processing: ", run_summary$files_selected_for_processing)
  message("Question drift review rows: ", run_summary$question_drift_review_rows)
  message("Logs written to: ", LOG_DIR)
  message("============================================================")
} else if (exists("files_to_process") && nrow(files_to_process) == 0) {
  run_end_time <- Sys.time()
  run_duration <- as.numeric(difftime(run_end_time, run_start_time, units = "mins"))
  analyst_summary <- write_analyst_run_summary("no_files_selected")
  actionable_qa <- write_actionable_qa_outputs("no_files_selected")
  run_summary <- data.table::data.table(
    run_start_time = as.character(run_start_time),
    run_end_time = as.character(run_end_time),
    run_duration_minutes = run_duration,
    run_mode = RUN_MODE,
    preflight_only = FALSE,
    no_files_selected = TRUE,
    surveys_to_run = paste(SURVEYS_TO_RUN, collapse = ", "),
    registry_rows_loaded = if (exists("registry")) nrow(registry) else NA_integer_,
    files_discovered = if (exists("file_index")) nrow(file_index) else NA_integer_,
    files_selected_for_processing = 0L,
    question_drift_review_rows = if (exists("question_drift_review")) nrow(question_drift_review) else NA_integer_,
    outputs_overwritten = FALSE,
    logs_dir = LOG_DIR
  )
  write_log(run_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_06_run_summary.csv")))

  message("")
  message("============================================================")
  message("C360 run stopped after discovery because no files were selected.")
  message("No ingestion/export files were overwritten.")
  message("Duration: ", round(run_duration, 2), " minutes")
  message("Registry rows loaded: ", run_summary$registry_rows_loaded)
  message("Files discovered: ", run_summary$files_discovered)
  message("Files selected for processing: 0")
  message("Logs written to: ", LOG_DIR)
  message("============================================================")
} else {
  if (isTRUE(RUN_PRE_INGESTION_QC_GATE)) {
    pre_ingestion_qa <- write_actionable_qa_outputs("pre_ingestion")
    missing_q_count <- qc_issue_count(pre_ingestion_qa$missing_registry_questions)
    missing_dashboard_q_count <- qc_issue_count(pre_ingestion_qa$missing_dashboard_registry_questions)
    new_q_count <- qc_issue_count(pre_ingestion_qa$new_questions_detected)

    if (
      missing_dashboard_q_count > 0 &&
        exists("STOP_ON_MISSING_DASHBOARD_REGISTRY_QUESTIONS") &&
        isTRUE(STOP_ON_MISSING_DASHBOARD_REGISTRY_QUESTIONS) &&
        !isTRUE(FORCE_RUN_WITH_MISSING_DASHBOARD_QUESTIONS)
    ) {
      stop(
        "Pre-ingestion QC gate stopped the run: ",
        missing_dashboard_q_count,
        " dashboard-included registry question(s) expected by C360 were missing from selected raw files. ",
        "Overall missing registry question(s): ",
        missing_q_count,
        ". ",
        "Review ",
        file.path(QC_RUN_DIR, "QA_0_analyst_triage.csv"),
        " and ",
        file.path(QC_RUN_DIR, "QA_3_question_review.csv"),
        " before running ingestion. To override after documented review, set ",
        "options(C360_FORCE_RUN_WITH_MISSING_DASHBOARD_QUESTIONS = TRUE) for that run, or ",
        "options(C360_STOP_ON_MISSING_DASHBOARD_REGISTRY_QUESTIONS = FALSE)."
      )
    }

    if (new_q_count > 0 && isTRUE(STOP_ON_NEW_QUESTIONS_DETECTED)) {
      stop(
        "Pre-ingestion QC gate stopped the run: ",
        new_q_count,
        " source question-like column(s) were not in the registry. Review ",
        file.path(QC_RUN_DIR, "QA_0_analyst_triage.csv"),
        " and ",
        file.path(LOG_DIR, paste0(Sys.Date(), "_QA_4_new_questions_detected.csv")),
        " before running ingestion. To override after documented review, set ",
        "options(C360_STOP_ON_NEW_QUESTIONS_DETECTED = FALSE)."
      )
    }

    if (missing_q_count > 0 || new_q_count > 0) {
      message(
        "Pre-ingestion QC gate completed with review items: missing registry questions=",
        missing_q_count,
        ", missing dashboard registry questions=",
        missing_dashboard_q_count,
        ", new detected questions=",
        new_q_count,
        ". Continuing because blocking options allow it."
      )
    } else {
      message("Pre-ingestion QC gate passed with no blocking question issues.")
    }
  }

  for (script in c("03_ingest_response_level.R", "04_periodise_aggregate_flags.R", "05_bolt_on_export.R")) {
    run_step(script, file.path(PROJECT_DIR, script))
  }

  run_end_time <- Sys.time()
  run_duration <- as.numeric(difftime(run_end_time, run_start_time, units = "mins"))

  run_summary <- data.table::data.table(
    run_start_time = as.character(run_start_time),
    run_end_time = as.character(run_end_time),
    run_duration_minutes = run_duration,
    run_mode = RUN_MODE,
    target_update_quarter = if (exists("TARGET_UPDATE_QUARTER") && !is.na(TARGET_UPDATE_QUARTER)) TARGET_UPDATE_QUARTER else "",
    run_output_dir = if (exists("RUN_OUTPUT_DIR")) RUN_OUTPUT_DIR else "",
    surveys_to_run = paste(SURVEYS_TO_RUN, collapse = ", "),
    registry_rows_loaded = if (exists("registry")) nrow(registry) else NA_integer_,
    registry_questions_matched = if (exists("ingest_question_log") && nrow(ingest_question_log) > 0) {
      sum(ingest_question_log$available_in_file, na.rm = TRUE)
    } else {
      NA_integer_
    },
    full_output_rows = if (exists("full_output")) nrow(full_output) else NA_integer_,
    dashboard_output_rows = if (exists("dashboard_output")) nrow(dashboard_output) else NA_integer_,
    key_question_output_rows = if (exists("key_question_output")) nrow(key_question_output) else NA_integer_,
    internal_rows = if (exists("internal_tableau_wide")) nrow(internal_tableau_wide) else NA_integer_,
    internal_trimmed_rows = if (exists("internal_tableau_trimmed")) nrow(internal_tableau_trimmed) else NA_integer_,
    external_trimmed_rows = if (exists("external_tableau_trimmed")) nrow(external_tableau_trimmed) else NA_integer_,
    files_processed = if (exists("file_ingest_log") && nrow(file_ingest_log) > 0) {
      sum(file_ingest_log$status == "processed", na.rm = TRUE)
    } else {
      0L
    },
    logs_dir = LOG_DIR
  )

  write_log(run_summary, file.path(LOG_DIR, paste0(Sys.Date(), "_06_run_summary.csv")))
  analyst_summary <- write_analyst_run_summary("complete")
  actionable_qa <- write_actionable_qa_outputs("complete")

  message("")
  message("============================================================")
  message("C360 registry-driven pipeline completed")
  message("End time: ", format(run_end_time, "%Y-%m-%d %H:%M:%S"))
  message("Duration: ", round(run_duration, 2), " minutes")
  message("Registry rows loaded: ", run_summary$registry_rows_loaded)
  message("Registry questions matched in processed files: ", run_summary$registry_questions_matched)
  message("Full output rows: ", run_summary$full_output_rows)
  message("Dashboard output rows: ", run_summary$dashboard_output_rows)
  message("Internal rows: ", run_summary$internal_rows)
  message("Internal trimmed rows: ", run_summary$internal_trimmed_rows)
  message("External trimmed rows: ", run_summary$external_trimmed_rows)
  message("Key-question output rows: ", run_summary$key_question_output_rows)
  message("Run output folder: ", RUN_OUTPUT_DIR)
  message("Logs written to: ", LOG_DIR)
  message("PGP cap rule: records dated after ", PGP_WEIGHT_CAP_AFTER_DATE, " are forced to max weight ", PGP_WEIGHT_CAP_VALUE)
  message("============================================================")
}
