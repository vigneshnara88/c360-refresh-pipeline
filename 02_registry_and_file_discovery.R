# ============================================================
# 02_registry_and_file_discovery.R
# Load the question registry, discover raw files, and write QA logs
# ============================================================
#
# Corrected logic:
# - Load all valid rows from the registry.
# - Do not filter by Key Question?, Include in Dashboard?, Sensitivity, or
#   include_flag during ingestion.
# - Use Include in Dashboard? only later in 05_bolt_on_export.R.

if (!exists("REGISTRY_PATH")) {
  source("00_config.R")
}
if (!exists("clean_col")) {
  source(file.path(PROJECT_DIR, "01_helpers.R"))
}

message("============================================================")
message("02_registry_and_file_discovery.R")
message("Registry path: ", REGISTRY_PATH)
message("Selected surveys: ", paste(SURVEYS_TO_RUN, collapse = ", "))
message("============================================================")

if (!file.exists(REGISTRY_PATH)) {
  stop("Question registry does not exist: ", REGISTRY_PATH)
}

coalesce_registry_cols <- function(raw, candidates, default = NA_character_) {
  cols <- intersect(clean_col(candidates), names(raw))
  out <- rep(default, nrow(raw))
  if (length(cols) == 0) {
    return(out)
  }
  for (cc in cols) {
    val <- as.character(raw[[cc]])
    take <- is_blankish(out) & !is_blankish(val)
    out[take] <- val[take]
  }
  out
}

resolve_registry_sheet <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!(ext %in% c("xlsx", "xls"))) {
    return(NA)
  }

  sheets <- readxl::excel_sheets(path)
  fallbacks <- REGISTRY_SHEET_FALLBACKS %||% c(REGISTRY_SHEET, "Master_Registry", "Sheet2", "Sheet1")
  found <- fallbacks[fallbacks %in% sheets]

  if (length(found) > 0) {
    return(found[[1]])
  }

  sheets[[1]]
}

read_registry_raw <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    sheet <- resolve_registry_sheet(path)
    raw <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
    raw <- as.data.frame(raw)
    attr(raw, "registry_sheet") <- sheet
    return(raw)
  }

  if (ext %in% c("csv", "txt")) {
    raw <- data.table::fread(path, stringsAsFactors = FALSE)
    raw <- as.data.frame(raw)
    attr(raw, "registry_sheet") <- NA_character_
    return(raw)
  }

  if (ext == "rds") {
    raw <- readRDS(path)
    raw <- as.data.frame(raw)
    attr(raw, "registry_sheet") <- NA_character_
    return(raw)
  }

  stop("Unsupported registry file extension: ", ext)
}

load_registry <- function(path) {
  reg_raw <- read_registry_raw(path)
  registry_sheet <- attr(reg_raw, "registry_sheet")

  if (nrow(reg_raw) == 0) {
    stop("Question registry has zero rows.")
  }

  names(reg_raw) <- make.unique(clean_col(names(reg_raw)), sep = "_dup")
  reg_raw <- data.table::as.data.table(reg_raw)

  fully_blank <- apply(reg_raw, 1, function(z) all(is_blankish(z)))
  reg_raw <- reg_raw[!fully_blank]

  dataset <- coalesce_registry_cols(reg_raw, c("dataset", "survey"))
  raw_question_column <- coalesce_registry_cols(reg_raw, c("raw_question_column", "variable", "question_code"))
  question <- coalesce_registry_cols(reg_raw, c("question", "question_text", "label"))
  unique_code <- coalesce_registry_cols(reg_raw, c("unique_code", "unique"))

  tier1 <- coalesce_registry_cols(reg_raw, c("tier1", "tier_1"))
  tier2 <- coalesce_registry_cols(reg_raw, c("tier2", "tier_2"))
  tier3 <- coalesce_registry_cols(reg_raw, c("tier3", "tier_3"))

  key_question <- coalesce_registry_cols(
    reg_raw,
    c("key_question", "key_question_flag", "keyqn", "key_question_")
  )
  include_dashboard <- coalesce_registry_cols(
    reg_raw,
    c("include_in_dashboard", "dashboard_include", "include_dashboard", "include_flag", "sensitivity")
  )
  dataset_label <- coalesce_registry_cols(reg_raw, c("dataset_label", "survey_label", "dataset_name"))
  update_frequency <- coalesce_registry_cols(reg_raw, c("update_frequency", "refresh_frequency", "frequency"))
  normal_refresh_mode <- coalesce_registry_cols(reg_raw, c("normal_refresh_mode", "refresh_mode"))
  source_folder <- coalesce_registry_cols(reg_raw, c("source_folder", "folder", "data_folder"))
  direction_raw <- coalesce_registry_cols(
    reg_raw,
    c("direction_coding", "direction", "Direction Coding")
  )
  reverse_required <- coalesce_registry_cols(reg_raw, c("reverse_code_required", "reverse_required"))

  remarks <- coalesce_registry_cols(
    reg_raw,
    c("remarks", "notes", "additional_remarks_feb_2025", "change_notes")
  )
  processed_by_pipeline <- coalesce_registry_cols(
    reg_raw,
    c("processed_by_pipeline", "processed_by_pipeline_", "processed")
  )
  scale_type <- coalesce_registry_cols(reg_raw, c("scale_type", "scale"))
  valid_from <- coalesce_registry_cols(reg_raw, c("valid_from", "from_date"))
  valid_to <- coalesce_registry_cols(reg_raw, c("valid_to", "to_date"))

  invalid_required <- is_blankish(dataset) | is_blankish(raw_question_column) | is_blankish(question)

  invalid_rows <- data.table::data.table(
    source_row = seq_len(nrow(reg_raw)),
    dataset = dataset,
    raw_question_column = raw_question_column,
    question = question,
    issue = "missing required dataset, variable/raw question column, or question"
  )[invalid_required]

  direction_clean <- stringr::str_to_lower(stringr::str_squish(as.character(direction_raw)))
  reverse_clean <- stringr::str_to_lower(stringr::str_squish(as.character(reverse_required)))
  direction <- dplyr::case_when(
    direction_clean == "reverse" ~ "Reverse",
    direction_clean == "standard" ~ "Standard",
    direction_clean == "check" ~ "Check",
    reverse_clean %in% c("yes", "y", "true", "1") ~ "Reverse",
    is_blankish(direction_clean) ~ "Standard",
    TRUE ~ "Check"
  )

  out <- data.table::data.table(
    source_row = seq_len(nrow(reg_raw)),
    dataset = stringr::str_to_lower(stringr::str_squish(as.character(dataset))),
    survey = stringr::str_to_lower(stringr::str_squish(as.character(dataset))),
    dataset_label = stringr::str_squish(as.character(dataset_label)),
    update_frequency = stringr::str_squish(as.character(update_frequency)),
    normal_refresh_mode = stringr::str_squish(as.character(normal_refresh_mode)),
    source_folder = stringr::str_squish(as.character(source_folder)),
    raw_question_column = stringr::str_squish(as.character(raw_question_column)),
    raw_question_column_clean = clean_col(raw_question_column),
    variable = clean_col(raw_question_column),
    unique_code = stringr::str_squish(as.character(unique_code)),
    question = stringr::str_squish(as.character(question)),
    Tier1 = stringr::str_squish(as.character(tier1)),
    Tier2 = stringr::str_squish(as.character(tier2)),
    Tier3 = stringr::str_squish(as.character(tier3)),
    key_question_flag = normalise_key_flag(key_question),
    Keyqn = normalise_key_flag(key_question),
    include_in_dashboard = normalise_dashboard_flag(include_dashboard),
    direction = direction,
    `Direction Coding` = direction,
    reverse_code_required = ifelse(direction == "Reverse", "Yes", "No"),
    Remarks = stringr::str_squish(as.character(remarks)),
    Notes = stringr::str_squish(as.character(remarks)),
    processed_by_pipeline = stringr::str_squish(as.character(processed_by_pipeline)),
    scale_type = stringr::str_to_lower(stringr::str_squish(as.character(scale_type))),
    valid_from = as.Date(parse_date_flexible(valid_from)),
    valid_to = as.Date(parse_date_flexible(valid_to)),
    registry_sheet = as.character(registry_sheet %||% NA_character_)
  )

  out[is_blankish(unique_code), unique_code := paste(dataset, variable, sep = ".")]
  out[, unique := unique_code]
  out[is_blankish(dataset_label), dataset_label := dataset]
  out[is_blankish(update_frequency), update_frequency := dplyr::case_when(
    dataset %in% names(SURVEY_REFRESH_CADENCE)[SURVEY_REFRESH_CADENCE == "quarterly"] ~ "Quarterly",
    dataset %in% names(SURVEY_REFRESH_CADENCE)[SURVEY_REFRESH_CADENCE == "yearly"] ~ "Yearly",
    TRUE ~ "Legacy"
  )]
  out[is_blankish(normal_refresh_mode), normal_refresh_mode := dplyr::case_when(
    update_frequency == "Quarterly" ~ "Quarterly run",
    update_frequency == "Yearly" ~ "Annual run",
    TRUE ~ "Custom/check"
  )]
  out[is_blankish(source_folder), source_folder := file.path("data", dataset)]
  out[is_blankish(scale_type), scale_type := DEFAULT_SCALE_TYPE]
  out[is_blankish(Remarks), Remarks := NA_character_]
  out[is_blankish(Notes), Notes := NA_character_]

  out <- out[!invalid_required]

  attr(out, "invalid_rows") <- invalid_rows
  attr(out, "registry_sheet") <- registry_sheet
  out[]
}

build_registry_qa <- function(registry) {
  invalid_rows <- attr(registry, "invalid_rows") %||% data.table::data.table()

  qa <- list()

  if (nrow(invalid_rows) > 0) {
    qa[[length(qa) + 1]] <- invalid_rows[, .(
      issue = issue,
      severity = "error",
      dataset = dataset,
      variable = raw_question_column,
      unique = NA_character_,
      question = question,
      detail = "Dropped from registry because the row cannot be matched safely."
    )]
  }

  missing_unique <- registry[is_blankish(unique_code)]
  if (nrow(missing_unique) > 0) {
    qa[[length(qa) + 1]] <- missing_unique[, .(
      issue = "missing unique_code",
      severity = "warning",
      dataset, variable, unique, question,
      detail = "unique defaults to dataset.variable"
    )]
  }

  missing_tiers <- registry[is_blankish(Tier1) | is_blankish(Tier2) | is_blankish(Tier3)]
  if (nrow(missing_tiers) > 0) {
    qa[[length(qa) + 1]] <- missing_tiers[, .(
      issue = "missing Tier1/Tier2/Tier3",
      severity = "warning",
      dataset, variable, unique, question,
      detail = "Tier fields are preferred metadata and should be reviewed."
    )]
  }

  direction_check <- registry[direction == "Check"]
  if (nrow(direction_check) > 0) {
    qa[[length(qa) + 1]] <- direction_check[, .(
      issue = "direction coding check",
      severity = "warning",
      dataset, variable, unique, question,
      detail = "Direction Coding is Check or unrecognised. Scoring was not reversed."
    )]
  }

  unsupported_scale <- registry[!(scale_type %in% SUPPORTED_SCALE_TYPES)]
  if (nrow(unsupported_scale) > 0) {
    qa[[length(qa) + 1]] <- unsupported_scale[, .(
      issue = "unsupported scale_type",
      severity = "warning",
      dataset, variable, unique, question,
      detail = paste0("scale_type=", scale_type, "; standardise_score_col will use auto_likert fallback.")
    )]
  }

  duplicate_unique <- registry[duplicated(unique) | duplicated(unique, fromLast = TRUE)]
  if (nrow(duplicate_unique) > 0) {
    qa[[length(qa) + 1]] <- duplicate_unique[, .(
      issue = "duplicate unique",
      severity = "warning",
      dataset, variable, unique, question,
      detail = "Duplicate unique codes can duplicate output rows."
    )]
  }

  duplicate_variable <- registry[
    duplicated(paste(dataset, variable)) | duplicated(paste(dataset, variable), fromLast = TRUE)
  ]
  if (nrow(duplicate_variable) > 0) {
    qa[[length(qa) + 1]] <- duplicate_variable[, .(
      issue = "duplicate dataset-variable",
      severity = "warning",
      dataset, variable, unique, question,
      detail = "Multiple registry rows point to the same raw variable."
    )]
  }

  if (length(qa) == 0) {
    return(data.table::data.table())
  }

  data.table::rbindlist(qa, fill = TRUE)
}

registry <- load_registry(REGISTRY_PATH)
registry_qa <- build_registry_qa(registry)

registry_summary <- registry[
  ,
  .(
    registry_questions = .N,
    key_questions = sum(Keyqn == "Yes", na.rm = TRUE),
    dashboard_included = sum(include_in_dashboard == "Include", na.rm = TRUE),
    key_but_dashboard_excluded = sum(Keyqn == "Yes" & include_in_dashboard != "Include", na.rm = TRUE),
    direction_check = sum(direction == "Check", na.rm = TRUE)
  ),
  by = dataset
][order(dataset)]

write_log(
  registry_summary,
  file.path(LOG_DIR, paste0(Sys.Date(), "_02_registry_summary.csv"))
)
write_log(
  registry_qa,
  file.path(LOG_DIR, paste0(Sys.Date(), "_02_registry_qa.csv"))
)
write_log(
  registry[direction == "Check"],
  file.path(LOG_DIR, paste0(Sys.Date(), "_02_direction_coding_check_rows.csv"))
)

message("Registry sheet read: ", attr(registry, "registry_sheet") %||% "n/a")
message("Registry rows loaded for ingestion: ", nrow(registry))
message("Dashboard-included registry rows: ", sum(registry$include_in_dashboard == "Include", na.rm = TRUE))
message("Key-question registry rows: ", sum(registry$Keyqn == "Yes", na.rm = TRUE))

# ============================================================
# Raw file discovery and manifest selection
# ============================================================

list_source_files <- function(survey) {
  survey <- stringr::str_to_lower(as.character(survey))
  survey_dir <- file.path(DATA_DIR, survey)

  if (!dir.exists(survey_dir)) {
    warning("Survey folder does not exist: ", survey_dir)
    return(character(0))
  }

  files <- list.files(
    survey_dir,
    full.names = TRUE,
    recursive = isTRUE(RECURSIVE_FILE_DISCOVERY),
    include.dirs = FALSE
  )

  if (length(files) == 0) {
    return(character(0))
  }

  ext_ok <- tolower(tools::file_ext(files)) %in% tolower(VALID_EXTENSIONS)
  files <- files[ext_ok]

  if (length(files) == 0) {
    return(character(0))
  }

  exclude_regex <- paste(EXCLUDE_FILE_PATTERNS, collapse = "|")
  files <- files[!stringr::str_detect(stringr::str_to_lower(basename(files)), exclude_regex)]

  mode <- if (survey %in% names(source_file_mode)) source_file_mode[[survey]] else "period_files"
  if (identical(mode, "latest_file_only") && length(files) > 1) {
    info <- file.info(files)
    files <- files[which.max(info$mtime)]
  }

  normalizePath(files, winslash = "/", mustWork = FALSE)
}

build_file_index <- function(surveys) {
  out <- list()
  for (survey in surveys) {
    files <- list_source_files(survey)
    if (length(files) == 0) {
      next
    }
    info <- file.info(files)
    out[[length(out) + 1]] <- data.table::data.table(
      survey = survey,
      dataset = survey,
      file_path = files,
      file_name = basename(files),
      file_ext = tolower(tools::file_ext(files)),
      file_mtime = as.character(info$mtime),
      file_size = as.numeric(info$size)
    )
  }

  if (length(out) == 0) {
    return(data.table::data.table(
      survey = character(),
      dataset = character(),
      file_path = character(),
      file_name = character(),
      file_ext = character(),
      file_mtime = character(),
      file_size = numeric()
    ))
  }

  data.table::rbindlist(out, fill = TRUE)
}

load_manifest <- function(path) {
  if (!file.exists(path)) {
    return(data.table::data.table())
  }
  data.table::as.data.table(readRDS(path))
}

select_files_to_process <- function(file_index, manifest) {
  if (nrow(file_index) == 0) {
    file_index[, reason := character()]
    return(file_index)
  }

  selected <- data.table::copy(file_index)

  if (isTRUE(FULL_REBUILD) || nrow(manifest) == 0) {
    selected[, reason := ifelse(isTRUE(FULL_REBUILD), "full_rebuild", "not_in_manifest")]
    return(selected[])
  }

  manifest <- add_missing_columns(manifest, c("file_path", "file_mtime", "file_size"))
  manifest[, manifest_key := paste(file_path, file_mtime, file_size, sep = "||")]
  selected[, manifest_key := paste(file_path, file_mtime, file_size, sep = "||")]
  selected <- selected[!(manifest_key %in% manifest$manifest_key)]
  selected[, reason := "new_or_modified_file"]
  selected[, manifest_key := NULL]
  selected[]
}

file_index <- build_file_index(SURVEYS_TO_RUN)
manifest <- load_manifest(MANIFEST_PATH)
files_to_process <- select_files_to_process(file_index, manifest)

write_log(file_index, file.path(LOG_DIR, paste0(Sys.Date(), "_02_file_index.csv")))
write_log(files_to_process, file.path(LOG_DIR, paste0(Sys.Date(), "_02_files_to_process.csv")))

message("Files discovered: ", nrow(file_index))
message("Files selected for processing: ", nrow(files_to_process))

# ============================================================
# Question drift review
# ============================================================

raw_question_exclude_regex <- function(survey_id = NA_character_) {
  patterns <- if (exists("RAW_QUESTION_EXCLUDE_PATTERNS")) {
    RAW_QUESTION_EXCLUDE_PATTERNS
  } else {
    c(
      "^\\.",
      "^@",
      "(^|_)unique(_|$)",
      "(^|_)(id|uuid|guid|caseid|serial|record|hhid)(_|$)",
      "hhid$",
      "respondent",
      "response_id",
      "(^|_)(date|time|timestamp|start|end|duration)(_|$)",
      "(^|_)(day|month|year|quarter|week|period)(_|$)",
      "(dat[a-z0-9_]*|date|txt[a-z0-9_]*|text|num[a-z0-9_]*|oth[0-9]*|other|recode)$",
      "(^|_)(weight|wt|sample|quota|wave|batch|source|survey|dataset|file|folder)(_|$)",
      "(^|_)(gender|sex|age|race|educ|income|housing|marital|postal|postcode|address)(_|$)",
      "(^|_)(pmet|overall|vulnerable)(_|$)",
      "(^|_)(specify|comment|remarks|remark|open_end|openend)(_|$)",
      "(^|_)(derived|calc|flag)(_|$)",
      "^ps[0-9]+[a-z]?$"
    )
  }

  survey_key <- normalise_survey_id(survey_id %||% NA_character_)
  if (
    exists("RAW_QUESTION_EXCLUDE_PATTERNS_BY_SURVEY") &&
      length(survey_key) > 0 &&
      !is.na(survey_key) &&
      survey_key %in% names(RAW_QUESTION_EXCLUDE_PATTERNS_BY_SURVEY)
  ) {
    patterns <- c(patterns, RAW_QUESTION_EXCLUDE_PATTERNS_BY_SURVEY[[survey_key]])
  }

  paste(patterns, collapse = "|")
}

filter_raw_question_candidates <- function(candidates, registry_vars = character(), survey_id = NA_character_) {
  candidates <- data.table::as.data.table(candidates)
  if (nrow(candidates) == 0) {
    return(candidates)
  }

  candidates <- add_missing_columns(candidates, c("raw_column_clean", "raw_column_original"))
  registry_vars <- unique(as.character(registry_vars %||% character()))
  alias_sources <- if (exists("raw_question_alias_source_cols")) {
    raw_question_alias_source_cols(survey_id)
  } else {
    character()
  }
  is_registry_mapped <- candidates$raw_column_clean %in% c(registry_vars, alias_sources)
  exclude_regex <- raw_question_exclude_regex(survey_id)

  clean_lower <- stringr::str_to_lower(as.character(candidates$raw_column_clean))
  original_lower <- stringr::str_to_lower(as.character(candidates$raw_column_original))
  exclude <- stringr::str_detect(clean_lower, exclude_regex) |
    stringr::str_detect(original_lower, exclude_regex)
  exclude[is.na(exclude)] <- FALSE

  unique(candidates[is_registry_mapped | !exclude])
}

extract_raw_question_candidates <- function(raw, name_map = NULL, registry_vars = character(), survey_id = NA_character_) {
  raw_cols <- names(raw)
  candidates <- data.table::data.table(
    raw_column_clean = raw_cols,
    raw_column_original = raw_cols
  )

  if (!is.null(name_map) && nrow(name_map) > 0) {
    name_map <- data.table::as.data.table(name_map)
    candidates <- merge(
      candidates[, .(raw_column_clean)],
      name_map[, .(
        raw_column_clean = clean_name,
        raw_column_original = original_name
      )],
      by = "raw_column_clean",
      all.x = TRUE,
      sort = FALSE
    )
    candidates[is_blankish(raw_column_original), raw_column_original := raw_column_clean]
  }

  candidates <- filter_raw_question_candidates(candidates, registry_vars, survey_id)
  unique(c(candidates$raw_column_clean, candidates$raw_column_original))
}

extract_raw_question_candidates_detailed <- function(raw, name_map = NULL, registry_vars = character(), survey_id = NA_character_) {
  raw_cols <- names(raw)
  candidates <- data.table::data.table(
    raw_column_clean = raw_cols,
    raw_column_original = raw_cols
  )

  if (!is.null(name_map) && nrow(name_map) > 0) {
    name_map <- data.table::as.data.table(name_map)
    candidates <- merge(
      candidates[, .(raw_column_clean)],
      name_map[, .(
        raw_column_clean = clean_name,
        raw_column_original = original_name
      )],
      by = "raw_column_clean",
      all.x = TRUE,
      sort = FALSE
    )
    candidates[is_blankish(raw_column_original), raw_column_original := raw_column_clean]
  }

  filter_raw_question_candidates(candidates, registry_vars, survey_id)
}

format_registry_matches <- function(match_values, reg_s, value_col, display_col) {
  if (length(match_values) == 0 || nrow(reg_s) == 0) {
    return("")
  }

  reg_s <- data.table::as.data.table(reg_s)
  out <- character()
  for (mv in match_values) {
    hit <- reg_s[as.character(get(value_col)) == as.character(mv)]
    if (nrow(hit) == 0) {
      next
    }
    out <- c(out, paste0(hit[[display_col]][[1]], " [", hit$unique[[1]], "]"))
  }
  paste(unique(out), collapse = " | ")
}

empty_source_raw_question_match_log <- function() {
  data.table::data.table(
    survey = character(),
    file_name = character(),
    file_path = character(),
    raw_column = character(),
    raw_column_clean = character(),
    mapped_to_registry = logical(),
    nearest_registry_variables = character(),
    nearest_registry_variable_distances = character(),
    nearest_registry_questions = character(),
    nearest_registry_question_distances = character(),
    review_action = character()
  )
}

build_unmatched_raw_question_review <- function(raw, name_map, reg_s, file_row, survey_id) {
  source_match <- build_source_raw_question_match_log(raw, name_map, reg_s, file_row, survey_id)
  source_match[mapped_to_registry == FALSE]
}

build_source_raw_question_match_log <- function(raw, name_map, reg_s, file_row, survey_id) {
  if (nrow(reg_s) == 0) {
    return(empty_source_raw_question_match_log())
  }

  registry_vars <- unique(reg_s$raw_question_column_clean)
  candidates <- extract_raw_question_candidates_detailed(raw, name_map, registry_vars, survey_id)
  if (nrow(candidates) == 0) {
    return(empty_source_raw_question_match_log())
  }

  alias_sources <- if (exists("raw_question_alias_source_cols")) {
    raw_question_alias_source_cols(survey_id)
  } else {
    character()
  }
  label_alias_sources <- attr(raw, "registry_question_label_alias_log")$source_clean_name %||% character()
  candidates[, mapped_to_registry := raw_column_clean %in% c(registry_vars, alias_sources, label_alias_sources)]

  review_rows <- vector("list", nrow(candidates))
  registry_question_lookup <- unique(data.table::data.table(
    question_clean = clean_question_text(reg_s$question),
    question = reg_s$question,
    unique = reg_s$unique
  ))

  for (i in seq_len(nrow(candidates))) {
    raw_clean <- candidates$raw_column_clean[[i]]
    raw_original <- candidates$raw_column_original[[i]]
    mapped <- isTRUE(candidates$mapped_to_registry[[i]])

    variable_matches <- find_best_matches(raw_clean, registry_vars, DRIFT_REVIEW_TOP_N)
    question_matches <- find_best_matches(
      clean_question_text(raw_original),
      registry_question_lookup$question_clean,
      DRIFT_REVIEW_TOP_N
    )

    question_display <- format_registry_matches(
      question_matches$match,
      registry_question_lookup,
      "question_clean",
      "question"
    )

    review_rows[[i]] <- data.table::data.table(
      survey = survey_id,
      file_name = basename(file_row$file_path[[1]]),
      file_path = file_row$file_path[[1]],
      raw_column = raw_original,
      raw_column_clean = raw_clean,
      mapped_to_registry = mapped,
      nearest_registry_variables = format_registry_matches(
        variable_matches$match,
        reg_s,
        "raw_question_column_clean",
        "variable"
      ),
      nearest_registry_variable_distances = paste(variable_matches$distance, collapse = " | "),
      nearest_registry_questions = question_display,
      nearest_registry_question_distances = paste(question_matches$distance, collapse = " | "),
      review_action = ifelse(
        mapped,
        "Raw source question column is mapped to the registry.",
        "Raw source question column is not mapped to this survey registry. Confirm whether it is a real C360 question, then add/update the registry or document exclusion."
      )
    )
  }

  data.table::rbindlist(review_rows, fill = TRUE)
}

detect_question_drift_one_file <- function(file_row, registry) {
  path <- file_row$file_path[[1]]
  survey_id <- normalise_survey_id(file_row$survey[[1]])

  reg_s <- filter_registry_for_survey(registry, survey_id)
  if (nrow(reg_s) == 0) {
    return(list(
      match_log = data.table::data.table(),
      review_log = data.table::data.table(),
      unmatched_raw_log = empty_source_raw_question_match_log(),
      source_raw_log = empty_source_raw_question_match_log()
    ))
  }

  raw <- tryCatch(
    read_any_file(path),
    error = function(e) {
      warning("Could not read file during drift check: ", basename(path), " | ", e$message)
      NULL
    }
  )

  if (is.null(raw)) {
    return(list(
      match_log = data.table::data.table(),
      review_log = data.table::data.table(),
      unmatched_raw_log = empty_source_raw_question_match_log(),
      source_raw_log = empty_source_raw_question_match_log()
    ))
  }

  raw <- standardise_names(as.data.frame(raw))
  name_map <- attr(raw, "name_map")
  raw <- tryCatch(derive_date_fields(raw, survey_id, path), error = function(e) raw)
  raw <- tryCatch(apply_survey_patches(raw, survey_id), error = function(e) raw)

  if (".date" %in% names(raw)) {
    reg_s <- filter_registry_for_file_dates(reg_s, raw$.date)
  }
  raw <- apply_registry_question_label_aliases(raw, reg_s)

  raw_cols <- names(raw)
  registry_vars <- unique(reg_s$raw_question_column_clean)
  reg_s[, available_in_file := raw_question_column_clean %in% raw_cols]
  source_raw_log <- build_source_raw_question_match_log(raw, name_map, reg_s, file_row, survey_id)
  unmatched_raw_log <- source_raw_log[mapped_to_registry == FALSE]

  match_log <- reg_s[, .(
    survey = survey_id,
    file_name = basename(path),
    file_path = path,
    dataset,
    variable,
    unique,
    question,
    raw_question_column,
    raw_question_column_clean,
    available_in_file
  )]

  missing <- reg_s[available_in_file == FALSE]
  if (nrow(missing) == 0) {
    return(list(
      match_log = match_log,
      review_log = data.table::data.table(),
      unmatched_raw_log = unmatched_raw_log,
      source_raw_log = source_raw_log
    ))
  }

  candidates <- extract_raw_question_candidates(raw, name_map, registry_vars, survey_id)
  review_rows <- vector("list", nrow(missing))

  for (i in seq_len(nrow(missing))) {
    target_col <- missing$raw_question_column_clean[[i]]
    target_question <- missing$question[[i]]

    name_matches <- find_best_matches(target_col, clean_col(candidates), DRIFT_REVIEW_TOP_N)
    text_matches <- find_best_matches(clean_question_text(target_question), clean_question_text(candidates), DRIFT_REVIEW_TOP_N)

    review_rows[[i]] <- data.table::data.table(
      survey = survey_id,
      file_name = basename(path),
      file_path = path,
      dataset = missing$dataset[[i]],
      variable = missing$variable[[i]],
      unique = missing$unique[[i]],
      registry_raw_question_column = missing$raw_question_column[[i]],
      registry_raw_question_column_clean = target_col,
      registry_question = target_question,
      include_in_dashboard = missing$include_in_dashboard[[i]],
      Keyqn = missing$Keyqn[[i]],
      best_raw_column_candidates = paste(name_matches$match, collapse = " | "),
      best_raw_column_distances = paste(name_matches$distance, collapse = " | "),
      best_question_text_candidates = paste(text_matches$match, collapse = " | "),
      best_question_text_distances = paste(text_matches$distance, collapse = " | "),
      review_action = "Review raw file and registry. Do not silently remap."
    )
  }

  list(
    match_log = match_log,
    review_log = data.table::rbindlist(review_rows, fill = TRUE),
    unmatched_raw_log = unmatched_raw_log,
    source_raw_log = source_raw_log
  )
}

question_match_log <- data.table::data.table()
question_drift_review <- data.table::data.table()
unmatched_raw_question_review <- empty_source_raw_question_match_log()
source_raw_question_match_log <- empty_source_raw_question_match_log()

if (isTRUE(RUN_QUESTION_DRIFT_CHECKS) && nrow(file_index) > 0) {
  drift_scope <- if (exists("DRIFT_CHECK_FILE_SCOPE") && DRIFT_CHECK_FILE_SCOPE == "files_to_process") {
    files_to_process
  } else {
    file_index
  }

  if (nrow(drift_scope) > 0) {
    drift_results <- c360_progress_lapply(
      nrow(drift_scope),
      "Question drift check",
      function(i) detect_question_drift_one_file(drift_scope[i], registry)
    )

    question_match_log <- data.table::rbindlist(
      lapply(drift_results, `[[`, "match_log"),
      fill = TRUE
    )
    question_drift_review <- data.table::rbindlist(
      lapply(drift_results, `[[`, "review_log"),
      fill = TRUE
    )
    unmatched_raw_question_review <- data.table::rbindlist(
      lapply(drift_results, `[[`, "unmatched_raw_log"),
      fill = TRUE
    )
    source_raw_question_match_log <- data.table::rbindlist(
      lapply(drift_results, `[[`, "source_raw_log"),
      fill = TRUE
    )
  }
}

write_log(question_match_log, file.path(LOG_DIR, paste0(Sys.Date(), "_02_question_match_log.csv")))
write_log(question_drift_review, file.path(LOG_DIR, paste0(Sys.Date(), "_02_question_drift_review.csv")))
write_log(
  unmatched_raw_question_review,
  file.path(LOG_DIR, paste0(Sys.Date(), "_02_unmatched_raw_question_review.csv"))
)
write_log(
  source_raw_question_match_log,
  file.path(LOG_DIR, paste0(Sys.Date(), "_02_source_question_registry_match.csv"))
)

message("Question match log rows: ", nrow(question_match_log))
message("Question drift review rows: ", nrow(question_drift_review))
message("Unmatched raw question review rows: ", nrow(unmatched_raw_question_review))
message("Source raw question match rows: ", nrow(source_raw_question_match_log))
message("02_registry_and_file_discovery.R completed.")
