# ============================================================
# 01_helpers.R
# Shared helper functions for C360 pipeline
# ============================================================

required_packages <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "stringr",
  "readxl",
  "haven",
  "lubridate"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install missing R packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(data.table)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

is_blankish <- function(x) {
  x_chr <- stringr::str_trim(as.character(x))
  is.na(x) | x_chr == "" | stringr::str_to_lower(x_chr) %in% c("na", "n/a", "null", "#null", "#null!")
}

clean_col <- function(x) {
  vapply(
    x,
    function(one) {
      if (is.na(one)) {
        return("")
      }

      s <- as.character(one)
      s <- stringr::str_replace_all(s, "\ufeff", "")
      s <- stringr::str_replace_all(s, "[\r\n\t]", " ")
      s <- stringr::str_squish(s)

      # MPolls and similar raw files often store question text after the
      # variable code, for example "Q15a_1_4. I am reasonably confident...".
      # Keep only the leading code when that pattern is visible.
      code_match <- stringr::str_match(
        s,
        "^\\s*([A-Za-z]+[0-9][A-Za-z0-9]*(?:_[0-9A-Za-z]+)*)(?:\\s|\\.|\\(|$)"
      )[, 2]

      if (!is.na(code_match) && nzchar(code_match)) {
        s <- code_match
      }

      s <- stringr::str_replace_all(s, "\\.+", "_")
      s <- stringr::str_replace_all(s, "\\s+", "_")
      s <- stringr::str_replace_all(s, "[^A-Za-z0-9_]", "_")
      s <- stringr::str_replace_all(s, "_+", "_")
      s <- stringr::str_replace_all(s, "^_|_$", "")
      stringr::str_to_lower(s)
    },
    character(1),
    USE.NAMES = FALSE
  )
}

clean_question_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\ufeff", "")
  x <- stringr::str_replace_all(x, "[\r\n\t]", " ")
  x <- stringr::str_squish(x)
  x <- stringr::str_replace(
    x,
    "^\\s*[A-Za-z]+[0-9][A-Za-z0-9]*(?:_[A-Za-z0-9]+)*(?:\\s*[\\.:\\)-]\\s+)(?=[A-Za-z])",
    ""
  )
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9 ]", " ")
  stringr::str_squish(x)
}

normalise_survey_id <- function(x) {
  stringr::str_to_lower(stringr::str_squish(as.character(x)))
}

filter_registry_for_survey <- function(registry, survey_id) {
  reg <- data.table::as.data.table(registry)
  if (!("dataset" %in% names(reg))) {
    stop("Registry is missing required dataset column.")
  }

  survey_key <- normalise_survey_id(survey_id)
  dataset_key <- normalise_survey_id(reg[["dataset"]])
  reg[dataset_key == survey_key]
}

add_missing_columns <- function(df, cols, value = NA) {
  for (cc in cols) {
    if (!(cc %in% names(df))) {
      df[[cc]] <- if (nrow(df) == 0) value[0] else value
    }
  }
  df
}

write_log <- function(log_dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(log_dt)) {
    log_dt <- data.table::as.data.table(log_dt)
    if (ncol(log_dt) == 0) {
      log_dt <- data.table::data.table(note = "no rows")
    }
    data.table::fwrite(log_dt, path, na = "")
  }
  invisible(path)
}

c360_format_duration <- function(seconds) {
  seconds <- round(as.numeric(seconds))
  if (!is.finite(seconds) || is.na(seconds)) {
    return("unknown")
  }

  hours <- seconds %/% 3600
  minutes <- (seconds %% 3600) %/% 60
  secs <- seconds %% 60

  if (hours > 0) {
    sprintf("%02d:%02d:%02d", hours, minutes, secs)
  } else {
    sprintf("%02d:%02d", minutes, secs)
  }
}

c360_progress_lapply <- function(n, label, FUN) {
  n <- as.integer(n)
  if (is.na(n) || n <= 0) {
    return(list())
  }

  message(label, ": ", n, " step(s)")
  start_time <- Sys.time()
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)

  out <- vector("list", n)
  for (i in seq_len(n)) {
    out[[i]] <- FUN(i)
    utils::setTxtProgressBar(pb, i)

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    eta <- if (i > 0) elapsed / i * (n - i) else NA_real_
    message(
      label, " ", i, " of ", n,
      " | elapsed: ", c360_format_duration(elapsed),
      " | ETA: ", c360_format_duration(eta)
    )
  }

  out
}

normalise_flag <- function(x) {
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  x[is.na(x) | x == "" | x %in% c("na", "n/a", "null")] <- NA_character_
  x
}

normalise_key_flag <- function(x) {
  key <- normalise_flag(x)
  yes_values <- if (exists("KEY_QUESTION_YES_VALUES")) KEY_QUESTION_YES_VALUES else c("yes", "y", "true", "1")
  ifelse(!is.na(key) & key %in% yes_values, "Yes", "No")
}

normalise_dashboard_flag <- function(x) {
  key <- normalise_flag(x)
  include_values <- if (exists("DASHBOARD_INCLUDE_VALUES")) {
    DASHBOARD_INCLUDE_VALUES
  } else {
    c("include", "included", "yes", "y", "true", "1")
  }
  ifelse(!is.na(key) & key %in% include_values, "Include", "Exclude")
}

is_dashboard_include <- function(x) {
  normalise_dashboard_flag(x) == "Include"
}

is_key_yes <- function(x) {
  normalise_key_flag(x) == "Yes"
}

# ============================================================
# File reading and column names
# ============================================================

read_any_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("csv", "txt")) {
    out <- data.table::fread(
      path,
      stringsAsFactors = FALSE,
      encoding = "UTF-8",
      na.strings = c("", "NA", "N/A", "NULL", "#NULL", "#NULL!")
    )
    return(as.data.frame(out))
  }

  if (ext %in% c("xlsx", "xls")) {
    out <- readxl::read_excel(path, sheet = 1, col_types = "text", .name_repair = "minimal")
    return(as.data.frame(out))
  }

  if (ext == "sav") {
    out <- haven::read_sav(path, user_na = TRUE)
    column_labels <- vapply(
      out,
      function(col) {
        label <- attr(col, "label", exact = TRUE)
        if (is.null(label) || length(label) == 0) NA_character_ else as.character(label[[1]])
      },
      character(1),
      USE.NAMES = FALSE
    )
    column_label_map <- data.frame(
      original_name = names(out),
      original_label = column_labels,
      stringsAsFactors = FALSE
    )
    out <- haven::as_factor(out, only_labelled = TRUE)
    out <- as.data.frame(out)
    attr(out, "c360_column_label_map") <- column_label_map
    return(out)
  }

  if (ext == "rds") {
    out <- readRDS(path)
    if (is.data.frame(out)) {
      return(as.data.frame(out))
    }
    if (is.list(out) && "data" %in% names(out) && is.data.frame(out$data)) {
      return(as.data.frame(out$data))
    }
    stop("RDS file is not a data.frame and is not a list with $data.")
  }

  stop("Unsupported file extension: ", ext)
}

standardise_names <- function(df) {
  old_names <- names(df)
  new_names <- clean_col(old_names)
  source_label_map <- attr(df, "c360_column_label_map")
  if (is.null(source_label_map) || nrow(source_label_map) == 0) {
    source_label_map <- data.frame(
      original_name = old_names,
      original_label = vapply(
        df,
        function(col) {
          label <- attr(col, "label", exact = TRUE)
          if (is.null(label) || length(label) == 0) NA_character_ else as.character(label[[1]])
        },
        character(1),
        USE.NAMES = FALSE
      ),
      stringsAsFactors = FALSE
    )
  } else {
    source_label_map <- as.data.frame(source_label_map, stringsAsFactors = FALSE)
  }
  source_label_map <- add_missing_columns(source_label_map, c("original_name", "original_label"))
  label_lookup <- source_label_map$original_label[match(old_names, source_label_map$original_name)]

  blank_idx <- is.na(new_names) | new_names == ""
  duplicate_idx <- duplicated(new_names) & !blank_idx
  keep_idx <- !blank_idx & !duplicate_idx

  dropped <- data.frame(
    original_name = old_names[!keep_idx],
    clean_name = new_names[!keep_idx],
    reason = ifelse(blank_idx[!keep_idx], "blank_column_name", "duplicate_clean_name"),
    stringsAsFactors = FALSE
  )

  df <- df[, keep_idx, drop = FALSE]
  names(df) <- new_names[keep_idx]

  attr(df, "name_map") <- data.frame(
    original_name = old_names[keep_idx],
    clean_name = new_names[keep_idx],
    stringsAsFactors = FALSE
  )
  attr(df, "column_label_map") <- data.frame(
    original_name = old_names[keep_idx],
    clean_name = new_names[keep_idx],
    original_label = label_lookup[keep_idx],
    stringsAsFactors = FALSE
  )
  attr(df, "dropped_name_map") <- dropped

  df
}

apply_registry_question_label_aliases <- function(df, reg_s) {
  label_map <- attr(df, "column_label_map")
  if (is.null(label_map) || nrow(label_map) == 0 || nrow(reg_s) == 0) {
    return(df)
  }

  label_map <- data.table::as.data.table(label_map)
  label_map <- add_missing_columns(label_map, c("original_name", "clean_name", "original_label"))
  label_map <- label_map[
    clean_name %in% names(df) &
      !is_blankish(original_label)
  ]
  if (nrow(label_map) == 0) {
    return(df)
  }

  reg_s <- data.table::as.data.table(reg_s)
  reg_s <- add_missing_columns(reg_s, c("raw_question_column_clean", "raw_question_column", "unique", "question"))
  reg_s <- reg_s[
    !(raw_question_column_clean %in% names(df)) &
      !is_blankish(raw_question_column_clean) &
      !is_blankish(question)
  ]
  if (nrow(reg_s) == 0) {
    return(df)
  }

  label_map[, question_label_clean := clean_question_text(original_label)]
  reg_s[, registry_question_clean := clean_question_text(question)]
  label_map <- label_map[!is_blankish(question_label_clean)]
  reg_s <- reg_s[!is_blankish(registry_question_clean)]

  candidates <- merge(
    unique(reg_s[, .(
      raw_question_column_clean,
      raw_question_column,
      unique,
      question,
      registry_question_clean
    )]),
    unique(label_map[, .(
      source_clean_name = clean_name,
      source_original_name = original_name,
      source_label = original_label,
      question_label_clean
    )]),
    by.x = "registry_question_clean",
    by.y = "question_label_clean",
    allow.cartesian = TRUE
  )
  if (nrow(candidates) == 0) {
    return(df)
  }

  candidates[, sources_per_target := data.table::uniqueN(source_clean_name), by = raw_question_column_clean]
  candidates[, targets_per_source := data.table::uniqueN(raw_question_column_clean), by = source_clean_name]
  candidates <- candidates[sources_per_target == 1L & targets_per_source == 1L]
  if (nrow(candidates) == 0) {
    return(df)
  }

  data.table::setorder(candidates, raw_question_column_clean, source_clean_name)
  candidates <- unique(candidates, by = c("raw_question_column_clean", "source_clean_name"))

  for (i in seq_len(nrow(candidates))) {
    source <- candidates$source_clean_name[[i]]
    target <- candidates$raw_question_column_clean[[i]]
    if (source %in% names(df) && !(target %in% names(df))) {
      df[[target]] <- df[[source]]
    }
  }

  attr(df, "registry_question_label_alias_log") <- candidates[, .(
    source_original_name,
    source_clean_name,
    source_label,
    target_clean_name = raw_question_column_clean,
    target_raw_question_column = raw_question_column,
    target_unique = unique,
    target_question = question,
    reason = "registry_question_label_alias"
  )]
  df
}

find_first_existing_col <- function(df, candidates) {
  candidates_clean <- clean_col(candidates)
  found <- candidates_clean[candidates_clean %in% names(df)]
  if (length(found) == 0) NA_character_ else found[[1]]
}

# ============================================================
# Date and period helpers
# ============================================================

parse_date_flexible <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    out <- rep(as.Date(NA), length(x))
    idx <- !is.na(x) & x > 20000 & x < 60000
    out[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(out)
  }

  x_chr <- stringr::str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  x_chr <- stringr::str_replace_all(x_chr, "--", "-")

  out <- suppressWarnings(as.Date(x_chr))

  need_parse <- is.na(out) & !is.na(x_chr)
  if (any(need_parse)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_chr[need_parse],
      orders = c(
        "ymd", "dmy", "mdy", "Ymd", "dmY", "mdY",
        "b-y", "b-Y", "B-y", "B-Y", "Y-b", "Y-B",
        "Y b", "Y B", "b Y", "B Y", "Y-m", "m-Y", "Y q", "Yq"
      ),
      quiet = TRUE,
      tz = "UTC"
    ))
    out[need_parse] <- as.Date(parsed)
  }

  out
}

parse_period_from_filename <- function(path) {
  file <- stringr::str_to_lower(basename(path))

  q_match <- stringr::str_match(file, "(20[0-9]{2}|[0-9]{2})\\s*[-_ ]?q\\s*([1-4])")
  if (!is.na(q_match[1, 1])) {
    yy <- as.integer(q_match[1, 2])
    if (yy < 100) yy <- 2000 + yy
    qq <- as.integer(q_match[1, 3])
    return(as.Date(sprintf("%04d-%02d-01", yy, (qq - 1) * 3 + 1)))
  }

  q_match <- stringr::str_match(file, "([1-4])q\\s*[-_ ]?(20[0-9]{2}|[0-9]{2})")
  if (!is.na(q_match[1, 1])) {
    qq <- as.integer(q_match[1, 2])
    yy <- as.integer(q_match[1, 3])
    if (yy < 100) yy <- 2000 + yy
    return(as.Date(sprintf("%04d-%02d-01", yy, (qq - 1) * 3 + 1)))
  }

  month_match <- stringr::str_match(file, "([a-z]{3,9})[-_ ]+([0-9]{2,4})")
  if (!is.na(month_match[1, 1])) {
    mm <- match(substr(month_match[1, 2], 1, 3), tolower(month.abb))
    yy <- as.integer(month_match[1, 3])
    if (!is.na(mm) && !is.na(yy)) {
      if (yy < 100) yy <- 2000 + yy
      return(as.Date(sprintf("%04d-%02d-01", yy, mm)))
    }
  }

  month_match <- stringr::str_match(file, "(20[0-9]{2}|[0-9]{2})[-_ ]+([a-z]{3,9})")
  if (!is.na(month_match[1, 1])) {
    yy <- as.integer(month_match[1, 2])
    mm <- match(substr(month_match[1, 3], 1, 3), tolower(month.abb))
    if (!is.na(mm) && !is.na(yy)) {
      if (yy < 100) yy <- 2000 + yy
      return(as.Date(sprintf("%04d-%02d-01", yy, mm)))
    }
  }

  y_match <- stringr::str_match(file, "(20[0-9]{2})")
  if (!is.na(y_match[1, 1])) {
    return(as.Date(sprintf("%s-01-01", y_match[1, 1])))
  }

  as.Date(NA)
}

derive_date_fields <- function(df, survey = NA_character_, path = NA_character_) {
  date_col <- find_first_existing_col(df, DATE_COL_CANDIDATES)
  parsed_date <- rep(as.Date(NA), nrow(df))
  date_source <- rep("unparsed", nrow(df))
  date_source_column <- rep(NA_character_, nrow(df))

  if (!is.na(date_col)) {
    parsed_date <- parse_date_flexible(df[[date_col]])
    if (any(!is.na(parsed_date))) {
      date_source[!is.na(parsed_date)] <- "date_column"
      date_source_column[!is.na(parsed_date)] <- date_col
    }
  }

  if (all(is.na(parsed_date))) {
    month_col <- find_first_existing_col(df, MONTH_COL_CANDIDATES)
    year_col <- find_first_existing_col(df, YEAR_COL_CANDIDATES)

    if (!is.na(month_col) && !is.na(year_col)) {
      month_raw <- as.character(df[[month_col]])
      year_raw <- as.character(df[[year_col]])
      month_num <- suppressWarnings(as.integer(month_raw))
      month_num[is.na(month_num)] <- match(substr(tolower(month_raw[is.na(month_num)]), 1, 3), tolower(month.abb))
      year_num <- suppressWarnings(as.integer(year_raw))
      year_num[!is.na(year_num) & year_num < 100] <- year_num[!is.na(year_num) & year_num < 100] + 2000

      ok <- !is.na(month_num) & !is.na(year_num)
      parsed_date[ok] <- as.Date(sprintf("%04d-%02d-01", year_num[ok], month_num[ok]))
      date_source[ok] <- "month_year_columns"
      date_source_column[ok] <- paste(month_col, year_col, sep = " + ")
    }
  }

  if (all(is.na(parsed_date)) && !is.na(path)) {
    inferred <- parse_period_from_filename(path)
    parsed_date <- rep(inferred, nrow(df))
    if (!is.na(inferred)) {
      date_source <- rep("filename", nrow(df))
      date_source_column <- rep(basename(path), nrow(df))
    }
  }

  df$.date <- as.Date(parsed_date)
  df$.date_source <- as.character(date_source)
  df$.date_source_column <- as.character(date_source_column)
  df$.year <- lubridate::year(df$.date)
  df$.quarter_num <- lubridate::quarter(df$.date)
  df$.month_num <- lubridate::month(df$.date)
  df$.week_num <- lubridate::isoweek(df$.date)

  df$.year_period <- ifelse(is.na(df$.year), NA_character_, as.character(df$.year))
  df$.quarter_period <- ifelse(
    is.na(df$.year) | is.na(df$.quarter_num),
    NA_character_,
    paste0(df$.year, " Q", df$.quarter_num)
  )
  df$.month_period <- ifelse(
    is.na(df$.year) | is.na(df$.month_num),
    NA_character_,
    paste(df$.year, month.abb[df$.month_num])
  )
  df$.week_period <- ifelse(
    is.na(df$.year) | is.na(df$.week_num),
    NA_character_,
    sprintf("%s W%02d", df$.year, df$.week_num)
  )

  df
}

quarter_period_bounds <- function(period) {
  period <- stringr::str_squish(as.character(period %||% ""))
  m <- stringr::str_match(period, "^(20[0-9]{2})\\s*Q\\s*([1-4])$")
  if (is.na(m[1, 1])) {
    return(list(start = as.Date(NA), end = as.Date(NA), year = NA_integer_, quarter = NA_integer_))
  }

  yy <- as.integer(m[1, 2])
  qq <- as.integer(m[1, 3])
  start_month <- (qq - 1) * 3 + 1
  start <- as.Date(sprintf("%04d-%02d-01", yy, start_month))
  next_start <- if (qq == 4) {
    as.Date(sprintf("%04d-01-01", yy + 1))
  } else {
    as.Date(sprintf("%04d-%02d-01", yy, start_month + 3))
  }

  list(start = start, end = next_start - 1, year = yy, quarter = qq)
}

target_quarter_enabled <- function() {
  exists("ENFORCE_TARGET_QUARTER", inherits = TRUE) &&
    isTRUE(get("ENFORCE_TARGET_QUARTER", inherits = TRUE)) &&
    exists("TARGET_UPDATE_QUARTER", inherits = TRUE) &&
    !is.na(get("TARGET_UPDATE_QUARTER", inherits = TRUE))
}

target_quarter_bounds <- function() {
  if (!target_quarter_enabled()) {
    return(quarter_period_bounds(NA_character_))
  }
  quarter_period_bounds(get("TARGET_UPDATE_QUARTER", inherits = TRUE))
}

assign_target_quarter_date_fields <- function(df, source = "target_quarter") {
  bounds <- target_quarter_bounds()
  if (is.na(bounds$start)) {
    return(df)
  }

  df$.date <- as.Date(bounds$start)
  df$.date_source <- source
  df$.date_source_column <- get("TARGET_UPDATE_QUARTER", inherits = TRUE)
  df$.year <- bounds$year
  df$.quarter_num <- bounds$quarter
  df$.month_num <- lubridate::month(df$.date)
  df$.week_num <- lubridate::isoweek(df$.date)
  df$.year_period <- as.character(bounds$year)
  df$.quarter_period <- get("TARGET_UPDATE_QUARTER", inherits = TRUE)
  df$.month_period <- paste(bounds$year, month.abb[df$.month_num])
  df$.week_period <- sprintf("%s W%02d", bounds$year, df$.week_num)
  df
}

period_order_value <- function(Time, Period) {
  Time <- as.character(Time)
  Period <- as.character(Period)
  out <- rep(NA_real_, length(Period))

  is_year <- Time %in% c("Year", "Annual")
  out[is_year] <- suppressWarnings(as.numeric(stringr::str_extract(Period[is_year], "20[0-9]{2}")))

  is_quarter <- Time == "Quarter"
  year_q <- suppressWarnings(as.numeric(stringr::str_extract(Period[is_quarter], "20[0-9]{2}")))
  q_q <- suppressWarnings(as.numeric(stringr::str_match(Period[is_quarter], "Q\\s*([1-4])")[, 2]))
  out[is_quarter] <- year_q * 4 + q_q

  is_month <- Time == "Month"
  if (any(is_month)) {
    month_date <- parse_date_flexible(Period[is_month])
    out[is_month] <- lubridate::year(month_date) * 12 + lubridate::month(month_date)
  }

  is_week <- Time == "Week"
  if (any(is_week)) {
    week_year <- suppressWarnings(as.numeric(stringr::str_extract(Period[is_week], "20[0-9]{2}")))
    week_num <- suppressWarnings(as.numeric(stringr::str_match(Period[is_week], "W\\s*([0-9]{1,2})")[, 2]))
    out[is_week] <- week_year * 53 + week_num
  }

  out
}

# ============================================================
# Survey patches and response scoring
# ============================================================

coalesce_nonmissing <- function(target, source) {
  target_chr <- as.character(target)
  source_chr <- as.character(source)
  target_blank <- is.na(target_chr) | stringr::str_trim(target_chr) == ""
  source_present <- !is.na(source_chr) & stringr::str_trim(source_chr) != ""
  target_chr[target_blank & source_present] <- source_chr[target_blank & source_present]
  target_chr
}

set_na_before_date <- function(df, cols, comparable_from) {
  if (!(".date" %in% names(df))) {
    return(df)
  }
  idx <- !is.na(df$.date) & df$.date < comparable_from
  idx[is.na(idx)] <- FALSE
  for (cc in intersect(clean_col(cols), names(df))) {
    df[[cc]][idx] <- NA
  }
  df
}

raw_question_aliases_for_survey <- function(survey) {
  if (!exists("RAW_QUESTION_ALIASES_BY_SURVEY")) {
    return(character())
  }

  aliases <- RAW_QUESTION_ALIASES_BY_SURVEY
  survey_key <- normalise_survey_id(survey)
  if (!is.list(aliases) || !(survey_key %in% names(aliases))) {
    return(character())
  }

  out <- aliases[[survey_key]]
  if (is.null(out) || length(out) == 0) {
    return(character())
  }
  out
}

raw_question_alias_source_cols <- function(survey) {
  clean_col(names(raw_question_aliases_for_survey(survey)))
}

apply_raw_question_aliases <- function(df, survey) {
  aliases <- raw_question_aliases_for_survey(survey)
  if (length(aliases) == 0) {
    return(df)
  }

  source_cols <- clean_col(names(aliases))
  target_cols <- clean_col(unname(aliases))

  for (i in seq_along(source_cols)) {
    source <- source_cols[[i]]
    target <- target_cols[[i]]
    if (source %in% names(df)) {
      if (!(target %in% names(df))) {
        df[[target]] <- NA_character_
      }
      df[[target]] <- coalesce_nonmissing(df[[target]], df[[source]])
    }
  }

  df
}

patch_mpolls_historical <- function(df) {
  aliases <- if (exists("MPOLLS_Q20B_TO_Q20A_ALIASES")) {
    MPOLLS_Q20B_TO_Q20A_ALIASES
  } else {
    c(q20b_1_8 = "q20a_1_8", q20b_1_9 = "q20a_1_9", q20b_1_15 = "q20a_1_15")
  }

  source_cols <- clean_col(names(aliases))
  target_cols <- clean_col(unname(aliases))

  for (i in seq_along(source_cols)) {
    source <- source_cols[[i]]
    target <- target_cols[[i]]
    if (source %in% names(df)) {
      if (!(target %in% names(df))) {
        df[[target]] <- NA_character_
      }
      df[[target]] <- coalesce_nonmissing(df[[target]], df[[source]])
    }
  }

  if ("q20a_1_9" %in% names(df) && ".date" %in% names(df)) {
    drop_from <- MPOLLS_Q20A_1_9_DROP_FROM %||% as.Date("2014-06-01")
    drop_to <- MPOLLS_Q20A_1_9_DROP_TO %||% as.Date("2014-10-31")
    idx <- !is.na(df$.date) & df$.date >= drop_from & df$.date <= drop_to
    idx[is.na(idx)] <- FALSE
    df$q20a_1_9[idx] <- NA
  }

  if ("q90a_2" %in% names(df) && "q88a_1_5" %in% names(df)) {
    df$q90a_2 <- coalesce_nonmissing(df$q90a_2, df$q88a_1_5)
  }

  df <- set_na_before_date(df, "q90a_2", MPOLLS_Q90A_2_COMPARABLE_FROM)
  df <- set_na_before_date(df, c("q51a_4", "q51a_2", "q51a_3"), MPOLLS_Q51A_COMPARABLE_FROM)
  df <- set_na_before_date(df, "q19a_1_2", MPOLLS_Q19A_1_2_COMPARABLE_FROM)
  df <- set_na_before_date(df, c("q40a_2", "q40a_4"), MPOLLS_Q40A_COMPARABLE_FROM)
  df <- set_na_before_date(df, "q20a_1_6", MPOLLS_Q20A_1_6_COMPARABLE_FROM)

  df
}

apply_survey_patches <- function(df, survey) {
  if (!exists("ENABLE_SURVEY_PATCHES") || !isTRUE(ENABLE_SURVEY_PATCHES)) {
    return(df)
  }

  survey <- stringr::str_to_lower(as.character(survey))

  df <- apply_raw_question_aliases(df, survey)

  if (survey == "mpolls") {
    df <- patch_mpolls_historical(df)
  }

  df
}

is_missing_label <- function(x) {
  x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  x_chr <- stringr::str_replace_all(x_chr, "\u2019", "'")
  missing_values <- if (exists("MISSING_VALUE_LABELS")) {
    stringr::str_to_lower(MISSING_VALUE_LABELS)
  } else {
    c("", "na", "n/a", "#null", "#null!", "unknown", "don't know", "dont know", "refused", "98", "99")
  }
  x_chr %in% missing_values |
    stringr::str_detect(x_chr, "prefer not") |
    stringr::str_detect(x_chr, "refus") |
    stringr::str_detect(x_chr, "don't know") |
    stringr::str_detect(x_chr, "dont know") |
    stringr::str_detect(x_chr, "not sure")
}

strip_value_label_prefix <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  stringr::str_squish(stringr::str_replace(x_chr, "^[0-9]+\\s*[\\.)-]\\s*", ""))
}

standardise_score_col <- function(x, scale_type = DEFAULT_SCALE_TYPE) {
  raw <- as.character(x)
  raw <- stringr::str_squish(raw)
  raw[raw == ""] <- NA_character_

  label <- strip_value_label_prefix(raw)
  lower <- stringr::str_to_lower(label)
  lower <- stringr::str_replace_all(lower, "\u2019", "'")

  out <- rep(NA_real_, length(raw))
  detects <- function(pattern) {
    !is.na(lower) & stringr::str_detect(lower, pattern)
  }

  missing_idx <- is.na(raw) | is_missing_label(raw) | is_missing_label(label)

  out[detects("strongly\\s+disagree|very\\s+dissatisfied")] <- 1
  out[is.na(out) & detects("^disagree$|dissatisfied")] <- 2
  out[is.na(out) & detects("neutral|neither|remain\\s+the\\s+same|no\\s+change")] <- 3
  out[is.na(out) & detects("^agree$|satisfied|better")] <- 4
  out[detects("strongly\\s+agree|very\\s+satisfied")] <- 5
  out[is.na(out) & detects("^worse$|worse\\s+off")] <- 2

  numeric_raw <- suppressWarnings(as.numeric(raw))
  numeric_label <- suppressWarnings(as.numeric(label))
  num <- ifelse(is.na(numeric_label), numeric_raw, numeric_label)

  nonmissing_num <- num[!is.na(num) & !missing_idx]
  max_num <- if (length(nonmissing_num) == 0) NA_real_ else max(nonmissing_num, na.rm = TRUE)
  min_num <- if (length(nonmissing_num) == 0) NA_real_ else min(nonmissing_num, na.rm = TRUE)
  scale_type <- stringr::str_to_lower(as.character(scale_type %||% DEFAULT_SCALE_TYPE))

  numeric_idx <- is.na(out) & !is.na(num) & !missing_idx

  if (scale_type %in% c("likert_0_10") || (scale_type == "auto_likert" && !is.na(max_num) && min_num >= 0 && max_num > 5 && max_num <= 10)) {
    out[numeric_idx & num %in% c(0, 1)] <- 1
    out[numeric_idx & num %in% c(2, 3, 4)] <- 2
    out[numeric_idx & num == 5] <- 3
    out[numeric_idx & num %in% c(6, 7, 8)] <- 4
    out[numeric_idx & num %in% c(9, 10)] <- 5
  } else if (scale_type %in% c("likert_1_9") || (scale_type == "auto_likert" && !is.na(max_num) && min_num >= 1 && max_num > 5 && max_num <= 9)) {
    out[numeric_idx & num %in% c(1, 2)] <- 1
    out[numeric_idx & num %in% c(3, 4)] <- 2
    out[numeric_idx & num == 5] <- 3
    out[numeric_idx & num %in% c(6, 7)] <- 4
    out[numeric_idx & num %in% c(8, 9)] <- 5
  } else {
    out[numeric_idx & num %in% 1:5] <- num[numeric_idx & num %in% 1:5]
  }

  out[missing_idx] <- NA_real_
  out[!(out %in% 1:5)] <- NA_real_
  out
}

apply_direction_coding <- function(score, direction) {
  direction <- stringr::str_to_lower(stringr::str_squish(as.character(direction)))
  out <- safe_as_numeric(score)
  reverse_idx <- !is.na(out) & direction == "reverse"
  out[reverse_idx] <- 6 - out[reverse_idx]
  out
}

# ============================================================
# Demographic and weight helpers
# ============================================================

clean_value_key <- function(x) {
  x <- stringr::str_to_lower(as.character(x))
  x <- stringr::str_replace_all(x, "\u2019", "'")
  x <- stringr::str_replace_all(x, "\\$", "")
  x <- stringr::str_replace_all(x, ",", "")
  x <- stringr::str_replace_all(x, "&", "and")
  stringr::str_replace_all(x, "[^a-z0-9]", "")
}

recode_personal_income_to_dashboard_band <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  key <- clean_value_key(x_chr)
  out <- dplyr::case_when(
    key %in% c("noincome", "noincomeorallowance") ~ "No Income",
    key %in% c("1", "01", "under1000", "below1000", "lessthan1000", "10001499", "15001999") ~ "$1-1999",
    key %in% c("14", "15", "16", "56", "57", "20002499", "25002999", "25002817", "28182999", "30003499", "35003999", "20003999") ~ "$2000-3999",
    key %in% c("17", "18", "19", "40004999", "50005999", "40005999") ~ "$4000-5999",
    key %in% c("20", "21", "22", "60007999", "80009999", "60009999") ~ "$6000-9999",
    key %in% c("23", "24", "25", "26", "10000andabove", "1000014999", "1500019999", "20000andabove") ~ "$10000andabove",
    TRUE ~ x_chr
  )
  out[is_missing_label(x_chr)] <- NA_character_
  out
}

recode_household_income_detail <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  key <- clean_value_key(x_chr)
  out <- dplyr::case_when(
    key %in% c("1", "under1000", "below1000") ~ "$1-$999",
    key %in% c("2", "10001499") ~ "$1000-$1499",
    key %in% c("3", "15001999") ~ "$1500-$1999",
    key %in% c("4", "20002499") ~ "$2000-$2499",
    key %in% c("5", "25002999", "27", "28", "25002817", "28182999") ~ "$2500-$2999",
    key %in% c("6", "30003499") ~ "$3000-$3499",
    key %in% c("7", "35003999") ~ "$3500-$3999",
    key %in% c("8", "40004499") ~ "$4000-$4499",
    key %in% c("9", "45004999") ~ "$4500-$4999",
    key %in% c("10", "50005999") ~ "$5000-$5999",
    key %in% c("11", "60006999") ~ "$6000-$6999",
    key %in% c("12", "70007999") ~ "$7000-$7999",
    key %in% c("13", "80008999") ~ "$8000-$8999",
    key %in% c("14", "90009999") ~ "$9000-$9999",
    key %in% c("15", "1000010999") ~ "$10000-$10999",
    key %in% c("16", "1100011999") ~ "$11000-$11999",
    key %in% c("17", "1200012999") ~ "$12000-$12999",
    key %in% c("18", "1300013999") ~ "$13000-$13999",
    key %in% c("19", "1400014999") ~ "$14000-$14999",
    key %in% c("20", "1500015999") ~ "$15000-$15999",
    key %in% c("21", "1600016999") ~ "$16000-$16999",
    key %in% c("22", "1700017999") ~ "$17000-$17999",
    key %in% c("23", "1800018999") ~ "$18000-$18999",
    key %in% c("24", "1900019999") ~ "$19000-$19999",
    key %in% c("25", "26", "20000andabove", "20000above") ~ "$20000andabove",
    TRUE ~ x_chr
  )
  out[is_missing_label(x_chr)] <- NA_character_
  out
}

derive_age_40up_from_age <- function(x) {
  x_chr <- as.character(x)
  age_num <- suppressWarnings(as.numeric(stringr::str_extract(x_chr, "[0-9]{1,3}")))
  dplyr::case_when(
    !is.na(age_num) & age_num >= 40 ~ "40 and above",
    !is.na(age_num) & age_num < 40 ~ "Below 40",
    stringr::str_detect(stringr::str_to_lower(x_chr), "40|older|above") ~ "40 and above",
    TRUE ~ NA_character_
  )
}

normalise_demo_value <- function(demo, value = NULL) {
  if (is.null(value)) {
    value <- demo
    demo <- NA_character_
  }

  demo_clean <- clean_col(demo)
  x <- strip_value_label_prefix(value)
  x <- stringr::str_squish(as.character(x))
  x[x == ""] <- NA_character_

  if (!is.na(demo_clean) && demo_clean == "overall") {
    return(rep("overall", length(x)))
  }

  if (!is.na(demo_clean) && demo_clean == "income") {
    return(recode_personal_income_to_dashboard_band(x))
  }

  if (!is.na(demo_clean) && demo_clean == "household_income") {
    return(recode_household_income_detail(x))
  }

  x[is_missing_label(x)] <- NA_character_
  x_low <- stringr::str_to_lower(x)

  if (!is.na(demo_clean) && demo_clean == "gender") {
    x <- dplyr::case_when(
      x_low %in% c("1", "m", "male") ~ "Male",
      x_low %in% c("2", "f", "female") ~ "Female",
      TRUE ~ x
    )
  }

  if (!is.na(demo_clean) && demo_clean == "race") {
    x <- dplyr::case_when(
      stringr::str_detect(x_low, "chinese") ~ "Chinese",
      stringr::str_detect(x_low, "malay") ~ "Malay",
      stringr::str_detect(x_low, "indian") ~ "Indian",
      stringr::str_detect(x_low, "others|other") ~ "Others",
      TRUE ~ x
    )
  }

  if (!is.na(demo_clean) && demo_clean == "age_40up") {
    derived <- derive_age_40up_from_age(x)
    x <- ifelse(is.na(derived), x, derived)
  }

  x
}

add_demo_columns <- function(df) {
  df$overall <- "overall"

  for (demo in DEMO_CUTS) {
    demo_clean <- clean_col(demo)
    if (demo_clean == "overall" || demo_clean %in% names(df)) {
      next
    }

    candidates <- DEMO_COL_SYNONYMS[[demo_clean]]
    if (is.null(candidates)) {
      df[[demo_clean]] <- NA_character_
      next
    }

    source_col <- find_first_existing_col(df, candidates)
    df[[demo_clean]] <- if (!is.na(source_col)) df[[source_col]] else NA_character_
  }

  if ("age_40up" %in% DEMO_CUTS && all(is.na(df$age_40up)) && "age" %in% names(df)) {
    df$age_40up <- derive_age_40up_from_age(df$age)
  }

  for (demo in intersect(clean_col(DEMO_CUTS), names(df))) {
    df[[demo]] <- normalise_demo_value(demo, df[[demo]])
  }

  df
}

get_weight_vector <- function(df, survey) {
  annotate_weight_vector <- function(
    w,
    pgp_weight_cap_after_date = NA_character_,
    pgp_weight_cap_limit = NA_real_,
    pgp_rows_capped = 0L,
    pgp_rows_above_cap = 0L
  ) {
    attr(w, "pgp_weight_cap_after_date") <- as.character(pgp_weight_cap_after_date)
    attr(w, "pgp_weight_cap_limit") <- as.numeric(pgp_weight_cap_limit)
    attr(w, "pgp_post_cap_rows_capped") <- as.integer(pgp_rows_capped)
    attr(w, "pgp_post_cap_rows_above_limit") <- as.integer(pgp_rows_above_cap)
    w
  }

  candidates <- clean_col(overall_weight_candidates)
  weight_col <- candidates[candidates %in% names(df)]

  if (length(weight_col) == 0) {
    if (MISSING_WEIGHT_ACTION == "drop") {
      return(annotate_weight_vector(rep(NA_real_, nrow(df))))
    }
    return(annotate_weight_vector(rep(1, nrow(df))))
  }

  w <- safe_as_numeric(df[[weight_col[[1]]]])
  if (MISSING_WEIGHT_ACTION == "set_to_1") {
    w[is.na(w)] <- 1
  }
  if (MISSING_WEIGHT_ACTION == "drop") {
    w[is.na(w)] <- NA_real_
  }
  w[!is.na(w) & w <= 0] <- NA_real_

  pgp_rows_capped <- 0L
  pgp_rows_above_cap <- 0L
  pgp_weight_cap_after_date <- NA_character_
  pgp_weight_cap_limit <- NA_real_
  if (
    stringr::str_to_lower(as.character(survey)) == "pgp" &&
      ".date" %in% names(df) &&
      exists("PGP_WEIGHT_CAP_AFTER_DATE") &&
      exists("PGP_WEIGHT_CAP_VALUE")
  ) {
    pgp_weight_cap_after_date <- as.character(PGP_WEIGHT_CAP_AFTER_DATE)
    pgp_weight_cap_limit <- PGP_WEIGHT_CAP_VALUE
    cap_idx <- !is.na(df$.date) &
      as.Date(df$.date) > PGP_WEIGHT_CAP_AFTER_DATE &
      !is.na(w) &
      w > PGP_WEIGHT_CAP_VALUE
    pgp_rows_capped <- sum(cap_idx, na.rm = TRUE)
    w[cap_idx] <- PGP_WEIGHT_CAP_VALUE
    pgp_rows_above_cap <- sum(
      !is.na(df$.date) &
        as.Date(df$.date) > PGP_WEIGHT_CAP_AFTER_DATE &
        !is.na(w) &
        w > PGP_WEIGHT_CAP_VALUE,
      na.rm = TRUE
    )
  }

  if (MISSING_WEIGHT_ACTION == "set_to_1") {
    w[is.na(w)] <- 1
  }

  annotate_weight_vector(
    w,
    pgp_weight_cap_after_date,
    pgp_weight_cap_limit,
    pgp_rows_capped,
    pgp_rows_above_cap
  )
}

get_respondent_id <- function(raw, survey, file_name) {
  candidates <- c(
    "respondent_id", "response_id", "respid", "id", "serial", "caseid",
    "uuid", "record", "record_id"
  )
  id_col <- find_first_existing_col(raw, candidates)
  if (!is.na(id_col)) {
    return(as.character(raw[[id_col]]))
  }
  paste(survey, file_name, seq_len(nrow(raw)), sep = "::")
}

filter_registry_for_file_dates <- function(registry, dates) {
  registry <- data.table::as.data.table(registry)
  if (!("valid_from" %in% names(registry))) registry[, valid_from := as.Date(NA)]
  if (!("valid_to" %in% names(registry))) registry[, valid_to := as.Date(NA)]

  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0) {
    return(registry)
  }

  file_min_date <- min(dates)
  file_max_date <- max(dates)

  registry[
    (is.na(valid_from) | valid_from <= file_max_date) &
      (is.na(valid_to) | valid_to >= file_min_date)
  ]
}

find_best_matches <- function(target, candidates, top_n = 5) {
  candidates <- unique(candidates[!is.na(candidates) & candidates != ""])
  if (length(candidates) == 0 || is.na(target) || target == "") {
    return(data.table::data.table(match = character(), distance = numeric()))
  }
  d <- utils::adist(target, candidates, partial = TRUE, ignore.case = TRUE)
  ord <- order(as.numeric(d))[seq_len(min(top_n, length(candidates)))]
  data.table::data.table(match = candidates[ord], distance = as.numeric(d)[ord])
}
