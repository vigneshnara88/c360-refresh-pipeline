# ============================================================
# 00_config.R
# C360 registry-driven quarterly update pipeline
# ============================================================
#
# Analyst workflow for a normal quarterly update:
# 1. Put the newest quarterly raw files under data/esp, data/mpolls,
#    data/pgp, and data/s100.
# 2. Update the question registry workbook if questions or metadata changed.
# 3. Run source("06_run_pipeline.R").
#
# Important registry rules:
# - The registry is the ingestion universe. The pipeline processes all valid
#   registry-listed questions that are present in raw files.
# - Key Question? is a metadata tag only.
# - Include in Dashboard? is a display/export flag only.
# - Dashboard filtering happens in 05_bolt_on_export.R, never during ingestion.
# - Question wording/code changes should be documented in registry Remarks.

# ============================================================
# 00A. Analyst quick settings
# ============================================================
#
# Most analysts should use one of these wrapper scripts instead of editing this
# file each run:
# - run_quarterly_update.R
# - run_annual_update.R
# - run_preflight_check.R
#
# The wrappers set C360_* options before 06_run_pipeline.R loads this config.
# Direct edits here are still fine when paths or defaults change.

c360_get_setting <- function(name, default) {
  opt <- getOption(name, NULL)
  if (!is.null(opt)) {
    return(opt)
  }

  env <- Sys.getenv(name, unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(env)
  }

  default
}

c360_as_logical <- function(x, default = FALSE) {
  if (is.logical(x)) {
    return(isTRUE(x))
  }
  x <- tolower(trimws(as.character(x)))
  if (x %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }
  if (x %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }
  default
}

c360_as_vector <- function(x, default = character()) {
  if (length(x) > 1) {
    return(tolower(trimws(as.character(x))))
  }
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return(default)
  }
  tolower(trimws(strsplit(x, ",", fixed = TRUE)[[1]]))
}

c360_normalise_quarter <- function(x) {
  if (is.null(x) || length(x) == 0) {
    x <- ""
  }
  x <- trimws(as.character(x[[1]]))
  if (!nzchar(x)) {
    return(NA_character_)
  }

  x_lower <- tolower(x)
  m <- regexec("(20[0-9]{2}|[0-9]{2})[^0-9a-z]*q[^1-4]*([1-4])", x_lower)
  parts <- regmatches(x_lower, m)[[1]]
  if (length(parts) == 3) {
    yy <- as.integer(parts[[2]])
    if (yy < 100) yy <- yy + 2000
    return(sprintf("%04d Q%d", yy, as.integer(parts[[3]])))
  }

  m <- regexec("q[^1-4]*([1-4])[^0-9a-z]*(20[0-9]{2}|[0-9]{2})", x_lower)
  parts <- regmatches(x_lower, m)[[1]]
  if (length(parts) == 3) {
    yy <- as.integer(parts[[3]])
    if (yy < 100) yy <- yy + 2000
    return(sprintf("%04d Q%d", yy, as.integer(parts[[2]])))
  }

  stop("C360_TARGET_QUARTER must look like '2026 Q1' or '2026q1'.")
}

# quarterly = pgp, mpolls, s100, esp.
# annual    = quarterly surveys plus pps and trust.
# custom    = CUSTOM_SURVEYS_TO_RUN below.
RUN_MODE <- c360_get_setting("C360_RUN_MODE", "quarterly")
FULL_REBUILD <- c360_as_logical(c360_get_setting("C360_FULL_REBUILD", FALSE))
RUN_PREFLIGHT_ONLY <- c360_as_logical(c360_get_setting("C360_PREFLIGHT_ONLY", FALSE))
CUSTOM_SURVEYS_TO_RUN <- c360_as_vector(c360_get_setting("C360_CUSTOM_SURVEYS", "mpolls"), "mpolls")
TARGET_UPDATE_QUARTER <- c360_normalise_quarter(c360_get_setting("C360_TARGET_QUARTER", ""))
ENFORCE_TARGET_QUARTER <- c360_as_logical(
  c360_get_setting("C360_ENFORCE_TARGET_QUARTER", !is.na(TARGET_UPDATE_QUARTER)),
  !is.na(TARGET_UPDATE_QUARTER)
)

# ============================================================
# 00B. Project paths
# ============================================================

PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

DATA_DIR <- file.path(PROJECT_DIR, "data")
RUN_DATE <- as.character(Sys.Date())
RUN_TIMESTAMP_LABEL <- format(Sys.time(), "%Y-%m-%d_%H%M")
OUTPUT_BASE_DIR <- file.path(PROJECT_DIR, "outputs")
RUN_OUTPUT_DIR <- file.path(OUTPUT_BASE_DIR, paste0("run_", RUN_DATE))
OUTPUT_DIR <- file.path(RUN_OUTPUT_DIR, "exports")
SUPPORTING_OUTPUT_DIR <- file.path(RUN_OUTPUT_DIR, "supporting")
LOG_DIR <- file.path(RUN_OUTPUT_DIR, "qc")
INTERMEDIATE_DIR <- file.path(RUN_OUTPUT_DIR, "intermediate")
STATE_DIR <- file.path(OUTPUT_BASE_DIR, "_state")
QC_DIR <- LOG_DIR
QC_RUN_DIR <- LOG_DIR

# Supply the question registry locally; no registry or survey data is distributed.
REGISTRY_PATH <- c360_get_setting("C360_REGISTRY_PATH", file.path(DATA_DIR, "question_registry.xlsx"))
REGISTRY_SHEET <- "Master_Registry"
REGISTRY_SHEET_FALLBACKS <- c("Master_Registry", "Sheet2", "Sheet1")

# Existing historical C360 export to bolt the new quarterly update onto.
# If absent, 05 will still export the newly processed rows.
EXISTING_C360_CSV <- c360_get_setting(
  "C360_EXISTING_C360_CSV",
  file.path(
    DATA_DIR,
    "existing_c360.csv"
  )
)

FULL_OUTPUT_CSV <- file.path(SUPPORTING_OUTPUT_DIR, "c360_full_all_registry_questions.csv")
DASHBOARD_OUTPUT_CSV <- file.path(SUPPORTING_OUTPUT_DIR, "c360_dashboard_included_questions.csv")
KEY_QUESTION_OUTPUT_CSV <- file.path(SUPPORTING_OUTPUT_DIR, "c360_key_questions.csv")

WRITE_DATED_TABLEAU_EXPORTS <- c360_as_logical(
  c360_get_setting("C360_WRITE_DATED_TABLEAU_EXPORTS", TRUE),
  TRUE
)

TABLEAU_EXPORT_STEM <- paste0(
  RUN_TIMESTAMP_LABEL,
  "_Dashboard_Yearly_Quarterly_Singlecut_For Tableau"
)
if (isTRUE(WRITE_DATED_TABLEAU_EXPORTS)) {
  INTERNAL_TABLEAU_WIDE_CSV <- file.path(
    OUTPUT_DIR,
    paste0(TABLEAU_EXPORT_STEM, "_Internal.csv")
  )
  INTERNAL_TABLEAU_TRIMMED_CSV <- file.path(
    OUTPUT_DIR,
    paste0(TABLEAU_EXPORT_STEM, "_Internal_Trimmed.csv")
  )
  EXTERNAL_TABLEAU_TRIMMED_CSV <- file.path(
    OUTPUT_DIR,
    paste0(TABLEAU_EXPORT_STEM, "_External_Trimmed.csv")
  )
} else {
  INTERNAL_TABLEAU_WIDE_CSV <- file.path(OUTPUT_DIR, "c360_internal.csv")
  INTERNAL_TABLEAU_TRIMMED_CSV <- file.path(OUTPUT_DIR, "c360_internal_trimmed.csv")
  EXTERNAL_TABLEAU_TRIMMED_CSV <- file.path(OUTPUT_DIR, "c360_external_trimmed.csv")
}
EXTERNAL_TABLEAU_SANITISED_CSV <- EXTERNAL_TABLEAU_TRIMMED_CSV
EXTERNAL_TABLEAU_SANITISED_TRIMMED_CSV <- EXTERNAL_TABLEAU_TRIMMED_CSV
# The old-format c360_qn_df respondent YTD bolt-on is expensive. Routine
# quarterly runs skip it by default; set C360_WRITE_LEGACY_RESPONDENT_QN_DF=TRUE
# for the year-end respondent-level baseline update.
WRITE_LEGACY_RESPONDENT_QN_DF <- c360_as_logical(
  c360_get_setting("C360_WRITE_LEGACY_RESPONDENT_QN_DF", FALSE),
  FALSE
)
C360_QN_DF_CANDIDATE_EXISTING_CSVS <- c(
  file.path(PROJECT_DIR, "c360_qn_df.csv"),
  file.path(DATA_DIR, "c360_qn_df.csv"),
  file.path(dirname(PROJECT_DIR), "c360", "c360_qn_df.csv")
)
C360_QN_DF_EXISTING_CSV_IDX <- which(file.exists(C360_QN_DF_CANDIDATE_EXISTING_CSVS))
C360_QN_DF_DEFAULT_EXISTING_CSV <- if (length(C360_QN_DF_EXISTING_CSV_IDX) > 0) {
  C360_QN_DF_CANDIDATE_EXISTING_CSVS[[C360_QN_DF_EXISTING_CSV_IDX[[1]]]]
} else {
  C360_QN_DF_CANDIDATE_EXISTING_CSVS[[1]]
}
EXISTING_C360_QN_DF_CSV <- c360_get_setting(
  "C360_EXISTING_QN_DF_CSV",
  C360_QN_DF_DEFAULT_EXISTING_CSV
)
C360_QN_DF_CURRENT_RUN_CSV <- file.path(
  INTERMEDIATE_DIR,
  paste0(RUN_TIMESTAMP_LABEL, "_respondent_level_c360_current_run.csv")
)
C360_QN_DF_YTD_UPDATED_CSV <- file.path(
  INTERMEDIATE_DIR,
  paste0(RUN_TIMESTAMP_LABEL, "_respondent_level_c360.csv")
)
C360_QN_DF_ID_STYLE <- tolower(trimws(as.character(c360_get_setting(
  "C360_QN_DF_ID_STYLE",
  "survey_year_id"
))))
if (!(C360_QN_DF_ID_STYLE %in% c("survey_year_id", "year_survey_id", "source"))) {
  stop("C360_QN_DF_ID_STYLE must be one of: survey_year_id, year_survey_id, source.")
}

# Compatibility names used by the V2 skeleton.
LATEST_OUTPUT_CSV <- FULL_OUTPUT_CSV
SANITISED_OUTPUT_CSV <- EXTERNAL_TABLEAU_SANITISED_CSV
TRIMMED_OUTPUT_CSV <- EXTERNAL_TABLEAU_SANITISED_TRIMMED_CSV

MANIFEST_PATH <- file.path(STATE_DIR, "processing_manifest.rds")

dir.create(OUTPUT_BASE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RUN_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SUPPORTING_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INTERMEDIATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(QC_RUN_DIR, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 00C. Survey refresh cadence
# ============================================================

SURVEY_REFRESH_CADENCE <- c(
  esp = "quarterly",
  mpolls = "quarterly",
  pgp = "quarterly",
  s100 = "quarterly",
  sps = "quarterly",
  pps = "yearly",
  trust = "yearly"
)

regular_quarterly_surveys <- names(SURVEY_REFRESH_CADENCE)[SURVEY_REFRESH_CADENCE == "quarterly"]
yearly_surveys <- names(SURVEY_REFRESH_CADENCE)[SURVEY_REFRESH_CADENCE == "yearly"]

if (RUN_MODE == "quarterly") {
  SURVEYS_TO_RUN <- regular_quarterly_surveys
} else if (RUN_MODE == "annual") {
  SURVEYS_TO_RUN <- c(regular_quarterly_surveys, yearly_surveys)
} else if (RUN_MODE == "custom") {
  SURVEYS_TO_RUN <- CUSTOM_SURVEYS_TO_RUN
} else {
  stop("RUN_MODE must be one of: quarterly, annual, custom.")
}

# ============================================================
# 00D. File ingestion rules
# ============================================================

VALID_EXTENSIONS <- c("csv", "txt", "xlsx", "xls", "sav", "rds")
EXCLUDE_FILE_PATTERNS <- c(
  "dictionary",
  "data dictionary",
  "questionnaire",
  "metadata",
  "mapping",
  "readme",
  "unique_questions",
  "registry",
  "~\\$"
)

# Used only by source-file-first QA. Registry-mapped columns are always retained,
# but obvious operational fields should not be reported as "unmatched questions".
RAW_QUESTION_EXCLUDE_PATTERNS <- c(
  "^\\.",
  "^@",
  "(^|_)unique(_|$)",
  "(^|_)(id|uuid|guid|caseid|serial|record|hhid)(_|$)",
  "hhid$",
  "respondent",
  "response_id",
  "panel_id",
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

# Optional survey-specific QA suppressions for known non-C360 source modules.
# These only affect "new question detected" QA noise; registry-mapped columns
# are still kept even when they look like metadata.
RAW_QUESTION_EXCLUDE_PATTERNS_BY_SURVEY <- list(
  mpolls = c(
    "^qtr$",
    "^q4_(months|years)$"
  ),
  s100 = c(
    "^postalcode$",
    "^d[0-9]+[a-z]?(?:_[a-z0-9]+)?$"
  )
)

RECURSIVE_FILE_DISCOVERY <- FALSE

# period_files: process all valid files in the selected survey folder.
# latest_file_only: use only the most recently modified file in the folder.
source_file_mode <- c(
  esp = "period_files",
  mpolls = "period_files",
  pgp = "period_files",
  s100 = "period_files",
  # CCP is retained for historical/custom rebuilds, not normal annual refreshes.
  ccp = "period_files",
  pps = "period_files",
  sps = "period_files",
  trust = "period_files"
)

# ============================================================
# 00E. Registry and display metadata
# ============================================================

DIRECTION_VALUES <- c("Standard", "Reverse", "Check")
DASHBOARD_INCLUDE_VALUES <- c("include", "included", "yes", "y", "true", "1")
KEY_QUESTION_YES_VALUES <- c("yes", "y", "true", "1", "key")

DEFAULT_SCALE_TYPE <- "auto_likert"
SUPPORTED_SCALE_TYPES <- c(
  "auto_likert",
  "likert_1_5",
  "likert_0_10",
  "likert_1_9",
  "better_same_worse"
)

RUN_QUESTION_DRIFT_CHECKS <- TRUE
DRIFT_CHECK_FILE_SCOPE <- "files_to_process"
DRIFT_REVIEW_TOP_N <- 5

# Direct full runs still perform a pre-ingestion QC gate after 02 and before 03.
# Dashboard-included missing registry questions are blocking by default because
# they would otherwise produce partial dashboard output silently. Other missing
# registry questions stay visible in QA but do not stop ingestion by default.
RUN_PRE_INGESTION_QC_GATE <- c360_as_logical(
  c360_get_setting("C360_RUN_PRE_INGESTION_QC_GATE", TRUE),
  TRUE
)
STOP_ON_MISSING_DASHBOARD_REGISTRY_QUESTIONS <- c360_as_logical(
  c360_get_setting(
    "C360_STOP_ON_MISSING_DASHBOARD_REGISTRY_QUESTIONS",
    c360_get_setting("C360_STOP_ON_MISSING_REGISTRY_QUESTIONS", TRUE)
  ),
  TRUE
)
FORCE_RUN_WITH_MISSING_DASHBOARD_QUESTIONS <- c360_as_logical(
  c360_get_setting("C360_FORCE_RUN_WITH_MISSING_DASHBOARD_QUESTIONS", FALSE),
  FALSE
)
STOP_ON_NEW_QUESTIONS_DETECTED <- c360_as_logical(
  c360_get_setting("C360_STOP_ON_NEW_QUESTIONS_DETECTED", FALSE),
  FALSE
)
QA_RECONCILIATION_TOP_N_PER_MISSING <- as.integer(
  c360_get_setting("C360_QA_RECONCILIATION_TOP_N_PER_MISSING", 3)
)

# ============================================================
# 00F. Historical survey patches
# ============================================================

ENABLE_SURVEY_PATCHES <- TRUE

# Confirmed source-column aliases. Names are source columns in the raw file;
# values are the registry/raw-question columns C360 should ingest into.
# Keep this list small and source-owner reviewed. It is applied before matching.
RAW_QUESTION_ALIASES_BY_SURVEY <- list(
  mpolls = c(
    # 2026 Q1 source-code shifts reviewed from QA reconciliation output.
    # These fill the registry target column before discovery and ingestion.
    q112a_1 = "q1004",
    q142_7 = "q1027",
    q121_1 = "q1032r1",
    q122a_1 = "q1032r2",
    q146_2 = "q1069",
    q19a_1_13 = "q19a_1_3",
    q104_3 = "q40a_3",
    q114_1 = "q51a_1",
    q113_1 = "q61a_1",
    q113_2 = "q61a_2",
    q113_3 = "q61a_3",
    q113_4 = "q61a_4",
    q113_5 = "q61a_5",
    q113_97 = "q61a_9",
    q142_1 = "q54a_1",
    q142_2 = "q54a_2",
    q21a_1_17 = "q21a_1_7",
    q23a_2 = "q38a_2",
    q23a_3 = "q38a_3",
    q202 = "q2020",
    q203_1 = "q2033a",
    q203_2 = "q2033b",
    q203_3 = "q2033c",
    q207a = "q2072",
    q207b = "q2074"
  ),
  s100 = c(
    s2 = "q2",
    s3a = "q3",
    s5 = "q5",
    s11 = "q11"
  )
)

# Confirmed MPolls aliases/scattered-data patches. Registry rows should keep
# using the q20a variables; the helper fills q20a from q20b before matching.
MPOLLS_Q20B_TO_Q20A_ALIASES <- c(
  q20b_1_8 = "q20a_1_8",
  q20b_1_9 = "q20a_1_9",
  q20b_1_15 = "q20a_1_15",
  q20b_1_16 = "q20a_1_16"
)

MPOLLS_Q20A_1_9_DROP_FROM <- as.Date("2014-06-01")
MPOLLS_Q20A_1_9_DROP_TO <- as.Date("2014-10-31")
MPOLLS_Q90A_2_COMPARABLE_FROM <- as.Date("2023-11-01")
MPOLLS_Q51A_COMPARABLE_FROM <- as.Date("2023-06-01")
MPOLLS_Q19A_1_2_COMPARABLE_FROM <- as.Date("2020-12-01")
MPOLLS_Q40A_COMPARABLE_FROM <- as.Date("2023-04-01")
MPOLLS_Q20A_1_6_COMPARABLE_FROM <- as.Date("2023-08-01")

MISSING_VALUE_LABELS <- c(
  "",
  "na",
  "n/a",
  "#null",
  "#null!",
  "null",
  "unknown",
  "don't know",
  "dont know",
  "not sure",
  "refused",
  "refuse to answer",
  "prefer not to say",
  "prefer not to disclose",
  "prefer not to answer",
  "none of the above",
  "98",
  "99",
  "97",
  "-88",
  "-89"
)

# ============================================================
# 00G. Weight rules
# ============================================================

# Use one overall weight consistently for all demographic cuts.
overall_weight_candidates <- c(
  "overall_weight",
  "weight_overall",
  "weightoverall",
  "weight",
  "weight_combined",
  "weigh_combined",
  "sr_weightoverall",
  "sr_weightage",
  "weight_overall_capped",
  "Weight_Overall",
  "Weight"
)

MISSING_WEIGHT_ACTION <- "set_to_1"

# PGP weights dated 1 Sep 2025 onwards are forced to a maximum of 3.
PGP_WEIGHT_CAP_AFTER_DATE <- as.Date("2025-08-31")
PGP_WEIGHT_CAP_VALUE <- 3

# ============================================================
# 00H. Demographic cuts
# ============================================================

DEMO_CUTS <- c(
  "overall",
  "gender",
  "race",
  "age",
  "age_40up",
  "educ",
  "income",
  "household_income",
  "housing",
  "housing2",
  "marital",
  "evermarried",
  "pmet",
  "educ_group2",
  "degree_type",
  "lbsc",
  "demographic_group",
  "vulnerable_group",
  "planning_area",
  "region",
  "town_council",
  "rental_housing"
)

DEMO_COL_SYNONYMS <- list(
  gender = c("gender", "sex", "q9", "d1_gender", "sr_gender"),
  race = c("race", "ethnicity", "q11", "d2_race", "sr_race"),
  age = c("age", "age_group", "agegrp", "q10", "d1_age", "sr_age"),
  age_40up = c("age_40up", "age40up", "age_40_up"),
  educ = c("educ", "education", "highest_education", "q14", "d4", "sr_education"),
  income = c("income", "income2", "personal_income", "monthly_income", "q70"),
  household_income = c("household_income", "hh_income", "d3", "d3_1", "householdincome", "q120"),
  housing = c("housing", "housing_type", "house_type", "flat_type"),
  housing2 = c("housing2", "housing_group", "dwelling_type"),
  marital = c("marital", "marital_status", "d6"),
  evermarried = c("evermarried", "ever_married"),
  pmet = c("pmet", "pmet_status"),
  educ_group2 = c("educ_group2", "educ_group"),
  degree_type = c("degree_type", "degree"),
  lbsc = c("lbsc"),
  demographic_group = c("demographic_group"),
  vulnerable_group = c("vulnerable_group", "vulnerable"),
  planning_area = c("planning_area", "Planning_Area"),
  region = c("region", "Region"),
  town_council = c("town_council", "Town_Council"),
  rental_housing = c("rental_housing", "Rental_Housing")
)

# ============================================================
# 00I. Date and period rules
# ============================================================

DATE_COL_CANDIDATES <- c(
  "date",
  "Date",
  "start_date",
  "Start_Date",
  "START_DATE",
  "startdate",
  "interview_date",
  "date_of_interview",
  "Date.of.Interview",
  "OGdate",
  "time",
  "TIME"
)

MONTH_COL_CANDIDATES <- c("month", "Month", "MONTH", "mth", "survey_month")
YEAR_COL_CANDIDATES <- c("year", "Year", "YEAR", "yr", "survey_year")

DASHBOARD_OUTPUT_PERIODS <- c("Quarter", "Year")
INTERNAL_TEST_PERIODS <- c("Month", "Week")

period_levels_by_survey <- list(
  esp = c("Quarter", "Year", "Month", "Week"),
  mpolls = c("Quarter", "Year", "Month", "Week"),
  pgp = c("Quarter", "Year", "Month", "Week"),
  s100 = c("Quarter", "Year", "Month", "Week"),
  sps = c("Quarter", "Year", "Month", "Week"),
  ccp = c("Year"),
  pps = c("Year"),
  trust = c("Year")
)

# ============================================================
# 00J. Output and bolt-on rules
# ============================================================

MIN_SAMPLE_N <- 10

REPLACE_KEYS <- c("dataset", "unique", "demo", "demography", "Time", "Period")
OUTPUT_YEAR_TIME_LABEL <- "Annual"
KEEP_OLD_DISGREE_TYPO <- TRUE

PRECEDING_QUARTERS_FOR_SD <- 2
PRECEDING_YEARS_FOR_BASELINE <- 5

# Current Tableau dashboards are built on the old Sensitivity/Trend columns and
# old dashboard labels. Keep the default as legacy-compatible; CI labels remain
# opt-in analyst/debug fields.
TREND_LABEL_CONSECUTIVE_DECLINE <- "Consecutive Decline"
TREND_LABEL_CONSECUTIVE_IMPROVEMENT <- "Consecutive Improvement"
TREND_LABEL_NEITHER <- "Neither"
TREND_LABEL_NOT_APPLICABLE <- "Not Applicable"
SENSITIVITY_INCLUDE <- "Include"
SENSITIVITY_EXCLUDE <- "Exclude"
OLD_STAT_SIG_PVALUE_CUTOFF <- 0.10
OLD_STAT_SIG_LABEL_IMPROVE <- "Significant & Substantive Improvement"
OLD_STAT_SIG_LABEL_DECLINE <- "Significant & Substantive Decline"
OLD_STAT_SIG_LABEL_NO_CHANGE <- "No Significant & Substantive Change"
OLD_STAT_SIG_LABEL_INSUFFICIENT <- "Insufficient Baseline Data"
OLD_STAT_SIG_LABEL_NOT_APPLICABLE <- "Not Applicable"

SIGNIFICANCE_FLAGGING_APPROACH <- tolower(c360_get_setting(
  "C360_SIGNIFICANCE_FLAGGING_APPROACH",
  "legacy_cut"
))
if (!(SIGNIFICANCE_FLAGGING_APPROACH %in% c("legacy_cut", "ci", "hybrid_latest_ci"))) {
  stop("C360_SIGNIFICANCE_FLAGGING_APPROACH must be one of: legacy_cut, ci, hybrid_latest_ci.")
}
ENABLE_CI_SIGNIFICANCE_OUTPUT <- SIGNIFICANCE_FLAGGING_APPROACH %in% c("ci", "hybrid_latest_ci")
CI_SUBSTANTIVE_CHANGE_THRESHOLD <- suppressWarnings(as.numeric(c360_get_setting(
  "C360_CI_SUBSTANTIVE_CHANGE_THRESHOLD",
  3
)))
if (!is.finite(CI_SUBSTANTIVE_CHANGE_THRESHOLD)) {
  CI_SUBSTANTIVE_CHANGE_THRESHOLD <- 3
}
ENABLE_CI_MISSING_SE_RELATIVE_FALLBACK <- c360_as_logical(
  c360_get_setting("C360_ENABLE_CI_MISSING_SE_RELATIVE_FALLBACK", TRUE),
  TRUE
)
CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD <- suppressWarnings(as.numeric(c360_get_setting(
  "C360_CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD",
  0.15
)))
if (!is.finite(CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD) || CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD < 0) {
  CI_MISSING_SE_RELATIVE_FALLBACK_THRESHOLD <- 0.15
}
CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE <- suppressWarnings(as.numeric(c360_get_setting(
  "C360_CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE",
  CI_SUBSTANTIVE_CHANGE_THRESHOLD
)))
if (!is.finite(CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE) || CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE < 0) {
  CI_MISSING_SE_RELATIVE_FALLBACK_MIN_ABS_CHANGE <- CI_SUBSTANTIVE_CHANGE_THRESHOLD
}

FULL_INTERNAL_COLUMNS <- c(
  "dataset", "dataset_label", "update_frequency", "normal_refresh_mode",
  "source_folder", "variable", "demo", "demography", "question", "unique",
  "Tier1", "Tier2", "Tier3", "Keyqn", "key_question_flag",
  "include_in_dashboard", "direction", "Direction Coding", "Remarks",
  "Agree", "Disagree", "Neutral", "Net_Satisfaction", "total.sample.n",
  "unweighted_n", "effective_n", "Net_Sat_SE", "Net_Sat_CI_Lower",
  "Net_Sat_CI_Upper",
  "Sensitivity Count - Consective Decline (Net-Sat)",
  "Sensitivity Count - Consective Improved (Net-Sat)",
  "Sensitivity Count - No Consecutive Changes (Net-Sat)",
  "Trend", "Time", "Period", "Change (Agree)", "Change (Net-Satisfaction)",
  "SD (Preceding 2Q - Agree)", "SD (Preceding 2Q - Net)", "Sensitivity",
  "1", "2", "3", "4", "5", "f0", "f1", "f2a", "f2b", "f3", "f4",
  "b2b", "b3", "b4", "Polarisation_Index",
  "%Strongly Disagree", "%Disgree", "%Neutral", "%Agree",
  "%Strongly Agree", "%Net-Satisfaction", "Overall %Net-Satisfaction",
  "Diff %Net-Sat (Demo - Overall)", "Std Dev within Demo", "Notes",
  "Annual Change (Net-Satisfaction)",
  "Baseline Reference (Preceding 5 Year Mean)",
  "Deviation from Reference",
  "Baseline Deviation (Preceding 5 Year Absolute Mean)",
  "Sensitivity Count - No Change (Net-Sat)",
  "Sensitivity Count - Improvement (Net-Sat)",
  "Sensitivity Count - Decline (Net-Sat)",
  "Last_Processed", "Input_Reference", "source_file", "source_file_mtime"
)

LEGACY_WIDE_COLUMNS <- c(
  "dataset", "variable", "demo", "demography", "question", "unique",
  "Tier1", "Tier2", "Tier3", "Keyqn",
  "Agree", "Disagree", "Neutral", "Net_Satisfaction", "total.sample.n",
  "Sensitivity Count - Consective Decline (Net-Sat)",
  "Sensitivity Count - Consective Improved (Net-Sat)",
  "Sensitivity Count - No Consecutive Changes (Net-Sat)",
  "Trend", "Time", "Period",
  "Change (Agree)", "Change (Net-Satisfaction)",
  "SD (Preceding 2Q - Agree)", "SD (Preceding 2Q - Net)",
  "Sensitivity",
  "1", "2", "3", "4", "5",
  "f0", "f1", "f2a", "f2b", "f3", "f4",
  "b2b", "b3", "b4", "Polarisation_Index",
  "%Strongly Disagree", "%Disgree", "%Neutral", "%Agree",
  "%Strongly Agree", "%Net-Satisfaction",
  "Overall %Net-Satisfaction", "Diff %Net-Sat (Demo - Overall)",
  "Std Dev within Demo", "Notes",
  "Annual Change (Net-Satisfaction)",
  "Baseline Reference (Preceding 5 Year Mean)",
  "Deviation from Reference",
  "Baseline Deviation (Preceding 5 Year Absolute Mean)",
  "Sensitivity Count - No Change (Net-Sat)",
  "Sensitivity Count - Improvement (Net-Sat)",
  "Sensitivity Count - Decline (Net-Sat)",
  "Last_Processed", "Input_Reference"
)

OPI_AND_COUNT_COLUMNS <- c(
  "1", "2", "3", "4", "5",
  "f0", "f1", "f2a", "f2b", "f3", "f4",
  "b2b", "b3", "b4",
  "Polarisation_Index"
)

LEGACY_TRIMMED_COLUMNS <- setdiff(LEGACY_WIDE_COLUMNS, OPI_AND_COUNT_COLUMNS)

CI_OUTPUT_COLUMNS <- c(
  "Net_Sat_SE",
  "Net_Sat_CI_Lower",
  "Net_Sat_CI_Upper"
)

CI_SIGNIFICANCE_COLUMNS <- c(
  "Change_Net_Sat_SE",
  "Change_Net_Sat_CI_Lower",
  "Change_Net_Sat_CI_Upper",
  "Stat_Sig_CI",
  "Substantive_Change",
  "Change_Relative_To_Previous_Net_Sat",
  "Change_Flag_Method",
  "Change_Flag"
)

EXTERNAL_TABLEAU_EXCLUDE_DEMOS <- character()
EXTERNAL_TABLEAU_EXCLUDE_DEMOGRAPHIES <- character()

DASHBOARD_OUTPUT_COLUMNS <- LEGACY_TRIMMED_COLUMNS
TRIMMED_OUTPUT_COLUMNS <- DASHBOARD_OUTPUT_COLUMNS
