# Custom C360 refresh for debugging or one-off source updates.
# Edit only CUSTOM_SURVEYS below, then run this file.

CUSTOM_SURVEYS <- c("mpolls")

options(
  C360_RUN_MODE = "custom",
  C360_CUSTOM_SURVEYS = CUSTOM_SURVEYS,
  C360_PREFLIGHT_ONLY = FALSE,
  C360_FULL_REBUILD = FALSE
)

source("06_run_pipeline.R")
