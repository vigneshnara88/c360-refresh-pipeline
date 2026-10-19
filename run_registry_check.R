# Registry and file-match check only.
# This is useful after editing the Excel registry, before running a full update.

options(
  C360_RUN_MODE = "quarterly",
  C360_PREFLIGHT_ONLY = TRUE,
  C360_FULL_REBUILD = TRUE
)

source("06_run_pipeline.R")
