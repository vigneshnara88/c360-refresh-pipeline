# Preflight check only. No response-level processing or exports are written.
#
# Use "quarterly" for the normal in-year check.
# Use "annual" when pps and trust should also be checked.

options(
  C360_RUN_MODE = "quarterly",
  C360_PREFLIGHT_ONLY = TRUE,
  C360_FULL_REBUILD = FALSE
)

source("06_run_pipeline.R")
