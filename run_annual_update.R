# One-click annual C360 refresh.
# Surveys: pgp, mpolls, s100, esp, sps, pps, trust.

options(
  C360_RUN_MODE = "annual",
  C360_PREFLIGHT_ONLY = FALSE,
  C360_FULL_REBUILD = FALSE
)

source("06_run_pipeline.R")
