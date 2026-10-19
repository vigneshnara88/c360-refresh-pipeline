# One-click normal in-year C360 refresh.
# Surveys: pgp, mpolls, s100, esp, sps.

options(
  C360_RUN_MODE = "quarterly",
  C360_PREFLIGHT_ONLY = FALSE,
  C360_FULL_REBUILD = FALSE
)

source("06_run_pipeline.R")
