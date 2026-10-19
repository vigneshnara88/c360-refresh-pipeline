# C360 refresh pipeline

R tooling developed for future use in recurring survey-data refreshes. The aim is to reduce repeated processing, save runtime during routine updates, and make quarterly and annual refreshes easier to maintain.

The pipeline uses a question registry to identify relevant fields, selects new or changed source files, prepares response-level data, produces weighted aggregates, and combines refreshed periods with an existing dashboard baseline.

**This repository contains R source code and documentation only.** Supply authorised inputs locally. No datasets, synthetic data, example respondent records, registry workbooks or generated reports are included.

## Why this workflow

A recurring survey dashboard needs a consistent answer to a practical question: when another quarter of responses arrives, what needs to be recomputed, and what can be retained? Source files, question wording, response coding and reporting periods can change independently. Keeping that knowledge in one analyst's memory makes each refresh harder to repeat and hand over.

The question registry provides a shared description of the fields to collect and how they contribute to reporting. Ingestion, scoring, aggregation and presentation then follow that configuration through separate stages. This makes the repository useful as a reference for future refresh work: the transformation steps and update rules are explicit, rather than embedded in a one-off manual export.

A full rebuild can repeat expensive ingestion and aggregation even when only one survey period has changed. The refresh workflow records file paths, modification times and sizes in a manifest, then selects new or changed files for processing. Quarterly, annual and custom entry points control the refresh scope. A full rebuild remains available when definitions or source histories change.

The expensive legacy respondent-level export is optional and disabled by default for routine updates. These choices are intended to save runtime; no benchmark speedup is claimed.

## How a refresh works

1. **Define the scope.** Choose a quarterly, annual or custom update and load the paths, weights and output settings in `00_config.R`.
2. **Resolve the questions and files.** Read the question registry, discover the source files and compare their metadata with the retained manifest. Registry checks make missing or changed question mappings visible before aggregation.
3. **Prepare responses.** Match source fields to the registry and reshape the selected responses into a common long format. Shared helpers handle parsing and scoring rules.
4. **Build reporting measures.** Assign reporting periods, calculate configured weighted aggregates and construct demographic cuts. A weighted measure lets responses contribute according to the supplied survey weights; its interpretation still depends on those weights and the survey design.
5. **Update the baseline.** Combine refreshed periods with the retained dashboard baseline and format the exports for downstream use. Run summaries and logs record the selected files and processing decisions.

For example, a future quarterly refresh could process newly delivered quarter files and update the corresponding reporting periods while retaining the existing history. If a scoring definition changes, the appropriate response may instead be a rebuild of the affected history so that periods remain comparable. The scripts expose both decisions through configuration.

## Code map

| File | Purpose |
| --- | --- |
| `00_config.R` | Paths, run modes, survey cadence, weight settings and output contracts. |
| `01_helpers.R` | Shared file readers, parsing, scoring and weighting helpers. |
| `02_registry_and_file_discovery.R` | Question-registry loading, file discovery and manifest selection. |
| `03_ingest_response_level.R` | Registry-matched response ingestion and long-format preparation. |
| `04_periodise_aggregate_flags.R` | Period construction, weighted aggregation and demographic cuts. |
| `05_bolt_on_export.R` | Baseline updates and dashboard export formatting. |
| `06_run_pipeline.R` | Stage orchestration and refresh summaries. |
| `run_*_update.R` | Quarterly, annual and custom refresh entry points. |
| `run_preflight_check.R`, `run_registry_check.R` | Input and registry checks before a future refresh. |

## Local setup for future use

Install the R dependencies:

```r
install.packages(c("data.table", "dplyr", "tidyr", "stringr", "readxl", "haven", "lubridate"))
```

Work from the repository root. Supply source files under `data/<survey>/`, a question-registry workbook with a `Master_Registry` sheet, and an optional prior dashboard baseline. Input definitions and coding must match the configuration before use.

```r
options(
  C360_REGISTRY_PATH = "data/question_registry.xlsx",
  C360_EXISTING_C360_CSV = "data/existing_c360.csv",
  C360_TARGET_QUARTER = "2026 Q3"
)
source("run_quarterly_update.R")
```

Use `run_annual_update.R` for the annual source set. Edit the source list in `run_custom_update.R` for a targeted update. Outputs are written under `outputs/`, including the retained manifest state and dated run folders.

The question registry determines ingestion scope. Key-question and dashboard flags control presentation after ingestion. Changing a questionnaire, registry mapping or scoring rule requires a deliberate review of the affected history; file timestamps alone do not capture every semantic change.

## Scope

This is a code archive for future operational use. It documents the refresh architecture and its configuration; it does not publish survey findings, respondent information or a live dashboard.
