# Fellowship Research Project

This repository contains the research pipeline for intergenerational education mobility in Uzbekistan.

## Stack
- R (data cleaning, estimation, diagnostics)
- `targets` (pipeline orchestration)
- Quarto (main paper, appendix, policy brief, slides)
- Local project library in `r_libs/` (configured via `.Rprofile`)

## Project Structure
- `data/raw/`: immutable source files
- `data/processed/`: cleaned analysis-ready files
- `data/metadata/`: inventory and variable dictionaries
- `R/`: modular scripts
- `outputs/tables/`, `outputs/figures/`, `outputs/models/`: analysis outputs
- `reports/`: Quarto publication files
- `reports/01_start_here.qmd`: landing page that links the main reader-facing outputs
- `reports/05_process_guide.qmd`: plain-English walkthrough of the pipeline and empirics
- `reports/06_process_flowchart.qmd`: one-page visual map of how the paper is built

## Quick Start
1. Open `FellowshipResearch.Rproj` in RStudio.
2. Put raw datasets into:
   - `data/raw/lits/`
   - `data/raw/hbs/`
   - `data/raw/admin/`
3. Install packages (first run only):
   - `source("R/01_packages.R")`
   - `install_missing_packages()`
4. Optional reproducible environment setup:
   - `source("R/02_renv_bootstrap.R")`
   - `bootstrap_renv()`
5. Build pipeline:
    - `source("run_pipeline.R")`
6. Render reports:
    - `quarto render reports/01_start_here.qmd`
    - `quarto render reports/05_process_guide.qmd`
    - `quarto render reports/06_process_flowchart.qmd`
    - `quarto render reports/00_main.qmd`
    - `quarto render reports/10_technical_appendix.qmd`
    - `quarto render reports/20_policy_brief.qmd`
    - `quarto render reports/30_slides.qmd`
    - Windows fallback (if `quarto` is not in `PATH`): use full executable path.
7. Export audit/crosswalk sheets for sharing:
   - `source("R/14_export_audit_share_files.R")`
   - `export_audit_share_files()`

## Current Status
- Research strategy drafted in `research_strategy.md`.
- End-to-end scaffold created.
- Step 2 data audit is completed with workbooks in `data/metadata/02_data_audit.xlsx` and `data/metadata/03_variable_crosswalk.xlsx`.
- GitHub-readable CSV exports are available in `data/metadata/exports/` (index: `data/metadata/exports/README_audit_exports.md`).
- Step 3 measure pre-commit is now locked in `data/metadata/mobility_measure_set.csv` and `data/metadata/mobility_variable_lock.csv`.
