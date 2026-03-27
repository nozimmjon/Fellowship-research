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

## Quick Start
1. Open [FellowshipResearch.Rproj](C:/Users/n.ortiqov/Desktop/Fellowship%20research/FellowshipResearch.Rproj) in RStudio.
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
   - `& 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe' render reports/00_main.qmd`
   - `& 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe' render reports/10_technical_appendix.qmd`
   - `& 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe' render reports/20_policy_brief.qmd`
   - `& 'C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe' render reports/30_slides.qmd`

## Current Status
- Research strategy drafted in [research_strategy.md](C:/Users/n.ortiqov/Desktop/Fellowship%20research/research_strategy.md).
- End-to-end scaffold created.
- Step 2 data audit is completed with workbooks in `data/metadata/02_data_audit.xlsx` and `data/metadata/03_variable_crosswalk.xlsx`.
- Step 3 measure pre-commit is now locked in `data/metadata/mobility_measure_set.csv` and `data/metadata/mobility_variable_lock.csv`.
