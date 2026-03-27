# Research Strategy: Intergenerational Education Mobility in Uzbekistan

## 1) Study Goal
Estimate how intergenerational education mobility has changed in Uzbekistan since 2010, identify the strongest household and regional drivers, and quantify the effect of post-2017 education policy changes on mobility-relevant outcomes.

## 2) Core Research Questions
1. What are the levels and trends of intergenerational education mobility across cohorts and regions?
2. Which household and regional factors explain lower or higher mobility?
3. Did education-system changes after 2017 improve mobility-related outcomes, and for whom?

## 3) Empirical Architecture (Three Modules)

### Module A: Mobility Measurement (Descriptive + Comparative)
- Data: LiTS 2010, 2016, 2022-23 (+ any parent-child linkage available in HBS).
- Unit: adults with own education and parental education information.
- Main outcomes:
  - Years of schooling (continuous)
  - Highest degree (ordered categories)
  - Upward mobility indicator (child reaches tertiary conditional on low parental education)
- Metrics:
  - Intergenerational elasticity/slope (beta)
  - Rank-rank slope
  - Transition matrices (parent education x child education)
  - Absolute upward mobility rates by region/cohort

### Module B: Determinants of Mobility
- Micro regressions with region and cohort controls.
- Candidate determinants:
  - Parental education, occupation, income proxy
  - Household migration exposure
  - Family structure (including multigenerational household)
  - Urban/rural status
  - Local education infrastructure
- Methods:
  - OLS/ordered logit/logit (outcome-dependent)
  - Region and cohort fixed effects
  - Oaxaca-Shapley style decomposition to separate composition vs return effects

### Module C: Policy Evaluation (Post-2017 Reform Period)
- Build region-year panel (2010-2024) from administrative sources.
- Outcomes (region-year):
  - Enrollment rates, completion rates, exam performance, tertiary admission rates
- Treatment intensity (region-year):
  - Growth in school capacity, teacher recruitment, tertiary seats, public education spending per student
- Identification:
  - Event-study DiD with region and year fixed effects
  - Parallel-trend diagnostics and placebo leads
  - Clustered SEs at region level
- Note:
  - We avoid a "DiD from one cross-section" mistake by requiring true pre/post variation in region-year data.

## 4) Data Strategy

### 4.1 Data Inventory and Access Log
For each source, create a one-page record with:
- Owner and access status
- Time coverage
- Geographic coverage
- Key variables needed vs available
- Known limitations

### 4.2 Minimum Variable Dictionary
- IDs: survey wave, household ID, person ID, region, district, year
- Education: own years/level, parental years/level
- Household context: household size, migration, urban/rural, income/consumption proxy
- Policy environment: region-year school capacity, teacher counts, budget, tertiary places

### 4.3 Harmonization Rules
- Standardize region codes across years
- Build common education categories across datasets
- Define age windows (primary analysis: 25-64)
- Pre-register missing-data rules (complete-case + multiple imputation sensitivity)

## 5) Identification and Validation Plan

### 5.1 Identification Threats
- Selection into educational attainment
- Omitted regional shocks
- Survey comparability across waves

### 5.2 Mitigations
- Fixed effects (region, cohort, year)
- Covariate controls with clear causal ordering
- Robustness sets:
  - Alternative mobility definitions
  - Alternative age bands
  - Weighted vs unweighted estimates
  - Region-specific trends in policy models

### 5.3 Falsification / Placebo
- Pre-reform placebo timing in event-study
- Outcomes unlikely to move from education reforms as negative controls

## 6) R + Quarto Production System

### 6.1 Project Structure
- `data/raw/` (immutable inputs)
- `data/processed/` (analysis-ready files)
- `R/` (scripts/functions)
- `outputs/tables/`, `outputs/figures/`
- `reports/` (Quarto manuscripts)

### 6.2 Reproducibility Stack
- `renv` for package locking
- `targets` for pipeline orchestration
- `fixest`, `tidyverse`, `arrow`, `modelsummary`, `gt`, `janitor`
- Quarto for final manuscript and appendix rendering

### 6.3 Publication Outputs
- Main paper (English)
- Technical appendix (methods and robustness)
- Policy brief (short, non-technical)
- Slide deck (key results)

## 7) Execution Timeline (6 Weeks)
1. Week 1: Final question freeze, data audit completion, and variable crosswalk lock.
2. Week 2: Construct and validate mobility measures; produce national trend tables.
3. Week 3: Add subgroup and regional analysis; finalize core figures.
4. Week 4: Estimate correlates models; finalize pandemic module as mechanisms (or causal only if diagnostics pass).
5. Week 5: Integrate HBS and admin context; draft intro, data, methods, and results text.
6. Week 6: Tighten identification language, remove weak claims, finalize appendix, references, and publication outputs.

## 8) Immediate Next Actions (Start Here)
1. Build the project skeleton (`data/`, `R/`, `reports/`, `outputs/`).
2. Create `R/00_config.R` with paths, region mappings, and standard labels.
3. Create `R/10_data_inventory.R` template and fill for LiTS/HBS/admin sources.
4. Initialize Quarto report shell with sections linked to each module.

## 9) Success Criteria
- All tables/figures reproduce from a clean run.
- Main identification assumptions are explicitly tested and documented.
- Policy conclusions remain directionally stable across robustness checks.
