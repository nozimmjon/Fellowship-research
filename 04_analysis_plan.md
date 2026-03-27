# 04 Analysis Plan (Locked Final Spec)

## 1) Frozen Question
"How persistent is educational attainment across generations in Uzbekistan, how has that persistence changed from 2010 to 2022-23, and which household and regional factors are associated with higher mobility?"

## 2) Architecture Lock
- Module A: core descriptive mobility results (2010, 2016, 2022-23).
- Module B: correlates of mobility with locked core covariates and conditional additions.
- Module C: short LiTS IV mechanism section (2022-23 only), suggestive and non-causal.
- Reform/COVID DiD: conditional only, not part of main baseline until admin diagnostics pass.

## 3) Sample Lock
- Age 25-64.
- Cohorts: 25-34, 35-44, 45-54, 55-64.
- One record per LiTS respondent.
- Weighted estimation in tables and regressions.
- Suppress subgroup metrics when valid `N < 30`.

## 4) Measure Lock (Module A)
1. `rank_rank_slope`
2. `transition_matrix_share`
3. `upward_mobility_rate`
4. `downward_mobility_rate`
5. `persistence_probability`

Subgroup splits:
- overall
- urban/rural
- gender
- region
- cohort

## 5) Variable Lock

### Core analysis variables
- `wave_year`
- `own_years_schooling`
- `parent_years_schooling`
- `own_ed_level`
- `parent_ed_level`
- `region`
- `urban`
- `gender`
- `cohort`

### Conditional Tier B variables
- `hh_income_proxy`
- `migration_exposure`
- `multigenerational_hh`

### Harmonization rule
- Main parent category proxy: `max(father_ed_level, mother_ed_level)`.
- Robustness proxy: averaged ordered parent score.

## 6) Equation Lock
- Eq. 1 wave-specific rank-rank slope.
- Eq. 2 pooled persistence with wave interactions.
- Eq. 3 attainment score correlates model.
- Eq. 4 upward mobility model (full + restricted low-parent sample).
- Eq. 5 persistence heterogeneity interactions.
- Eq. 6 LiTS IV mechanism model (2022-23 only; suggestive).
- Eq. 7 HBS supplemental model (appendix-only, conditional on linkage quality).

## 7) Estimation Rules
- Weighted estimates by default.
- Robust SEs clustered at PSU if available; otherwise at region.
- No causal language in Modules A and B.
- Module C language restricted to association/suggestive evidence.

## 8) Deliverable Map

### Main text tables
1. Data sources and harmonization.
2. Sample composition by wave.
3. National mobility metrics by wave.
4. Transition matrices by wave.
5. Pooled persistence regressions.
6. Heterogeneity interactions.
7. Upward mobility regressions.
8. LiTS IV mechanism results.

### Appendix tables
- A1 variable crosswalk by wave.
- A2 covariate coverage/selection.
- A3 parent proxy robustness.
- A4 HBS linkage diagnostics.
- A5 HBS supplemental estimates (conditional).

### Main figures
1. Trend in mobility metrics by wave.
2. Transition heatmaps by wave.
3. Region-level mobility plot.
4. Cohort mobility gradients.
5. LiTS IV mechanism coefficient plot.

## 9) Current Code and Metadata Anchors
- `data/metadata/mobility_measure_set.csv`
- `data/metadata/mobility_variable_lock.csv`
- `outputs/tables/module_a_*`
- `outputs/tables/tier_a_*`
- `outputs/tables/module_b_*`
- `outputs/tables/module_c_mechanism_*`
- `outputs/tables/module_c_mechanism_robustness_*`
