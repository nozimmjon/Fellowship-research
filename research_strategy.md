# Research Strategy: Intergenerational Educational Mobility in Uzbekistan

## 1) Frozen Paper Question
"How persistent is educational attainment across generations in Uzbekistan, how has that persistence changed from 2010 to 2022-23, and which household and regional factors are associated with higher mobility?"

## 2) Contribution Statement
This paper provides reproducible multi-wave evidence on intergenerational educational mobility in Uzbekistan using LiTS 2010, 2016, and 2022-23. It documents national trends and subgroup differences and relates mobility to household and regional correlates, while treating pandemic-era child-learning evidence as suggestive rather than causal.

## 3) Final Paper Architecture

### Module A (Core)
- National and subgroup mobility estimates from LiTS 2010, 2016, 2022-23.
- Locked measure set: rank-rank slope, transition matrix, upward mobility, downward mobility, persistence.

### Module B (Core)
- Correlates of mobility with fixed effects and locked core covariates.
- Conditional additions only if coverage/information thresholds pass.

### Module C (Short Extension)
- 2022-23 LiTS IV mechanism section using child-learning/device/support variables.
- Explicitly labeled as suggestive heterogeneity evidence.

### Conditional Module (Not Main by Default)
- Reform/COVID DiD only if admin data with credible region-by-time variation is added and diagnostics pass.

## 4) Final Sample Rules
- Respondents aged 25-64.
- Cohorts fixed to: 25-34, 35-44, 45-54, 55-64.
- One respondent-level observation per LiTS record.
- Weighted estimates in descriptive tables and regressions.
- Subgroup outputs suppressed when valid `N < 30`.

## 5) Canonical Variable Plan

### Core variables (main paper)
- `wave_year`
- `own_years_schooling`
- `parent_years_schooling`
- `own_ed_level`
- `parent_ed_level`
- `region`
- `urban`
- `gender`
- `cohort`

### Conditional Module B variables
- `hh_income_proxy`
- `migration_exposure`
- `multigenerational_hh`

## 6) Harmonization Rules
- Education categories are locked as:
  - `no_formal`, `primary`, `lower_secondary`, `upper_secondary`, `post_secondary_non_tertiary`, `tertiary`
- Parent proxy for category analysis:
  - Main spec: `parent_ed_level = max(father_ed_level, mother_ed_level)`
  - Robustness: averaged ordered parent score
- Primary cross-wave comparisons are category-based.
- Rank-rank slope is estimated with within-wave education ranks from monotone parent/child scores.

## 7) Locked Equation Set

### Eq. 1: Wave-Specific Rank-Rank Slope
`R_own(i,w) = alpha_w + beta_w * R_parent(i,w) + e(i,w)`

### Eq. 2: Pooled Persistence with Wave Interactions
`R_own(i) = alpha + beta*R_parent(i) + theta2016*(R_parent(i)*D2016) + theta2022*(R_parent(i)*D2022) + gamma'X(i) + region_FE + cohort_FE + wave_FE + e(i)`

### Eq. 3: Attainment Score Correlates
`S_own(i) = alpha + beta*S_parent(i) + gamma'X(i) + region_FE + cohort_FE + wave_FE + e(i)`

### Eq. 4: Upward Mobility Model
`Up(i) = alpha + sum_k beta_k * 1(parent_level(i)=k) + gamma'X(i) + region_FE + cohort_FE + wave_FE + e(i)`

### Eq. 5: Persistence Heterogeneity
`Persist(i) = alpha + beta*S_parent(i) + d1*(S_parent(i)*Urban(i)) + d2*(S_parent(i)*Female(i)) + d3*(S_parent(i)*D2022) + region_FE + cohort_FE + wave_FE + e(i)`

### Eq. 6: LiTS IV Mechanism Model (2022-23 only)
`M(i) = alpha + beta*S_parent(i) + gamma'X(i) + region_FE + e(i)`

### Eq. 7: HBS Supplemental Model (Appendix-only, conditional)
Run only if parent-child linkage diagnostics are adequate.

## 8) Estimation and Language Rules
- Weighted estimation throughout.
- Robust standard errors clustered at PSU if retained; otherwise clustered at region.
- Suppress subgroup outputs for `N < 30`.
- No causal verbs in Modules A and B.
- For Module C use: "associated with", "more likely", "suggestive evidence".

## 9) Main and Appendix Roadmap

### Main tables
1. Data sources, sample, and harmonization.
2. Sample composition by wave.
3. National mobility metrics by wave.
4. Transition matrices by wave.
5. Pooled persistence regressions.
6. Heterogeneity interactions.
7. Upward mobility regressions.
8. LiTS IV mechanism results (suggestive evidence).

### Appendix tables
- A1 variable crosswalk by wave.
- A2 covariate coverage and selection.
- A3 parent proxy robustness.
- A4 HBS linkage diagnostics.
- A5 HBS supplemental models (conditional).

### Main figures
1. Mobility metric trends by wave.
2. Transition heatmaps by wave.
3. Region-level mobility plot.
4. Cohort mobility gradients.
5. LiTS IV mechanism coefficient plot.

## 10) Items to Exclude from Current Main Draft
- Full causal reform claims.
- Any DiD section based on school-closure timing unless validated admin variation is available.
- Direct ma(h)alla effect claims unless measured.
- Main-text HBS intergenerational claims unless linkage diagnostics support them.

## 11) Execution Timeline (6 Weeks)
1. Week 1: freeze question, close audit, lock crosswalk.
2. Week 2: build mobility measures and national trend outputs.
3. Week 3: subgroup and regional outputs plus core figures.
4. Week 4: correlates models and mechanism section finalization.
5. Week 5: integrate HBS/admin context and draft full text.
6. Week 6: tighten identification language, finalize appendix, produce publication pack.
