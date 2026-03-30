# Re-check Log: Four Review Claims

Date: 2026-03-29
Branch: codex/empirical-audit
Scope: reproducibility-and-manuscript-sync audit (frozen repo)

## Claim 1: Module B heterogeneity manuscript/output mismatch
- Verdict: TRUE
- Evidence checked:
  - `reports/00_main.qmd` (heterogeneity narrative previously said urban and female interactions were not precisely estimated).
  - `outputs/tables/module_b_model_coefficients.csv` (`eq5_persistence_heterogeneity`, `parent_ed_score:female`).
  - `R/40_module_b_determinants.R` (Eq. 5 term list includes `parent_ed_score:female`).
- Fix implemented:
  - Updated Eq. 5 discussion in `reports/00_main.qmd` to match coefficient table output and report the female interaction estimate/p-value directly.
- Outputs changed: YES
  - After full rerun with region harmonization, the same term remains statistically significant (now `estimate = -0.0275368471`, `p.value = 0.0498812198`).
- Remaining caveat:
  - Exact numeric value moved versus prior frozen table due Claim 2 harmonization changes; interpretation remains "negative and borderline-significant".

## Claim 2: Cross-wave region harmonization / pooled region FE problem
- Verdict: TRUE
- Evidence checked:
  - `R/20_ingest_data.R` previously passed raw labels through (`coalesce(Region2, Region1)`, `region_name`, raw `region`).
  - `R/40_module_b_determinants.R` uses `region` fixed effects and clusters by `region`.
  - `outputs/tables/tier_a_region_trends.csv` contained inconsistent labels (e.g., `Bukhara oblast`/`Bukhara region`, `Djizak`/`Jizzak`, `Sirdarya`/`Syrdarya`, `Tashkent` variants).
  - `reports/00_main.qmd` previously over-claimed comparability of regional identifiers.
- Fix implemented:
  - Added locked region harmonization map + helper in `R/20_ingest_data.R` (`UZB_REGION_HARMONIZATION_MAP`, `harmonize_uzbekistan_region`).
  - Applied harmonization at ingestion in `as_harmonized_frame()`.
  - Applied the same harmonization in Module C raw preparation (`R/50_module_c_mechanisms.R`) for consistency.
  - Narrowed manuscript language in `reports/00_main.qmd` to state labels are harmonized but regional coverage differs by wave.
- Outputs changed: YES
  - `data/processed/lits_harmonized.csv` regions are canonicalized.
  - `outputs/tables/tier_a_region_trends.csv` now uses a consistent 14-name scheme.
  - Region-FE models (including Module B coefficients) changed slightly due canonicalized FE categories.
- Remaining caveat:
  - Cross-wave regional support is still unbalanced by wave; harmonization solves label comparability, not coverage asymmetry.

## Claim 3: Module C strict <=9 split fragility understated
- Verdict: TRUE
- Evidence checked:
  - Frozen robustness outputs showed extreme coefficients in `weighted_leq9` (e.g., large-magnitude signs in stoppage model).
  - `outputs/tables/module_c_mechanism_robustness_scenarios.csv` showed `n_low_group = 7` vs `n_high_group = 176`.
  - The Module C script (previously `R/50_module_c_policy_did.R`, now `R/50_module_c_mechanisms.R`) lacked explicit support diagnostics/safeguards for sparse-split FE logit scenarios.
- Fix implemented:
  - Added support diagnostics in renamed script `R/50_module_c_mechanisms.R`:
    - `MODULE_C_MIN_GROUP_N = 15`
    - `MODULE_C_MIN_EVENTS_PER_GROUP = 2`
    - `assess_module_c_model_support()`
  - Robustness scenarios with weak support are now flagged and suppressed (`status = "degenerate_support"`, `support_reason` populated) instead of treated as ordinary estimates.
  - Added scenario-level support status fields in `module_c_mechanism_robustness_scenarios.csv`.
  - Strengthened manuscript language in `reports/00_main.qmd` to state strict `<=9` is degenerate support, not an interpretable robustness estimate.
  - Updated `reports/10_technical_appendix.qmd` robustness table to display support diagnostics.
- Outputs changed: YES
  - `outputs/tables/module_c_mechanism_robustness_coefficients.csv` now suppresses strict `<=9` estimates as degenerate support.
  - `outputs/tables/module_c_mechanism_robustness_scenarios.csv` now includes support-status fields.
- Remaining caveat:
  - Module C remains a small-sample descriptive extension; even supported robustness scenarios are not causal evidence.

## Claim 4: Legacy design / stale repo documents
- Verdict: PARTLY TRUE
- Evidence checked:
  - `research_strategy.md` contained stale lines (category-first cross-wave wording and Eq. 6 mismatch with implemented Module C model).
  - `_targets.R` sourced `R/50_module_c_policy_did.R` even though implementation is non-DiD descriptive FE logit.
- Fix implemented:
  - Updated `research_strategy.md`:
    - rank-based cross-wave comparison as primary,
    - Eq. 6 aligned to implemented Module C logit with `parent_low_edu`, `urban`, `gender`, and interaction,
    - archived/non-implemented DiD wording for frozen scope.
  - Renamed `R/50_module_c_policy_did.R` -> `R/50_module_c_mechanisms.R`.
  - Updated `_targets.R` source reference accordingly.
- Outputs changed: INDIRECTLY
  - Naming/docs cleanup plus Module C diagnostics changes; no hidden design switch beyond explicit documented support guard.
- Remaining caveat:
  - Some historical planning documents outside the manuscript (e.g., archival proposal/planning notes) may still mention optional DiD ideas, but active pipeline/manuscript paths are now aligned.

## Regeneration and validation performed
- Rebuilt relevant targets:
  - `lits_harmonized`, `lits_harmonized_csv`
  - `module_a_metrics`, `module_a_files`, `module_a_tier_a`, `module_a_tier_a_files`
  - `module_b_models`, `module_b_files`
  - `module_c_model`, `module_c_files`
  - `empirical_audit`, `empirical_audit_files`
- Re-rendered manuscript dependencies:
  - `reports/00_main.qmd`
  - `reports/10_technical_appendix.qmd`

## Key post-fix checks
- `outputs/tables/module_b_model_coefficients.csv`: Eq. 5 female interaction remains negative and statistically significant at ~5%.
- `outputs/tables/tier_a_region_trends.csv`: region labels canonicalized and consistent.
- `outputs/tables/module_c_mechanism_robustness_coefficients.csv`: strict `<=9` flagged `degenerate_support`, estimates suppressed.
- `outputs/tables/module_c_mechanism_robustness_scenarios.csv`: explicit support threshold/status fields added.
- `reports/00_main.qmd`: text aligned to regenerated outputs on all four claims.
