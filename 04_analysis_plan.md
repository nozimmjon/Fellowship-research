# 04 Analysis Plan (Step 3 Locked)

## Paper Goal (Frozen)
"How persistent is educational attainment across generations in Uzbekistan, how has that persistence changed from 2010 to 2022-23, and which household and regional factors are associated with higher mobility?"

## Module A Fixed Measure Set
1. `rank_rank_slope`: respondent schooling rank on parental schooling rank.
2. `transition_matrix_share`: child education distribution conditional on parental education category.
3. `upward_mobility_rate`: share with child education category above parent category.
4. `downward_mobility_rate`: share with child education category below parent category.
5. `persistence_probability`: share with child education category equal to parent category.

## Subgroup Splits (Pre-committed)
- Overall
- Urban/rural
- Gender
- Region
- Cohort

## Minimum-N Rule
- Any rate or slope estimate with fewer than 30 valid observations is flagged `small_n`.
- For `small_n`, estimate is set to missing (`NA`) in outputs.

## Locked Variable Definitions
- Canonical variable definitions are fixed in:
  - `data/metadata/mobility_variable_lock.csv`
- Fixed metric definitions are fixed in:
  - `data/metadata/mobility_measure_set.csv`

## Output Files
- `outputs/tables/module_a_summary_metrics.csv`
- `outputs/tables/module_a_subgroup_metrics.csv`
- `outputs/tables/module_a_transition_matrix.csv`
- `outputs/tables/module_a_persistence_by_parent.csv`
