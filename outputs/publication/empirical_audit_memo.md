# Empirical Audit Memo

Generated on 2026-04-03.

## Scope

- This memo audits empirical sample accounting, denominator differences, model definitions, and parent-measure robustness before any manuscript rewrite.
- The adult LiTS audit uses the harmonized age-eligible sample (ages 25-64). Module C is audited separately because its child-module sample is a different 2022 respondent subset.

## Adult LiTS Sample

- Wave 2010: 1,157 adult respondents in the harmonized analytical sample.
- Wave 2016: 1,240 adult respondents in the harmonized analytical sample.
- Wave 2022: 794 adult respondents in the harmonized analytical sample.

## Parent Availability

- Wave 2010: both parents observed=872, father only=29, mother only=37, neither=219.
- Wave 2016: both parents observed=1,124, father only=17, mother only=45, neither=54.
- Wave 2022: both parents observed=774, father only=1, mother only=4, neither=15.

## Main Checks

- [ok] Wave 2010: own category N=1157, own years N=1157; parent category N=938, parent years N=938.
- [ok] Wave 2010: rank-model N=936 versus category-model N=936.
- [ok] Wave 2016: own category N=1240, own years N=1240; parent category N=1186, parent years N=1186.
- [ok] Wave 2016: rank-model N=1186 versus category-model N=1186.
- [ok] Wave 2022: own category N=794, own years N=794; parent category N=779, parent years N=779.
- [ok] Wave 2022: rank-model N=779 versus category-model N=779.
- [ok] eq2_persistence_trend: own_rank ~ parent_rank + i(wave_year_fe, parent_rank, ref = "2010") + urban + female + migration_exposure + multigenerational_hh | region + cohort + wave_year_fe
- [ok] eq3_attainment_score: own_ed_score ~ parent_ed_score + urban + female + migration_exposure + multigenerational_hh | region + cohort + wave_year_fe
- [ok] eq4_upward_full_lpm: upward_any ~ i(parent_ed_level, ref = "upper_secondary") + urban + female + migration_exposure + multigenerational_hh | region + cohort + wave_year_fe
- [ok] eq4_upward_lowparent_lpm: upward_any ~ i(parent_ed_level, ref = "upper_secondary") + urban + female + migration_exposure + multigenerational_hh | region + cohort + wave_year_fe
- [ok] eq5_persistence_heterogeneity: persist_same ~ parent_ed_score + parent_ed_score:urban + parent_ed_score:female + parent_ed_score:wave2022 + urban + female + migration_exposure + multigenerational_hh | region + cohort + wave_year_fe
- [ok] Eq. 2 parent_rank estimate=0.154, p-value=0.002.
- [caution] Main Module C models are weighted region-fixed-effects logits, so coefficients are on the log-odds scale.
- [caution] Stoppage robustness: 0 of 3 parent_low_edu scenarios significant at 5%; extreme coefficient present=no.
- [caution] Module C uses mean parent years for the low/high split; under the max-parent median split, threshold=16.0, low-group N=183, high-group N=0.
- [ok] Module C parental-schooling sample N=183; main stoppage-model usable N=183.
