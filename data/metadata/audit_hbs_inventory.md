# HBS Data Audit (one-page inventory)

Audit date: 2026-03-26

## Coverage

- HBS files found: 116
- Years detected: 2021, 2022, 2023, 2024, 2025

## Required Checks

- parent-child links: partially supported (household member IDs and relationship fields exist (hhid, iid, relationship, plus parent/proxy ID fields). Parent-child links likely reconstructable, but not guaranteed direct in all years.)
- education variable consistency: partially supported (All years contain: edu_highest, edu_years, edu_enrolled; edu_grade is not present in all years.)
- region availability: supported (Detected geographic fields: province, mahalla, soato_hhid, urban)
- household income/welfare availability: supported (Income/consumption-related modules present (e.g., nonwage, food, nonfood/cost modules).)
- multiple generations observable: supported (Roster has age + relationship + household/member IDs; multigenerational composition can be constructed.)
- migration variables: supported (Detected migration fields include: emig, immig, emig_remit, emig_country, immig_country)
- sampling weights: supported (Detected weight fields: popw, uwgt, indw, strata)

## Comparability Across Rounds

- Key modules (`m01_roster`, `m02_migration`, `m03_education`, `m00_passport`, `m00_weight`) exist in all detected years.
- Education core variables (`edu_highest`, `edu_years`, `edu_enrolled`) are present across years in `m03_education`; `edu_grade` appears only in later rounds.
- Questionnaire structure changes are visible in spending/nonfood modules by 2025; harmonization should use module-aware rules rather than assuming identical file names.

## Conclusion

- HBS is suitable for contextual welfare/household structure/migration analysis and regional background controls.
- Parent-child links are likely reconstructable via household roster relationships, but this should be validated with explicit parent identifier logic before using HBS for core intergenerational mobility estimation.
