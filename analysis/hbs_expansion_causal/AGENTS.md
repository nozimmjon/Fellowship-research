# AGENTS.md

## Module Rules
- The main LiTS paper is frozen and must not be altered for this module.
- Do not change `reports/00_main.qmd` or introduce causal claims into the main paper.
- The existing HBS supplementary-context workflow in `R/31_build_hbs_household_context.R`, `R/32_build_hbs_linkage_diagnostics.R`, and `R/33_estimate_hbs_appendix_models.R` remains valid and must not be overwritten.
- This HBS university-expansion module is exploratory until diagnostics pass.
- Do not estimate exploratory causal models until linkage, cohort timing, outcome maturity, merge coverage, and region-exposure checks are all reviewed.
- No main-text causal claims should be introduced at this stage.
- All module outputs belong under `outputs/hbs_expansion_causal/`.
- Use dedicated processed-file names beginning with `hbs_expansion_` so this module does not collide with older HBS files.
- Prefer the canonical admin filenames requested by the user. If `data/processed/admin/` is absent, use the same-named local admin files in `data/raw/admin/` and document that path drift explicitly.
- `HBS_EXPANSION_PROGRESS.md` must be updated after each major stage.
- Keep prose concise, formal, and explicitly conditional when discussing any exploratory estimates.
