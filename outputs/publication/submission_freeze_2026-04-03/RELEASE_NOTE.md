# Release Note

## Submission Freeze

Date: 2026-04-04  
Branch: `codex/empirical-audit`  
Base commit at freeze creation: `a83a630`

## What This Version Includes

- Pass 1 computation audit completed and clean.
- Upstream ingest patched so nonpositive sample weights are converted to missing before writing the harmonized LiTS file.
- Focused methodology revision completed without expanding the paper beyond its descriptive and associational scope.
- Main paper, technical appendix, policy brief, and slides aligned to the same disciplined timing claim.
- Policy brief completed a final editorial pass for presentation and handoff.
- Submission bundle created with a manifest and portable slide assets.

## Core Substantive Message

Educational persistence strengthens clearly from 2010 to 2016, but the evidence after 2016 is flatter and less precise; the paper is descriptive and associational, not causal.

## What Changed Most in This Release

- The paper now treats parental-education missingness explicitly rather than implicitly.
- Module B is framed and estimated as nested associational specifications, with potentially endogenous controls handled as sensitivity layers.
- Formal cross-wave tests now discipline the timing language in the paper and presentation materials.
- The policy brief is sharper and more policy-facing while keeping the same evidence and caveats.

## Included Deliverables

- `Uzbekistan_Educational_Mobility_Main_Paper.docx`
- `Uzbekistan_Educational_Mobility_Technical_Appendix.docx`
- `Uzbekistan_Educational_Mobility_Policy_Brief.docx`
- `Uzbekistan_Educational_Mobility_Slides.html`
- `slides_assets/`
- `slides_figures/`
- `submission_manifest.csv`

## Remaining Limits

- Repeated cross-sections rather than a panel.
- Earlier-wave parental-education missingness still affects level precision.
- Some 2022-23 subgroup and regional comparisons remain noisy.
- Inference uses region-clustered standard errors without an extra small-sample correction.
- Module C remains a bounded non-causal extension.
