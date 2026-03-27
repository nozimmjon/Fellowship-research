# Tight Literature Review Plan

## Purpose
- Support the locked paper, not the earlier broader proposal.
- Keep the review narrowly focused on what the paper needs to argue credibly.
- Preserve the current design lock:
  - Module A: descriptive mobility levels and trends
  - Module B: descriptive/associational correlates
  - Module C: short 2022-23 mechanism extension with suggestive, non-causal language

## What the Literature Review Must Do
1. Position the paper in the intergenerational mobility literature.
2. Justify the locked mobility measures used in the paper.
3. Explain why the paper's claims are descriptive or associational rather than causal.
4. Show the paper's contribution for Uzbekistan and comparable transition-economy settings.

## Scope Boundaries
- Do not turn the review into a standalone systematic-review project.
- Do not reopen the empirical design or reintroduce DiD as a default paper architecture.
- Do not promise PRISMA-style production unless full systematic screening is actually performed.
- Keep artifacts light and directly usable in the paper and appendix.

## Stream 1: Scoped Search and Screening
- Skill: `deep-research`
- Goal: build a transparent, tightly screened bibliography for the locked paper.
- Search buckets only:
  - intergenerational educational mobility
  - transition or post-socialist mobility evidence
  - Central Asia or Uzbekistan education inequality and mobility context
  - pandemic learning disruption as limited background for Module C
- Main output files:
  - `outputs/tables/literature_screened_bibliography.csv`
  - `outputs/tables/literature_search_log.csv`
- Required columns for bibliography table:
  - `citation_key`
  - `study`
  - `country_or_region`
  - `data`
  - `topic_bucket`
  - `core_relevance`
  - `keep_for_main_text`
  - `note`

## Stream 2: Concept and Measure Mapping
- Skill: `deep-research`
- Goal: connect the literature directly to the locked measure set and design choices.
- Core inputs:
  - `data/metadata/mobility_measure_set.csv`
  - `data/metadata/mobility_variable_lock.csv`
  - `research_strategy.md`
- Main output files:
  - `outputs/tables/literature_measure_mapping.csv`
  - `outputs/tables/literature_design_positioning.csv`
- Required questions:
  - How do comparable studies measure persistence and mobility?
  - Why do rank-rank slope, transition matrices, upward mobility, downward mobility, and persistence fit this paper?
  - What does the literature support as descriptive evidence versus causal evidence?
  - Where does Uzbekistan remain under-documented relative to comparator settings?

## Stream 3: Narrative Synthesis
- Skill: `academic-paper`
- Goal: draft a concise literature section for the main report plus a short appendix note on scope and identification limits.
- Main output files:
  - `outputs/publication/literature_review_section.md`
  - `outputs/publication/literature_limits_note.md`
- Main-text section jobs:
  - position the paper in the literature
  - motivate the measure set
  - frame the non-causal design honestly
  - state the Uzbekistan-specific contribution clearly
- Appendix note jobs:
  - summarize data and identification limits in the literature
  - explain why this paper avoids stronger causal claims
  - place Module C as a bounded suggestive extension

## Lightweight Evidence Table
- Collapse quality appraisal into one appendix-ready table instead of a separate review pipeline.
- Output file:
  - `outputs/tables/literature_evidence_table.csv`
- Suggested columns:
  - `study`
  - `data`
  - `identification_type`
  - `relevance_to_module`
  - `main_limitation`
  - `use_in_paper`

## Execution Order
1. Run Stream 1 and Stream 2 in parallel.
2. Use their outputs to draft Stream 3.
3. Merge the synthesis into the main report and technical appendix after empirical language is harmonized.

## Immediate Repo Prerequisite
- Harmonize `reports/10_technical_appendix.qmd` with the locked design before drafting the literature section.
- Specifically:
  - remove event-study DiD language as the default Module C description
  - remove placebo-lead and region-trend checks as baseline expectations
  - replace them with mechanism-sample, balance, subgroup, and robustness language appropriate to the current locked design

## Definition of Done
- The literature review is tight enough to support the locked paper.
- The review does not overpromise causal contribution.
- The appendix and literature framing point in the same direction as the frozen strategy.
