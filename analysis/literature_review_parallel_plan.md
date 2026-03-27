# Parallel Literature Review Plan (Academic Pipeline Skills)

## Project Snapshot

-   Frozen question: intergenerational educational mobility in Uzbekistan (2010, 2016, 2022-23).
-   Core design already locked in:
    -   `01_question_and_contribution.md`
    -   `research_strategy.md`
    -   `04_analysis_plan.md`
-   Existing data context already present:
    -   `data/raw/lits/` (LiTS waves + questionnaires/technical docs)
    -   `data/metadata/exports/` (audit + variable crosswalk exports)

## Objective of This Plan

Build a publishable literature review that supports Modules A/B/C without reopening the locked identification language and design.

## Parallel Workstreams

## Stream A: Evidence Search and Screening

-   Skill: `deep-research` in `systematic-review` mode.
-   Goal: build a transparent longlist and screened shortlist of relevant papers/reports.
-   Inputs:
    -   frozen question and scope from `research_strategy.md`
    -   region focus: Uzbekistan + comparator transition economies
    -   timeline focus: 2010-2026
-   Outputs:
    -   `outputs/tables/lit_a_search_log.csv`
    -   `outputs/tables/lit_a_screened_studies.csv`
    -   `outputs/tables/lit_a_prisma_counts.csv`

## Stream B: Concept and Measure Mapping

-   Skill: `deep-research` in `lit-review` mode.
-   Goal: map how the literature operationalizes persistence/mobility and align with locked measure set.
-   Inputs:
    -   `data/metadata/mobility_measure_set.csv`
    -   `data/metadata/mobility_variable_lock.csv`
-   Outputs:
    -   `outputs/tables/lit_b_measure_mapping.csv`
    -   `outputs/tables/lit_b_gaps_vs_project_design.csv`

## Stream C: Methods and Quality Appraisal

-   Skill: `deep-research` in `review` mode.
-   Goal: extract identification quality, data limitations, and external validity risks.
-   Inputs:
    -   Stream A screened shortlist
    -   your non-causal language lock for Modules A/B
-   Outputs:
    -   `outputs/tables/lit_c_quality_rubric.csv`
    -   `outputs/tables/lit_c_identification_risks.csv`

## Stream D: Narrative Synthesis Drafting

-   Skill: `academic-paper` in `plan` then `full` mode.
-   Goal: turn Streams A-C into a literature review section for main report + appendix note.
-   Inputs:
    -   outputs from Streams A/B/C
    -   section structure from `reports/00_main.qmd`
-   Outputs:
    -   `outputs/publication/literature_review_section.md`
    -   `outputs/publication/literature_review_appendix_note.md`

## Stream E: Review and Tightening

-   Skill: `academic-paper-reviewer` in `methodology-focus` and `quick` modes.
-   Goal: stress test overclaim risk and citation/logic coherence before merge.
-   Inputs:
    -   Stream D drafts
    -   design lock from `04_analysis_plan.md`
-   Outputs:
    -   `outputs/tables/lit_e_reviewer_findings.md`
    -   `outputs/tables/lit_e_revision_checklist.csv`

## Orchestration

-   Skill: `academic-pipeline` as checkpoint manager.
-   Use it to enforce stage gates and handoff quality:
    1.  Approve Streams A-C artifacts
    2.  Approve Stream D narrative
    3.  Approve Stream E fixes before merge

## Execution Order (Parallel + Dependencies)

1.  Run Streams A, B, C in parallel.
2.  Start Stream D after first complete pass of A-C outputs.
3.  Run Stream E after Stream D first draft.
4.  Merge reviewed literature section into:
    -   `reports/00_main.qmd` (main narrative)
    -   `reports/10_technical_appendix.qmd` (methods/quality note)

## Ready-to-Use Prompts

1.  Stream A prompt:
    -   "Use deep-research in systematic-review mode for intergenerational educational mobility in Uzbekistan (2010-2026). Produce search log, screened studies list, and PRISMA counts. Respect non-causal framing."
2.  Stream B prompt:
    -   "Use deep-research in lit-review mode to map mobility measures in the literature to my locked measures in data/metadata/mobility_measure_set.csv and identify gaps."
3.  Stream C prompt:
    -   "Use deep-research in review mode to score methodological quality and identification risks for the screened studies. Emphasize what supports descriptive/associational claims only."
4.  Stream D prompt:
    -   "Use academic-paper in plan/full mode to draft a literature review section aligned to research_strategy.md and 04_analysis_plan.md; output concise synthesis and unresolved debates."
5.  Stream E prompt:
    -   "Use academic-paper-reviewer in methodology-focus mode, then quick mode, to critique overclaim risk and coherence of the literature review draft and return a revision checklist."

## Definition of Done

-   Literature review has:
    -   transparent screening trail
    -   explicit measure-to-measure mapping
    -   quality/risk table
    -   synthesis text consistent with non-causal language lock
    -   reviewer checklist resolved
