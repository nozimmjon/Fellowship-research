# HBS Expansion Progress

This log tracks the exploratory HBS university-expansion module.

## Stage 1: HBS file audit

**Completed stage**

- HBS file audit

**Key findings**

- Detected HBS extracts for survey years 2021, 2022, 2023, 2024, 2025. The raw five-year audit has been written to the module manifest and variable inventory.

**Blockers**

- None at this stage.

**Next action**

- Construct the new design-specific person-level harmonized HBS file and linkage diagnostics.

## Stage 2: Person-level harmonization

**Completed stage**

- Person-level harmonization

**Key findings**

- Built a harmonized HBS person file with 367,127 person-year observations and conservative co-resident parent-linkage fields.

**Blockers**

- None at this stage.

**Next action**

- Compute the new younger-cohort linkage diagnostics and failure-mode audit.

## Stage 3: Linkage diagnostics

**Completed stage**

- Linkage diagnostics

**Key findings**

- Computed dedicated linkage diagnostics for ages 18-24, 22-30, and 25-35. The preliminary leading window is 18_24, with an old pooled 25-64 comparison rate of 9.3%.

**Blockers**

- None at this stage.

**Next action**

- Merge the canonical admin treatment panel and evaluate cohort timing, merge coverage, and outcome readiness.

## Stage 4: Residence-stability readiness pass

**Completed stage**

- Residence-stability readiness pass

**Key findings**

- Compared full linked, no migration-signal, and conservative likely-stayer samples for the 18_24 window. The binding constraint is none, and the rescued sample is no_migration_signal.

**Blockers**

- None at this stage.

**Next action**

- Unlock Model A only if a residence-stability restriction passes the diagnostic gate; otherwise keep the design note negative.

## Stage 5: Exploratory estimation decision

**Completed stage**

- Exploratory estimation decision

**Key findings**

- The residence-stability pass rescued the exposure-region proxy enough to unlock Model A only. Model B and Model C remain deferred.

**Blockers**

- None at this stage.

**Next action**

- Render the separate HBS expansion design note and verify the final output set.

## Stage 6: Model A review gate

**Completed stage**

- Model A review gate

**Key findings**

- Reviewed Model A on the rescued sample no_migration_signal. Model B unlock decision: no.

**Blockers**

- At least one Model A review check still failed.

**Next action**

- Hold the module at Model A only until the failed review checks are resolved.

## Stage 7: Outcome harmonization audit

**Completed stage**

- Outcome harmonization audit

**Key findings**

- Outcome-harmonization audit found that raw education fields are present across five years, but the chosen no_migration_signal sample still only supports 2025, so no candidate outcome rescues Model B.

**Blockers**

- No candidate outcome produced a repeated multi-year working sample under the chosen no-migration restriction.

**Next action**

- Keep Model B and Model C deferred unless an upstream linkage or migration harmonization pass restores repeated-year support.
