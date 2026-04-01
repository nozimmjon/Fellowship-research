from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
OUTPUT_DIR = ROOT / "outputs" / "hbs_expansion_causal"
TABLE_DIR = OUTPUT_DIR / "tables"
PROCESSED_DIR = ROOT / "data" / "processed"
PROGRESS_PATH = ROOT / "HBS_EXPANSION_PROGRESS.md"

ALL_YEARS = [2021, 2022, 2023, 2024, 2025]
INTERPRETABILITY_RANK = {"strong": 3, "moderate": 2, "weak": 1}

CANDIDATE_OUTCOMES = [
    {
        "outcome_name": "current enrollment in any education",
        "variable_name": "current_enrollment_any",
        "metric_type": "binary",
        "source_fields": ["edu_enrolled"],
        "concept_stable": True,
        "interpretability": "moderate",
        "interpretability_note": "Broad participation measure; includes non-tertiary schooling at the lower end of ages 18-24.",
    },
    {
        "outcome_name": "current enrollment in tertiary education",
        "variable_name": "tertiary_enrolled_proxy",
        "metric_type": "binary",
        "source_fields": ["edu_enrolled", "edu_highest"],
        "concept_stable": True,
        "interpretability": "strong",
        "interpretability_note": "Closest match to university access, but still a proxy built from enrollment plus current education track.",
    },
    {
        "outcome_name": "tertiary completion",
        "variable_name": "tertiary_completed_proxy",
        "metric_type": "binary",
        "source_fields": ["edu_enrolled", "edu_highest", "edu_complete"],
        "concept_stable": False,
        "interpretability": "weak",
        "interpretability_note": "Poor fit for ages 18-24 and `edu_complete` only appears in 2024-2025.",
    },
    {
        "outcome_name": "years of schooling",
        "variable_name": "education_years",
        "metric_type": "continuous",
        "source_fields": ["edu_years"],
        "concept_stable": True,
        "interpretability": "weak",
        "interpretability_note": "Observed consistently, but many ages 18-24 are still accumulating schooling so it is a noisy access outcome.",
    },
    {
        "outcome_name": "completed upper secondary or more",
        "variable_name": "completed_upper_secondary_or_more",
        "metric_type": "binary",
        "source_fields": ["edu_years"],
        "concept_stable": True,
        "interpretability": "moderate",
        "interpretability_note": "Stable broad attainment threshold, but wider than university expansion.",
    },
    {
        "outcome_name": "post-secondary / tertiary entry proxy",
        "variable_name": "post_secondary_entry_proxy",
        "metric_type": "binary",
        "source_fields": ["edu_enrolled", "edu_years"],
        "concept_stable": True,
        "interpretability": "strong",
        "interpretability_note": "Broad proxy for post-secondary participation after upper secondary completion.",
    },
    {
        "outcome_name": "upward mobility proxy",
        "variable_name": "upward_mobility_proxy",
        "metric_type": "binary",
        "source_fields": ["edu_years", "relationship"],
        "concept_stable": False,
        "interpretability": "weak",
        "interpretability_note": "Requires co-resident parent linkage and is not mature for ages 18-24.",
    },
]


def fmt_pct(value: float | int | None, digits: int = 1) -> str:
    if value is None or pd.isna(value):
        return "NA"
    return f"{100 * float(value):.{digits}f}%"


def fmt_num(value: float | int | None, digits: int = 2) -> str:
    if value is None or pd.isna(value):
        return "NA"
    return f"{float(value):.{digits}f}"


def weighted_mean(series: pd.Series, weights: pd.Series | None = None) -> float:
    x = pd.to_numeric(series, errors="coerce")
    keep = x.notna()
    if not keep.any():
        return np.nan
    if weights is None:
        return float(x[keep].mean())
    w = pd.to_numeric(weights, errors="coerce").copy()
    w.loc[keep & (w.isna() | (w <= 0))] = 1.0
    return float(np.average(x[keep], weights=w[keep]))


def weighted_share(series: pd.Series, weights: pd.Series | None = None) -> float:
    return weighted_mean(series.astype(float), weights)


def years_text(years: list[int]) -> str:
    if not years:
        return "none"
    return ", ".join(str(int(y)) for y in years)


def update_progress(note: str) -> None:
    if not PROGRESS_PATH.exists():
        return
    text = PROGRESS_PATH.read_text(encoding="utf-8").rstrip()
    stage_header = "## Stage 7: Outcome harmonization audit"
    stage_block = "\n".join(
        [
            stage_header,
            "",
            "**Completed stage**",
            "",
            "- Outcome harmonization audit",
            "",
            "**Key findings**",
            "",
            f"- {note}",
            "",
            "**Blockers**",
            "",
            "- No candidate outcome produced a repeated multi-year working sample under the chosen no-migration restriction.",
            "",
            "**Next action**",
            "",
            "- Keep Model B and Model C deferred unless an upstream linkage or migration harmonization pass restores repeated-year support.",
        ]
    )
    if stage_header in text:
        text = text.split(stage_header)[0].rstrip()
    PROGRESS_PATH.write_text(text + "\n\n" + stage_block + "\n", encoding="utf-8")


def main() -> None:
    merged_path = PROCESSED_DIR / "hbs_expansion_merged.parquet"
    inventory_path = OUTPUT_DIR / "hbs_5y_variable_inventory.csv"
    model_status_path = TABLE_DIR / "model_status.csv"

    if not merged_path.exists() or not inventory_path.exists() or not model_status_path.exists():
        raise FileNotFoundError("Merged parquet, variable inventory, or model status is missing.")

    df = pd.read_parquet(merged_path)
    model_status = pd.read_csv(model_status_path)
    inventory = pd.read_csv(inventory_path)

    model_a = model_status.loc[model_status["model"] == "Model A"]
    if model_a.empty:
        raise ValueError("Model A row missing from model_status.csv.")

    preferred_window = str(model_a["preferred_window"].iloc[0])
    chosen_sample = str(model_a["chosen_sample"].iloc[0])

    df = df.copy()
    df["sample_weight"] = pd.to_numeric(df["sample_weight"], errors="coerce").fillna(1.0)
    df.loc[df["sample_weight"] <= 0, "sample_weight"] = 1.0
    df["current_enrollment_any"] = pd.to_numeric(df["currently_enrolled"], errors="coerce")
    df["completed_upper_secondary_or_more"] = np.where(
        pd.to_numeric(df["education_years"], errors="coerce").notna(),
        (pd.to_numeric(df["education_years"], errors="coerce") >= 11).astype(float),
        np.nan,
    )
    df["post_secondary_entry_proxy"] = np.where(
        pd.to_numeric(df["education_years"], errors="coerce").notna() & pd.to_numeric(df["currently_enrolled"], errors="coerce").notna(),
        (
            (pd.to_numeric(df["education_years"], errors="coerce") >= 11)
            & (pd.to_numeric(df["currently_enrolled"], errors="coerce") == 1)
        ).astype(float),
        np.nan,
    )

    sample_stage_defs = {
        "all_18_24": df.loc[df["age"].between(18, 24)].copy(),
        "linked_18_24": df.loc[df["age"].between(18, 24) & (df["linked_under_rule"] == 1)].copy(),
        "linked_18_24_low_parent": df.loc[
            df["age"].between(18, 24) & (df["linked_under_rule"] == 1) & df["low_parent_education"].notna()
        ].copy(),
        "linked_18_24_no_migration_signal": df.loc[
            df["age"].between(18, 24)
            & (df["linked_under_rule"] == 1)
            & df["low_parent_education"].notna()
            & (df["person_migration_signal"].fillna(0) == 0)
        ].copy(),
    }

    sample_year_trace_rows = []
    for sample_stage, frame in sample_stage_defs.items():
        counts = frame.groupby("survey_year").size().to_dict()
        for year in ALL_YEARS:
            sample_year_trace_rows.append(
                {
                    "sample_stage": sample_stage,
                    "survey_year": year,
                    "sample_n": int(counts.get(year, 0)),
                }
            )
    sample_year_trace = pd.DataFrame(sample_year_trace_rows)

    analysis_samples = {
        "full_linked": sample_stage_defs["linked_18_24_low_parent"].copy(),
        "no_migration_signal": sample_stage_defs["linked_18_24_no_migration_signal"].copy(),
    }

    year_audit_rows = []
    summary_rows = []

    for sample_label, sample_df in analysis_samples.items():
        years_in_sample = sorted(int(y) for y in sample_df["survey_year"].dropna().unique())
        for outcome in CANDIDATE_OUTCOMES:
            outcome_var = outcome["variable_name"]
            years_with_non_missing: list[int] = []
            years_with_sample: list[int] = []

            for year in ALL_YEARS:
                year_df = sample_df.loc[sample_df["survey_year"] == year].copy()
                sample_n = int(len(year_df))
                if sample_n > 0:
                    years_with_sample.append(year)

                outcome_vals = pd.to_numeric(year_df[outcome_var], errors="coerce") if sample_n > 0 else pd.Series(dtype=float)
                non_missing_n = int(outcome_vals.notna().sum()) if sample_n > 0 else 0
                if non_missing_n > 0:
                    years_with_non_missing.append(year)

                non_missing_share = (non_missing_n / sample_n) if sample_n > 0 else np.nan
                if outcome["metric_type"] == "binary":
                    weighted_metric = (
                        weighted_share(outcome_vals == 1, year_df["sample_weight"]) if non_missing_n > 0 else np.nan
                    )
                else:
                    weighted_metric = weighted_mean(outcome_vals, year_df["sample_weight"]) if non_missing_n > 0 else np.nan

                year_audit_rows.append(
                    {
                        "sample_label": sample_label,
                        "outcome_name": outcome["outcome_name"],
                        "variable_name": outcome_var,
                        "metric_type": outcome["metric_type"],
                        "survey_year": year,
                        "sample_n": sample_n,
                        "non_missing_n": non_missing_n,
                        "non_missing_share": non_missing_share,
                        "weighted_metric": weighted_metric,
                    }
                )

            source_coverage = inventory.loc[inventory["variable_name"].isin(outcome["source_fields"])].groupby("variable_name")[
                "survey_year"
            ].nunique()
            source_fields_all_5y = all(source_coverage.get(field, 0) == len(ALL_YEARS) for field in outcome["source_fields"])
            interpretable_for_exposed = outcome["interpretability"] in {"strong", "moderate"}
            repeated_year_support = len(years_with_non_missing)
            repeated_year_rescue_pass = (
                sample_label == chosen_sample
                and repeated_year_support >= 3
                and outcome["concept_stable"]
                and interpretable_for_exposed
            )

            summary_rows.append(
                {
                    "sample_label": sample_label,
                    "outcome_name": outcome["outcome_name"],
                    "variable_name": outcome_var,
                    "metric_type": outcome["metric_type"],
                    "source_fields": ", ".join(outcome["source_fields"]),
                    "source_fields_all_5y": source_fields_all_5y,
                    "years_in_sample": years_text(years_in_sample),
                    "years_with_non_missing_outcome": years_text(years_with_non_missing),
                    "years_with_non_missing_n": repeated_year_support,
                    "coding_concept_stable": outcome["concept_stable"],
                    "interpretability_rating": outcome["interpretability"],
                    "interpretable_for_exposed_cohorts": interpretable_for_exposed,
                    "working_sample_rescue_pass": repeated_year_rescue_pass,
                    "interpretability_note": outcome["interpretability_note"],
                }
            )

    outcome_year_audit = pd.DataFrame(year_audit_rows)
    outcome_summary = pd.DataFrame(summary_rows)

    chosen_summary = outcome_summary.loc[outcome_summary["sample_label"] == chosen_sample].copy()
    chosen_year_audit = outcome_year_audit.loc[outcome_year_audit["sample_label"] == chosen_sample].copy()
    rescue_candidates = chosen_summary.loc[chosen_summary["working_sample_rescue_pass"]]

    stable_candidates = chosen_summary.loc[
        chosen_summary["coding_concept_stable"] & chosen_summary["interpretable_for_exposed_cohorts"]
    ].copy()
    stable_candidates["interpretability_score"] = stable_candidates["interpretability_rating"].map(INTERPRETABILITY_RANK).fillna(0)
    stable_candidates = stable_candidates.sort_values(
        ["years_with_non_missing_n", "interpretability_score", "outcome_name"],
        ascending=[False, False, True],
    )

    if stable_candidates.empty:
        best_candidate_name = "none"
        best_candidate_years = "none"
    else:
        best_candidate_name = str(stable_candidates["outcome_name"].iloc[0])
        best_candidate_years = str(stable_candidates["years_with_non_missing_outcome"].iloc[0])

    outcome_year_audit.to_csv(TABLE_DIR / "outcome_candidate_year_audit.csv", index=False)
    outcome_summary.to_csv(TABLE_DIR / "outcome_candidate_summary.csv", index=False)
    sample_year_trace.to_csv(TABLE_DIR / "sample_year_support_trace.csv", index=False)

    note_lines = [
        "# Outcome Harmonization Audit",
        "",
        f"- Target age window: `{preferred_window}`.",
        f"- Working analytical sample: `{chosen_sample}`.",
        "- Candidate outcomes audited: current enrollment in any education; current enrollment in tertiary education; tertiary completion; years of schooling; completed upper secondary or more; post-secondary / tertiary entry proxy; upward mobility proxy.",
        "- Raw education source fields `edu_enrolled`, `edu_highest`, and `edu_years` appear in all five HBS years; `edu_complete` appears only in 2024 and 2025.",
        "",
        f"- In the full linked 18-24 sample, usable linked observations appear in survey years `{years_text(sorted(int(y) for y in analysis_samples['full_linked']['survey_year'].dropna().unique()))}`.",
        f"- In the chosen `{chosen_sample}` sample, usable observations appear in survey years `{years_text(sorted(int(y) for y in analysis_samples[chosen_sample]['survey_year'].dropna().unique()))}`.",
        "- That means no candidate outcome can rescue repeated multi-year support inside the current working sample if the sample itself only appears in one survey year.",
        "",
        (
            f"- Best conceptually aligned candidate after the audit: `{best_candidate_name}`, but it is only supported in `{best_candidate_years}` within the chosen working sample."
            if best_candidate_name != "none"
            else "- No conceptually stable candidate outcome survived the audit in the chosen working sample."
        ),
        (
            "- No candidate outcome passes the repeated-year rescue rule for Model B. The exploratory design therefore remains at Model A only."
            if rescue_candidates.empty
            else "- At least one candidate outcome passes the repeated-year rescue rule. Model B can be reconsidered on that outcome."
        ),
        "",
        "Interpretation: the outcome fields themselves are not the only issue. The current chosen sample collapses to one survey year under the existing linkage and migration restrictions, so an outcome-only rescue does not restore a multi-year quasi-experimental design.",
    ]
    (OUTPUT_DIR / "outcome_harmonization_audit.md").write_text("\n".join(note_lines) + "\n", encoding="utf-8")

    model_b_note = (
        "Not estimated in this run. Outcome-harmonization audit found no repeated multi-year candidate outcome in the no-migration working sample, so Model B remains deferred."
        if rescue_candidates.empty
        else "Not estimated in this run. Outcome-harmonization audit identified a repeated multi-year candidate outcome, so Model B can be reconsidered as the next gated step."
    )
    model_status.loc[model_status["model"] == "Model B", "note"] = model_b_note
    model_status.loc[model_status["model"] == "Model C", "note"] = (
        "Deferred until a repeated multi-year outcome supports Model B on the same restricted sample."
    )
    model_status.to_csv(model_status_path, index=False)

    update_progress(
        "Outcome-harmonization audit found that raw education fields are present across five years, but the chosen no_migration_signal sample still only supports 2025, so no candidate outcome rescues Model B."
    )


if __name__ == "__main__":
    main()
