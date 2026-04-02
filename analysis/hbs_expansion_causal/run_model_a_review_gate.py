from __future__ import annotations

from pathlib import Path

import numpy as np
import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
OUTPUT_DIR = ROOT / "outputs" / "hbs_expansion_causal"
TABLE_DIR = OUTPUT_DIR / "tables"
PROCESSED_DIR = ROOT / "data" / "processed"
PROGRESS_PATH = ROOT / "HBS_EXPANSION_PROGRESS.md"

AGE_WINDOWS = {
    "18_24": (18, 24),
    "22_30": (22, 30),
    "25_35": (25, 35),
}

OUTCOME_MAP = {
    "tertiary enrollment": "tertiary_enrolled_proxy",
    "tertiary completion": "tertiary_completed_proxy",
    "upward mobility proxy": "upward_mobility_proxy",
}


def load_latest_merged_df() -> pd.DataFrame:
    parquet_path = PROCESSED_DIR / "hbs_expansion_merged.parquet"
    tmp_csv_path = TABLE_DIR / "_tmp_hbs_expansion_merged.csv"

    if tmp_csv_path.exists() and (
        not parquet_path.exists() or tmp_csv_path.stat().st_mtime > parquet_path.stat().st_mtime
    ):
        return pd.read_csv(tmp_csv_path)
    if parquet_path.exists():
        return pd.read_parquet(parquet_path)
    raise FileNotFoundError("Merged analysis dataset was missing in both parquet and temporary CSV form.")


def fmt_pct(value: float | int | None, digits: int = 1) -> str:
    if value is None or pd.isna(value):
        return "NA"
    return f"{100 * float(value):.{digits}f}%"


def fmt_num(value: float | int | None, digits: int = 3) -> str:
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


def weighted_group_shares(df: pd.DataFrame, group_var: str, weight_var: str = "sample_weight") -> pd.DataFrame:
    if df.empty:
        return pd.DataFrame(columns=["group", "share"])
    out = df[[group_var, weight_var]].copy()
    out = out.rename(columns={group_var: "group", weight_var: "weight"})
    out["weight"] = pd.to_numeric(out["weight"], errors="coerce").fillna(1.0)
    out.loc[out["weight"] <= 0, "weight"] = 1.0
    out = out[out["group"].notna() & (out["group"].astype(str).str.strip() != "")]
    if out.empty:
        return pd.DataFrame(columns=["group", "share"])
    out = out.groupby("group", as_index=False)["weight"].sum()
    total_weight = out["weight"].sum()
    out["share"] = np.where(total_weight > 0, out["weight"] / total_weight, np.nan)
    return out[["group", "share"]]


def ntile(series: pd.Series, n: int) -> pd.Series:
    ranked = series.rank(method="first")
    return np.ceil(ranked * n / ranked.count()).astype("Int64")


def build_exposure_gap_metrics(df: pd.DataFrame, outcome_var: str) -> dict[str, float]:
    if df.empty or df["exposure_index_17_20"].isna().all():
        return {
            "sample_n": int(len(df)),
            "quartile_1_rate": np.nan,
            "quartile_4_rate": np.nan,
            "top_bottom_gap": np.nan,
            "top_half_bottom_half_gap": np.nan,
        }

    exposure_df = df.loc[df["exposure_index_17_20"].notna()].copy()
    exposure_df["exposure_bin"] = ntile(exposure_df["exposure_index_17_20"], 4)

    def rate_for_mask(mask: pd.Series) -> float:
        subset = exposure_df.loc[mask]
        if subset.empty:
            return np.nan
        return weighted_share(subset[outcome_var] == 1, subset["sample_weight"])

    q1_rate = rate_for_mask(exposure_df["exposure_bin"] == 1)
    q4_rate = rate_for_mask(exposure_df["exposure_bin"] == 4)
    bottom_half_rate = rate_for_mask(exposure_df["exposure_bin"].isin([1, 2]))
    top_half_rate = rate_for_mask(exposure_df["exposure_bin"].isin([3, 4]))

    return {
        "sample_n": int(len(exposure_df)),
        "quartile_1_rate": q1_rate,
        "quartile_4_rate": q4_rate,
        "top_bottom_gap": q4_rate - q1_rate if pd.notna(q4_rate) and pd.notna(q1_rate) else np.nan,
        "top_half_bottom_half_gap": (
            top_half_rate - bottom_half_rate
            if pd.notna(top_half_rate) and pd.notna(bottom_half_rate)
            else np.nan
        ),
    }


def build_analysis_base_df(df: pd.DataFrame, preferred_window: str, outcome_var: str | None, require_outcome: bool) -> pd.DataFrame:
    age_min, age_max = AGE_WINDOWS[preferred_window]
    out = df.loc[
        df["age"].notna()
        & (df["age"] >= age_min)
        & (df["age"] <= age_max)
        & (df["linked_under_rule"] == 1)
        & df["admin_region"].notna()
        & df["low_parent_education"].notna()
    ].copy()
    if require_outcome and outcome_var is not None:
        out = out.loc[out[outcome_var].notna()].copy()
    return out


def get_residence_sample_df(df: pd.DataFrame, sample_label: str) -> pd.DataFrame:
    if sample_label == "full_linked":
        return df.copy()
    if sample_label == "no_migration_signal":
        return df.loc[df["person_migration_signal"].fillna(0) == 0].copy()
    if sample_label == "conservative_likely_stayer":
        return df.loc[
            (df["explicit_residence_history_signal"].fillna(0) == 0)
            & (df["household_migration_signal"].fillna(0) == 0)
        ].copy()
    return df.iloc[0:0].copy()


def write_empty_outputs(note_text: str) -> None:
    pd.DataFrame(columns=["check_name", "metric_summary", "pass"]).to_csv(TABLE_DIR / "model_b_gate_status.csv", index=False)
    for name in [
        "model_a_direction_review.csv",
        "model_a_region_sensitivity.csv",
        "model_a_cell_balance_summary.csv",
        "model_a_cell_counts.csv",
        "model_a_year_stability.csv",
        "model_a_sample_profile_comparison.csv",
    ]:
        pd.DataFrame().to_csv(TABLE_DIR / name, index=False)
    (OUTPUT_DIR / "model_a_review_note.md").write_text(f"# Model A Review Note\n\n{note_text}\n", encoding="utf-8")


def update_progress(model_b_gate_pass: bool, chosen_sample: str) -> None:
    if not PROGRESS_PATH.exists():
        return
    stage_header = "## Stage 6: Model A review gate"
    text = PROGRESS_PATH.read_text(encoding="utf-8")
    stage_block = "\n".join(
        [
            stage_header,
            "",
            "**Completed stage**",
            "",
            "- Model A review gate",
            "",
            "**Key findings**",
            "",
            f"- Reviewed Model A on the rescued sample {chosen_sample}. Model B unlock decision: {'yes' if model_b_gate_pass else 'no'}.",
            "",
            "**Blockers**",
            "",
            f"- {'None at this stage.' if model_b_gate_pass else 'At least one Model A review check still failed.'}",
            "",
            "**Next action**",
            "",
            f"- {'Keep Model B as the next gated step on the same no-migration-signal sample, but do not estimate it yet.' if model_b_gate_pass else 'Hold the module at Model A only until the failed review checks are resolved.'}",
            "",
        ]
    )
    if stage_header in text:
        text = text.split(stage_header)[0].rstrip() + "\n\n" + stage_block + "\n"
    else:
        text = text.rstrip() + "\n\n" + stage_block + "\n"
    PROGRESS_PATH.write_text(text, encoding="utf-8")


def main() -> None:
    model_status_path = TABLE_DIR / "model_status.csv"

    if not model_status_path.exists():
        write_empty_outputs("Model A review gate not run because the required model-status file was missing.")
        return

    model_status = pd.read_csv(model_status_path)
    model_a_row = model_status.loc[model_status["model"] == "Model A"]
    if model_a_row.empty or not bool(model_a_row["estimated"].iloc[0]):
        write_empty_outputs("Model A review gate not run because Model A was not available from the prior readiness pass.")
        return

    preferred_window = str(model_a_row["preferred_window"].iloc[0])
    preferred_outcome = str(model_a_row["preferred_outcome"].iloc[0])
    chosen_sample = str(model_a_row["chosen_sample"].iloc[0])
    outcome_var = OUTCOME_MAP.get(preferred_outcome)

    if preferred_window not in AGE_WINDOWS or outcome_var is None:
        write_empty_outputs("Model A review gate not run because the preferred window or preferred outcome could not be interpreted.")
        return

    try:
        df = load_latest_merged_df()
    except FileNotFoundError:
        write_empty_outputs("Model A review gate not run because the merged analysis dataset was missing.")
        return
    full_linked_complete = build_analysis_base_df(df, preferred_window, outcome_var, True)
    chosen_complete = get_residence_sample_df(full_linked_complete, chosen_sample)
    full_linked_prefilter = build_analysis_base_df(df, preferred_window, outcome_var, False)
    chosen_prefilter = get_residence_sample_df(full_linked_prefilter, chosen_sample)

    if chosen_complete.empty:
        write_empty_outputs("Model A review gate not run because the chosen analytical sample had no complete-case observations for the preferred outcome.")
        return

    for frame in [full_linked_complete, chosen_complete, chosen_prefilter]:
        frame["sample_weight"] = pd.to_numeric(frame["sample_weight"], errors="coerce").fillna(1.0)
        frame.loc[frame["sample_weight"] <= 0, "sample_weight"] = 1.0

    direction_groups = {
        "overall": chosen_complete.loc[chosen_complete["exposed_cohort_any_overlap"] == 1].copy(),
        "lower_parent_education": chosen_complete.loc[
            (chosen_complete["exposed_cohort_any_overlap"] == 1) & (chosen_complete["low_parent_education"] == 1)
        ].copy(),
        "higher_parent_education": chosen_complete.loc[
            (chosen_complete["exposed_cohort_any_overlap"] == 1) & (chosen_complete["low_parent_education"] == 0)
        ].copy(),
        "full_overlap_only": chosen_complete.loc[chosen_complete["exposed_cohort_full_overlap"] == 1].copy(),
    }

    direction_frames: list[pd.DataFrame] = []
    for review_group, group_df in direction_groups.items():
        if group_df.empty or group_df["exposure_index_17_20"].isna().all():
            continue
        group_df["exposure_bin"] = ntile(group_df["exposure_index_17_20"], 4)
        out = (
            group_df.groupby("exposure_bin", dropna=False)
            .apply(
                lambda x: pd.Series(
                    {
                        "sample_n": int(len(x)),
                        "weighted_mean": weighted_share(x[outcome_var] == 1, x["sample_weight"]),
                    }
                )
            )
            .reset_index()
        )
        out.insert(0, "review_group", review_group)
        direction_frames.append(out)
    direction_review = pd.concat(direction_frames, ignore_index=True) if direction_frames else pd.DataFrame(
        columns=["review_group", "exposure_bin", "sample_n", "weighted_mean"]
    )

    overall_gap = build_exposure_gap_metrics(chosen_complete.loc[chosen_complete["exposed_cohort_any_overlap"] == 1], outcome_var)
    full_overlap_gap = build_exposure_gap_metrics(chosen_complete.loc[chosen_complete["exposed_cohort_full_overlap"] == 1], outcome_var)
    direction_pass = (
        pd.notna(overall_gap["top_bottom_gap"])
        and overall_gap["top_bottom_gap"] >= 0
        and pd.notna(overall_gap["top_half_bottom_half_gap"])
        and overall_gap["top_half_bottom_half_gap"] >= 0
        and (pd.isna(full_overlap_gap["top_bottom_gap"]) or full_overlap_gap["top_bottom_gap"] >= -0.01)
    )

    region_rows = [dict(excluded_region="all_regions", **build_exposure_gap_metrics(
        chosen_complete.loc[chosen_complete["exposed_cohort_any_overlap"] == 1], outcome_var
    ))]
    for region_name in sorted(chosen_complete["admin_region"].dropna().unique()):
        region_rows.append(
            dict(
                excluded_region=region_name,
                **build_exposure_gap_metrics(
                    chosen_complete.loc[
                        (chosen_complete["exposed_cohort_any_overlap"] == 1)
                        & (chosen_complete["admin_region"] != region_name)
                    ],
                    outcome_var,
                ),
            )
        )
    region_sensitivity = pd.DataFrame(region_rows)

    region_positive_contribution = (
        chosen_complete.loc[chosen_complete["admin_region"].notna(), ["admin_region", "sample_weight", outcome_var]]
        .assign(positive_weight=lambda x: x["sample_weight"] * (x[outcome_var] == 1).astype(float))
        .groupby("admin_region", as_index=False)
        .agg(sample_n=("admin_region", "size"), region_weight=("sample_weight", "sum"), positive_weight=("positive_weight", "sum"))
    )
    total_region_weight = region_positive_contribution["region_weight"].sum()
    total_positive_weight = region_positive_contribution["positive_weight"].sum()
    region_positive_contribution["weighted_sample_share"] = (
        region_positive_contribution["region_weight"] / total_region_weight if total_region_weight > 0 else np.nan
    )
    region_positive_contribution["weighted_positive_share"] = (
        region_positive_contribution["positive_weight"] / total_positive_weight if total_positive_weight > 0 else np.nan
    )
    region_positive_contribution = region_positive_contribution.sort_values(
        ["weighted_positive_share", "sample_n"], ascending=[False, False]
    ).reset_index(drop=True)

    dominant_region = region_positive_contribution["admin_region"].iloc[0] if not region_positive_contribution.empty else np.nan
    dominant_positive_share = (
        region_positive_contribution["weighted_positive_share"].iloc[0] if not region_positive_contribution.empty else np.nan
    )
    target_region = "Tashkent city" if "Tashkent city" in region_sensitivity["excluded_region"].values else dominant_region
    target_row = region_sensitivity.loc[region_sensitivity["excluded_region"] == target_region]
    target_top_bottom = target_row["top_bottom_gap"].iloc[0] if not target_row.empty else np.nan
    target_top_half = target_row["top_half_bottom_half_gap"].iloc[0] if not target_row.empty else np.nan
    region_dominance_pass = (
        pd.notna(dominant_positive_share)
        and dominant_positive_share <= 0.50
        and pd.notna(target_top_bottom)
        and target_top_bottom >= -0.005
        and pd.notna(target_top_half)
        and target_top_half >= -0.005
    )

    cell_counts = (
        chosen_complete.groupby(["admin_region", "cohort_exposure_group", "low_parent_education"], dropna=False)
        .size()
        .reset_index(name="n")
    )
    cell_balance_summary = pd.DataFrame(
        [
            {
                "sample_label": chosen_sample,
                "sample_n": int(len(chosen_complete)),
                "cell_n": int(len(cell_counts)),
                "cell_min_n": float(cell_counts["n"].min()) if not cell_counts.empty else np.nan,
                "cell_p25_n": float(cell_counts["n"].quantile(0.25)) if not cell_counts.empty else np.nan,
                "cell_median_n": float(cell_counts["n"].median()) if not cell_counts.empty else np.nan,
                "cell_p75_n": float(cell_counts["n"].quantile(0.75)) if not cell_counts.empty else np.nan,
                "cell_max_n": float(cell_counts["n"].max()) if not cell_counts.empty else np.nan,
                "cell_share_ge_5": float((cell_counts["n"] >= 5).mean()) if not cell_counts.empty else np.nan,
                "cell_share_ge_10": float((cell_counts["n"] >= 10).mean()) if not cell_counts.empty else np.nan,
                "cell_share_ge_20": float((cell_counts["n"] >= 20).mean()) if not cell_counts.empty else np.nan,
            }
        ]
    )
    cell_balance_pass = (
        pd.notna(cell_balance_summary["cell_median_n"].iloc[0])
        and cell_balance_summary["cell_median_n"].iloc[0] >= 20
        and pd.notna(cell_balance_summary["cell_share_ge_10"].iloc[0])
        and cell_balance_summary["cell_share_ge_10"].iloc[0] >= 0.75
        and pd.notna(cell_balance_summary["cell_share_ge_20"].iloc[0])
        and cell_balance_summary["cell_share_ge_20"].iloc[0] >= 0.50
    )

    year_stability = (
        chosen_prefilter.groupby("survey_year")
        .apply(
            lambda x: pd.Series(
                {
                    "sample_n": int(len(x)),
                    "outcome_non_missing_share": float(x[outcome_var].notna().mean()),
                    "outcome_positive_rate": weighted_share(x[outcome_var] == 1, x["sample_weight"]),
                    "exposed_share": weighted_share(x["exposed_cohort_any_overlap"] == 1, x["sample_weight"]),
                }
            )
        )
        .reset_index()
        .sort_values("survey_year")
    )
    year_rate_range = (
        float(year_stability["outcome_positive_rate"].max() - year_stability["outcome_positive_rate"].min())
        if not year_stability.empty and year_stability["outcome_positive_rate"].notna().any()
        else np.nan
    )
    min_non_missing_share = (
        float(year_stability["outcome_non_missing_share"].min()) if not year_stability.empty else np.nan
    )
    year_stability_pass = (
        len(year_stability) >= 3
        and (year_stability["sample_n"] >= 100).all()
        and (year_stability["outcome_non_missing_share"] >= 0.85).all()
        and (pd.isna(year_rate_range) or year_rate_range <= 0.05)
    )

    def profile_for_sample(frame: pd.DataFrame, sample_label_value: str) -> dict[str, float | str]:
        region_shares = weighted_group_shares(frame, "admin_region")
        tashkent_share = region_shares.loc[region_shares["group"] == "Tashkent city", "share"]
        return {
            "sample_label": sample_label_value,
            "sample_n": int(len(frame)),
            "weighted_mean_age": weighted_mean(frame["age"], frame["sample_weight"]),
            "female_share": weighted_share(frame["sex"].astype(str).str.lower() == "female", frame["sample_weight"]),
            "urban_share": weighted_share(pd.to_numeric(frame["urban"], errors="coerce") == 1, frame["sample_weight"]),
            "tashkent_city_share": float(tashkent_share.iloc[0]) if not tashkent_share.empty else 0.0,
            "largest_region_share": float(region_shares["share"].max()) if not region_shares.empty else np.nan,
        }

    sample_profile = pd.DataFrame(
        [
            profile_for_sample(full_linked_complete, "full_linked"),
            profile_for_sample(chosen_complete, chosen_sample),
        ]
    )
    full_profile = sample_profile.loc[sample_profile["sample_label"] == "full_linked"].iloc[0]
    chosen_profile = sample_profile.loc[sample_profile["sample_label"] == chosen_sample].iloc[0]
    age_gap = abs(chosen_profile["weighted_mean_age"] - full_profile["weighted_mean_age"])
    female_gap = abs(chosen_profile["female_share"] - full_profile["female_share"])
    urban_gap = abs(chosen_profile["urban_share"] - full_profile["urban_share"])
    tashkent_gap = abs(chosen_profile["tashkent_city_share"] - full_profile["tashkent_city_share"])

    full_region_shares = weighted_group_shares(full_linked_complete, "admin_region").rename(columns={"share": "share_full"})
    chosen_region_shares = weighted_group_shares(chosen_complete, "admin_region").rename(columns={"share": "share_chosen"})
    region_profile_gap = full_region_shares.merge(chosen_region_shares, on="group", how="outer")
    region_profile_gap[["share_full", "share_chosen"]] = region_profile_gap[["share_full", "share_chosen"]].fillna(0.0)
    region_profile_gap["abs_gap"] = (region_profile_gap["share_chosen"] - region_profile_gap["share_full"]).abs()
    max_region_profile_gap = float(region_profile_gap["abs_gap"].max()) if not region_profile_gap.empty else np.nan

    sample_interpretability_pass = (
        pd.notna(age_gap)
        and age_gap <= 0.50
        and pd.notna(female_gap)
        and female_gap <= 0.08
        and pd.notna(urban_gap)
        and urban_gap <= 0.10
        and pd.notna(tashkent_gap)
        and tashkent_gap <= 0.10
        and pd.notna(max_region_profile_gap)
        and max_region_profile_gap <= 0.10
    )

    gate_status = pd.DataFrame(
        [
            {
                "check_name": "Direction sensible",
                "metric_summary": (
                    f"Exposed-cohort top vs bottom quartile gap = {fmt_pct(overall_gap['top_bottom_gap'])}; "
                    f"top-half vs bottom-half gap = {fmt_pct(overall_gap['top_half_bottom_half_gap'])}; "
                    f"full-overlap top vs bottom gap = {fmt_pct(full_overlap_gap['top_bottom_gap'])}"
                ),
                "pass": direction_pass,
            },
            {
                "check_name": "No single-region dominance",
                "metric_summary": (
                    f"Leading positive-contribution region = {dominant_region if pd.notna(dominant_region) else 'NA'} "
                    f"at {fmt_pct(dominant_positive_share)}; gap after excluding {target_region if pd.notna(target_region) else 'target region'} "
                    f"= {fmt_pct(target_top_bottom)}"
                ),
                "pass": region_dominance_pass,
            },
            {
                "check_name": "Cell balance",
                "metric_summary": (
                    f"Median cell n = {fmt_num(cell_balance_summary['cell_median_n'].iloc[0], 1)}; "
                    f"share >=10 = {fmt_pct(cell_balance_summary['cell_share_ge_10'].iloc[0])}; "
                    f"share >=20 = {fmt_pct(cell_balance_summary['cell_share_ge_20'].iloc[0])}"
                ),
                "pass": cell_balance_pass,
            },
            {
                "check_name": "Year stability",
                "metric_summary": (
                    f"Outcome non-missing share min = {fmt_pct(min_non_missing_share)}; "
                    f"year-to-year rate range = {fmt_pct(year_rate_range)}; years covered = {len(year_stability)}"
                ),
                "pass": year_stability_pass,
            },
            {
                "check_name": "Sample interpretability",
                "metric_summary": (
                    f"Age gap vs full linked = {fmt_num(age_gap, 2)}; "
                    f"female-share gap = {fmt_pct(female_gap)}; "
                    f"max region-share gap = {fmt_pct(max_region_profile_gap)}"
                ),
                "pass": sample_interpretability_pass,
            },
        ]
    )
    model_b_gate_pass = bool(gate_status["pass"].all())

    gate_status.to_csv(TABLE_DIR / "model_b_gate_status.csv", index=False)
    direction_review.to_csv(TABLE_DIR / "model_a_direction_review.csv", index=False)
    region_sensitivity.to_csv(TABLE_DIR / "model_a_region_sensitivity.csv", index=False)
    cell_balance_summary.to_csv(TABLE_DIR / "model_a_cell_balance_summary.csv", index=False)
    cell_counts.to_csv(TABLE_DIR / "model_a_cell_counts.csv", index=False)
    year_stability.to_csv(TABLE_DIR / "model_a_year_stability.csv", index=False)
    sample_profile.to_csv(TABLE_DIR / "model_a_sample_profile_comparison.csv", index=False)

    note_lines = [
        "# Model A Review Note",
        "",
        f"- Working sample reviewed: `{chosen_sample}`.",
        f"- Preferred linkage window: `{preferred_window}`.",
        f"- Preferred outcome: `{preferred_outcome}`.",
        f"- Direction check pass: `{direction_pass}`.",
        f"- Region-dominance check pass: `{region_dominance_pass}`.",
        f"- Cell-balance check pass: `{cell_balance_pass}`.",
        f"- Year-stability check pass: `{year_stability_pass}`.",
        f"- Sample-interpretability check pass: `{sample_interpretability_pass}`.",
        "",
        (
            f"Model A looks clean enough on the `{chosen_sample}` sample to unlock Model B as the next gated step. "
            "Model B is not estimated in this run; Model C remains deferred."
            if model_b_gate_pass
            else f"Model A does not yet clear all five review checks on the `{chosen_sample}` sample. "
            "The module should therefore stay at Model A only, with Model B and Model C still deferred."
        ),
    ]
    (OUTPUT_DIR / "model_a_review_note.md").write_text("\n".join(note_lines) + "\n", encoding="utf-8")

    model_status.loc[model_status["model"] == "Model B", "note"] = (
        "Not estimated in this run. Model A review gate passed; the next unlocked step is Model B on the no-migration-signal sample."
        if model_b_gate_pass
        else "Not estimated in this run. Model A review gate still fails at least one check, so Model B remains deferred."
    )
    model_status.loc[model_status["model"] == "Model C", "note"] = (
        "Deferred until after Model B is reviewed on the same restricted sample."
    )
    model_status.to_csv(model_status_path, index=False)

    update_progress(model_b_gate_pass, chosen_sample)


if __name__ == "__main__":
    main()
