from __future__ import annotations

from pathlib import Path
import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
META = ROOT / "data" / "metadata"


def read_csv(name: str) -> pd.DataFrame:
    path = META / name
    if not path.exists():
        return pd.DataFrame()
    return pd.read_csv(path)


def read_text(name: str) -> str:
    path = META / name
    if not path.exists():
        return ""
    return path.read_text(encoding="utf-8", errors="ignore")


def with_source(df: pd.DataFrame, source: str) -> pd.DataFrame:
    if df.empty:
        return df
    out = df.copy()
    out.insert(0, "source", source)
    return out


def build_data_audit_workbook() -> Path:
    lits_checks = read_csv("audit_lits_checks.csv")
    hbs_checks = read_csv("audit_hbs_checks.csv")
    admin_checks = read_csv("audit_admin_checks.csv")

    summary = pd.concat(
        [
            with_source(lits_checks, "LiTS (2010, 2016, 2022-23)"),
            with_source(hbs_checks, "HBS"),
            with_source(admin_checks, "Administrative data"),
        ],
        ignore_index=True,
    )

    status_counts = (
        summary.groupby(["source", "status"], dropna=False)
        .size()
        .reset_index(name="n")
        .sort_values(["source", "status"])
    )

    lits_files = read_csv("audit_lits_file_inventory.csv")
    lits_expected = read_csv("audit_lits_expected_waves.csv")
    hbs_modules = read_csv("audit_hbs_modules_by_year.csv")
    hbs_presence = read_csv("audit_hbs_key_module_presence.csv")
    hbs_edu = read_csv("audit_hbs_education_consistency_by_year.csv")
    admin_files = pd.DataFrame(
        {
            "path": [
                str(p.relative_to(ROOT)).replace("\\", "/")
                for p in (ROOT / "data" / "raw" / "admin").rglob("*")
                if p.is_file()
            ]
        }
    )

    inventories = pd.DataFrame(
        {
            "source": [
                "LiTS",
                "HBS",
                "Administrative data",
            ],
            "inventory_md_file": [
                "data/metadata/audit_lits_inventory.md",
                "data/metadata/audit_hbs_inventory.md",
                "data/metadata/audit_admin_inventory.md",
            ],
            "inventory_exists": [
                (META / "audit_lits_inventory.md").exists(),
                (META / "audit_hbs_inventory.md").exists(),
                (META / "audit_admin_inventory.md").exists(),
            ],
        }
    )

    inventory_text = pd.DataFrame(
        {
            "source": [
                "LiTS",
                "HBS",
                "Administrative data",
            ],
            "inventory_text": [
                read_text("audit_lits_inventory.md"),
                read_text("audit_hbs_inventory.md"),
                read_text("audit_admin_inventory.md"),
            ],
        }
    )

    out_path = META / "02_data_audit.xlsx"
    with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
        summary.to_excel(writer, sheet_name="Summary_Checks", index=False)
        status_counts.to_excel(writer, sheet_name="Status_Counts", index=False)
        lits_checks.to_excel(writer, sheet_name="LiTS_Checks", index=False)
        hbs_checks.to_excel(writer, sheet_name="HBS_Checks", index=False)
        admin_checks.to_excel(writer, sheet_name="Admin_Checks", index=False)
        lits_files.to_excel(writer, sheet_name="LiTS_File_Inventory", index=False)
        lits_expected.to_excel(writer, sheet_name="LiTS_Expected_Waves", index=False)
        hbs_modules.to_excel(writer, sheet_name="HBS_Modules_By_Year", index=False)
        hbs_presence.to_excel(writer, sheet_name="HBS_Key_Modules", index=False)
        hbs_edu.to_excel(writer, sheet_name="HBS_Edu_Consistency", index=False)
        admin_files.to_excel(writer, sheet_name="Admin_File_List", index=False)
        inventories.to_excel(writer, sheet_name="Inventory_Files", index=False)
        inventory_text.to_excel(writer, sheet_name="Inventory_Text", index=False)

    return out_path


def build_variable_crosswalk_workbook() -> Path:
    field_presence = read_csv("audit_lits_variable_presence_by_wave.csv")
    lits_checks = read_csv("audit_lits_checks.csv")
    hbs_checks = read_csv("audit_hbs_checks.csv")
    admin_checks = read_csv("audit_admin_checks.csv")
    lits_overlap = read_csv("audit_lits_wave_overlaps.csv")

    lits_crosswalk = pd.DataFrame()
    if not field_presence.empty:
        pivot_examples = (
            field_presence.pivot_table(
                index="field",
                columns="wave",
                values="examples",
                aggfunc="first",
            )
            .reset_index()
            .rename_axis(None, axis=1)
        )
        pivot_present = (
            field_presence.pivot_table(
                index="field",
                columns="wave",
                values="present",
                aggfunc="first",
            )
            .reset_index()
            .rename_axis(None, axis=1)
        )
        pivot_present.columns = [
            col if col == "field" else f"{col}_present"
            for col in pivot_present.columns
        ]
        lits_crosswalk = pivot_examples.merge(pivot_present, on="field", how="left")
        lits_crosswalk = lits_crosswalk.merge(
            lits_checks[["field", "status", "notes"]],
            on="field",
            how="left",
        )
        missing_from_presence = lits_checks.loc[
            ~lits_checks["field"].isin(lits_crosswalk["field"]),
            ["field", "status", "notes"],
        ]
        if not missing_from_presence.empty:
            lits_crosswalk = pd.concat(
                [lits_crosswalk, missing_from_presence],
                ignore_index=True,
                sort=False,
            )
    elif not lits_checks.empty:
        lits_crosswalk = lits_checks[["field", "status", "notes"]].copy()

    hbs_crosswalk = hbs_checks.rename(
        columns={
            "field": "construct",
            "status": "availability_status",
            "notes": "evidence",
        }
    )
    admin_crosswalk = admin_checks.rename(
        columns={
            "field": "construct",
            "status": "availability_status",
            "notes": "evidence",
        }
    )

    out_path = META / "03_variable_crosswalk.xlsx"
    with pd.ExcelWriter(out_path, engine="openpyxl") as writer:
        lits_crosswalk.to_excel(writer, sheet_name="LiTS_Construct_Crosswalk", index=False)
        field_presence.to_excel(writer, sheet_name="LiTS_Construct_Long", index=False)
        lits_overlap.to_excel(writer, sheet_name="LiTS_Wave_Overlap", index=False)
        hbs_crosswalk.to_excel(writer, sheet_name="HBS_Construct_Checks", index=False)
        admin_crosswalk.to_excel(writer, sheet_name="Admin_Construct_Checks", index=False)

    return out_path


def main() -> None:
    out1 = build_data_audit_workbook()
    out2 = build_variable_crosswalk_workbook()
    print(f"Wrote: {out1}")
    print(f"Wrote: {out2}")


if __name__ == "__main__":
    main()
