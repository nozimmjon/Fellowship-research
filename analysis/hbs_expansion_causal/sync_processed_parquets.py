from __future__ import annotations

from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[2]
TABLE_DIR = ROOT / "outputs" / "hbs_expansion_causal" / "tables"
PROCESSED_DIR = ROOT / "data" / "processed"

SYNC_TARGETS = [
    (
        TABLE_DIR / "_tmp_hbs_expansion_person_harmonized.csv",
        PROCESSED_DIR / "hbs_expansion_person_harmonized.parquet",
        "harmonized person file",
    ),
    (
        TABLE_DIR / "_tmp_hbs_expansion_merged.csv",
        PROCESSED_DIR / "hbs_expansion_merged.parquet",
        "merged analysis file",
    ),
]


def main() -> None:
    PROCESSED_DIR.mkdir(parents=True, exist_ok=True)
    synced = []
    for csv_path, parquet_path, label in SYNC_TARGETS:
        if not csv_path.exists():
            raise FileNotFoundError(f"Missing temporary CSV for {label}: {csv_path}")
        df = pd.read_csv(csv_path, low_memory=False)
        df.to_parquet(parquet_path, index=False)
        synced.append((label, parquet_path))

    for label, parquet_path in synced:
        print(f"Synced {label} to {parquet_path}")


if __name__ == "__main__":
    main()
