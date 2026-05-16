"""Fetch the CPC Oceanic Niño Index (ONI) table.

Source:
    https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt

The ASCII table has columns:

    SEAS  YR  TOTAL  ANOM
    DJF   1950 24.72 -1.53
    JFM   1950 25.17 -1.34
    ...

`SEAS` is a 3-letter rolling-3-month label; `TOTAL` is the Niño 3.4 SST
(°C) and `ANOM` is the anomaly vs the rolling 30-year base period. The
ONI is defined as a 3-month running mean of ERSSTv5 Niño 3.4 anomalies,
and the threshold for El Niño classification is +0.5 °C for five
consecutive overlapping seasons.
"""

from __future__ import annotations

import csv

from .common import RAW, DATA, banner, fetch_text

URL = "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
RAW_FILE = RAW / "oni.ascii.txt"
OUT_CSV = DATA / "oni.csv"

SEASON_MID_MONTH = {
    "DJF": 1, "JFM": 2, "FMA": 3, "MAM": 4, "AMJ": 5, "MJJ": 6,
    "JJA": 7, "JAS": 8, "ASO": 9, "SON": 10, "OND": 11, "NDJ": 12,
}


def parse_oni(text: str) -> list[tuple[str, int, int, float, float]]:
    rows: list[tuple[str, int, int, float, float]] = []
    lines = [ln.strip() for ln in text.splitlines() if ln.strip()]
    for ln in lines:
        parts = ln.split()
        if len(parts) != 4:
            continue
        seas, yr_s, total_s, anom_s = parts
        if seas not in SEASON_MID_MONTH:
            continue  # header line
        try:
            yr = int(yr_s)
            total = float(total_s)
            anom = float(anom_s)
        except ValueError:
            continue
        rows.append((seas, yr, SEASON_MID_MONTH[seas], total, anom))
    return rows


def write_csv(rows, path) -> int:
    with path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["season", "year", "mid_month", "sst_c", "sst_anom"])
        w.writerows(rows)
    return len(rows)


def main() -> None:
    banner("Oceanic Niño Index — CPC")
    fr = fetch_text(URL, RAW_FILE, max_age_hours=6)
    print(f"  raw: {fr.saved_to}  ({fr.bytes_written} bytes, cached={fr.cached})")
    rows = parse_oni(fr.saved_to.read_text())
    n = write_csv(rows, OUT_CSV)
    span = (rows[0], rows[-1]) if rows else None
    print(f"  parsed: {n} rows  span={span}")
    print(f"  tidy: {OUT_CSV}")


if __name__ == "__main__":
    main()
