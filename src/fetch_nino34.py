"""Fetch the monthly Niño 3.4 SST anomaly time series.

Primary source:
    NOAA PSL – https://psl.noaa.gov/data/correlation/nina34.anom.data
    (ERSSTv5 anomalies, base period 1991-2020, monthly, from 1948-01.)

The file is a fixed-width ASCII table:

    1948 2020          <- start/end year
    1948  -1.05 -1.21  ...   12 monthly values
    1949  ...
    ...
    -99.99             <- missing-value sentinel

We normalise it into two tidy CSVs in data/:

    nino34_monthly.csv     year, month, date, sst_anom
    nino34_monthly_long.csv  same, long format alias for Wolfram pipeline
"""

from __future__ import annotations

import csv
import datetime as dt

from .common import RAW, DATA, banner, fetch_text

URL_PSL = "https://psl.noaa.gov/data/correlation/nina34.anom.data"
RAW_FILE = RAW / "nina34.anom.data"
OUT_CSV = DATA / "nino34_monthly.csv"


def parse_psl_anom(text: str) -> list[tuple[int, int, float]]:
    """Parse the PSL fixed-width anomaly file into (year, month, value) rows."""
    rows: list[tuple[int, int, float]] = []
    missing_sentinels: set[float] = set()
    lines = [ln.rstrip() for ln in text.splitlines() if ln.strip()]
    if not lines:
        raise ValueError("empty PSL anomaly file")
    header = lines[0].split()
    if len(header) != 2:
        raise ValueError(f"unexpected header {header!r}")
    y0, y1 = int(header[0]), int(header[1])
    for ln in lines[1:]:
        parts = ln.split()
        # Footer block: missing-value sentinel (e.g. "-99.99") + free text.
        if len(parts) == 1:
            try:
                missing_sentinels.add(float(parts[0]))
            except ValueError:
                pass
            continue
        if len(parts) < 13:
            # not a data row
            continue
        try:
            yr = int(parts[0])
        except ValueError:
            continue
        if yr < y0 or yr > y1:
            continue
        vals = [float(x) for x in parts[1:13]]
        for m, v in enumerate(vals, start=1):
            rows.append((yr, m, v))
    # Drop sentinel-coded missing rows.
    if missing_sentinels:
        rows = [(y, m, v) for (y, m, v) in rows if v not in missing_sentinels]
    return rows


def write_csv(rows: list[tuple[int, int, float]], path) -> int:
    with path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["year", "month", "date", "sst_anom"])
        for y, m, v in rows:
            d = dt.date(y, m, 15).isoformat()
            w.writerow([y, m, d, f"{v:.4f}"])
    return len(rows)


def main() -> None:
    banner("Niño 3.4 monthly SST anomalies (NOAA PSL, ERSSTv5)")
    fr = fetch_text(URL_PSL, RAW_FILE, max_age_hours=6)
    print(f"  raw: {fr.saved_to}  ({fr.bytes_written} bytes, cached={fr.cached})")
    rows = parse_psl_anom(fr.saved_to.read_text())
    n = write_csv(rows, OUT_CSV)
    span = (rows[0], rows[-1]) if rows else None
    print(f"  parsed: {n} rows  span={span}")
    print(f"  tidy: {OUT_CSV}")


if __name__ == "__main__":
    main()
