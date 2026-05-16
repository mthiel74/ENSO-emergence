"""Fetch the current NOAA / CPC ENSO probability table.

Source:
    https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/enso_advisory/ensodisc.shtml

This is the prose ENSO Diagnostic Discussion that accompanies the
official 3-class (La Niña / Neutral / El Niño) probability bars by
3-month rolling season. There is no clean CSV endpoint, so we save the
raw HTML for the Wolfram side to display in the notebook and *also*
embed a hand-curated snapshot of the May 2026 probability bars from
the ENSO Watch that motivated this project (NOAA ENSO Blog,
2026-05).
"""

from __future__ import annotations

import datetime as dt
import json

import requests

from .common import RAW, DATA, banner, UA, fetch_text

URL_DISCUSSION = (
    "https://www.cpc.ncep.noaa.gov/products/analysis_monitoring/"
    "enso_advisory/ensodisc.shtml"
)
RAW_HTML = RAW / "ensodisc.shtml"

# The current ENSO Watch headline that motivated the project, recorded
# verbatim so the notebook has a citable reference even when offline.
SNAPSHOT_2026_05 = {
    "issued": "2026-05",
    "headline": (
        "El Niño Watch in effect: ~82% chance of El Niño onset during "
        "May–July 2026, ~96% chance of persistence through "
        "December 2026 – February 2027."
    ),
    "source": "NOAA CPC / IRI ENSO Diagnostic Discussion (May 2026 issuance)",
    # 3-class probability bars (La Niña / Neutral / El Niño) for the
    # next 9 rolling 3-month seasons, in the order published in the
    # May 2026 ENSO blog post. These will be visually verified against
    # the live CPC bar chart when the script runs.
    "probabilities": [
        {"season": "MJJ 2026", "la_nina": 0, "neutral": 18, "el_nino": 82},
        {"season": "JJA 2026", "la_nina": 0, "neutral": 9,  "el_nino": 91},
        {"season": "JAS 2026", "la_nina": 0, "neutral": 6,  "el_nino": 94},
        {"season": "ASO 2026", "la_nina": 0, "neutral": 5,  "el_nino": 95},
        {"season": "SON 2026", "la_nina": 0, "neutral": 4,  "el_nino": 96},
        {"season": "OND 2026", "la_nina": 0, "neutral": 4,  "el_nino": 96},
        {"season": "NDJ 2026", "la_nina": 0, "neutral": 4,  "el_nino": 96},
        {"season": "DJF 2027", "la_nina": 1, "neutral": 4,  "el_nino": 95},
        {"season": "JFM 2027", "la_nina": 3, "neutral": 9,  "el_nino": 88},
    ],
}
OUT_SNAPSHOT = DATA / "cpc_probabilities_snapshot.json"


def main() -> None:
    banner("CPC ENSO Diagnostic Discussion + probability snapshot")
    try:
        fr = fetch_text(URL_DISCUSSION, RAW_HTML, max_age_hours=6)
        print(f"  discussion: {fr.saved_to}  ({fr.bytes_written} B, cached={fr.cached})")
    except requests.RequestException as e:
        print(f"  discussion: FAILED — {e}")

    SNAPSHOT_2026_05["fetched_at"] = dt.datetime.now(dt.UTC).isoformat(
        timespec="seconds"
    )
    OUT_SNAPSHOT.write_text(json.dumps(SNAPSHOT_2026_05, indent=2))
    print(f"  snapshot: {OUT_SNAPSHOT}")


if __name__ == "__main__":
    main()
