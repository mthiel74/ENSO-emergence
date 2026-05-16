"""Fetch the latest IRI / CPC ENSO forecast plume image.

The plume is the canonical figure showing Niño 3.4 forecasts from
~25 dynamical + statistical models out to ~9 months. As of 2024 IRI
stopped publishing the underlying per-model data table; only the
rendered PNG remains public.

Endpoint:
    https://ensoforecast.iri.columbia.edu/cgi-bin/sst_table_img?month=<m>&year=<y>

The plume is issued in the second half of each month. We walk back
from the current month and grab the most recent issue available.
"""

from __future__ import annotations

import datetime as dt
import json

import requests

from .common import RAW, DATA, banner, fetch_bytes

URL_TEMPLATE = (
    "https://ensoforecast.iri.columbia.edu/cgi-bin/sst_table_img"
    "?month={month}&year={year}"
)

OUT_PNG = RAW / "iri_plume_current.png"
MANIFEST = DATA / "iri_plume_manifest.json"


def try_get(url: str, dest) -> dict:
    try:
        fr = fetch_bytes(url, dest, max_age_hours=6)
        return {
            "url": url,
            "saved_to": str(fr.saved_to.relative_to(dest.parent.parent)),
            "bytes": fr.bytes_written,
            "cached": fr.cached,
            "ok": True,
        }
    except requests.HTTPError as e:
        return {"url": url, "ok": False, "error": f"HTTP {e.response.status_code}"}
    except requests.RequestException as e:
        return {"url": url, "ok": False, "error": str(e)}


def walk_back_for_plume(start: dt.date, max_steps: int = 6) -> tuple[dict, dt.date | None]:
    """Walk back month-by-month until we find an available plume issue."""
    issue = dt.date(start.year, start.month, 1)
    last_attempt: dict = {}
    for _ in range(max_steps):
        url = URL_TEMPLATE.format(month=issue.month, year=issue.year)
        attempt = try_get(url, OUT_PNG)
        last_attempt = attempt
        if attempt["ok"]:
            return attempt, issue
        # step back one month
        prev_month = issue.month - 1 or 12
        prev_year = issue.year if issue.month > 1 else issue.year - 1
        issue = dt.date(prev_year, prev_month, 1)
    return last_attempt, None


def main() -> None:
    banner("IRI / CPC ENSO forecast plume")
    today = dt.date.today()
    attempt, issue = walk_back_for_plume(today)
    manifest = {
        "fetched_at": dt.datetime.now(dt.UTC).isoformat(timespec="seconds"),
        "issue_month": issue.isoformat() if issue else None,
        "attempt": attempt,
    }
    if attempt.get("ok"):
        print(
            f"  png: {attempt['saved_to']} "
            f"(issue {issue}, {attempt['bytes']} B, cached={attempt['cached']})"
        )
    else:
        print(f"  png: FAILED  last_error={attempt.get('error')}")
    MANIFEST.write_text(json.dumps(manifest, indent=2))
    print(f"  manifest: {MANIFEST}")


if __name__ == "__main__":
    main()
