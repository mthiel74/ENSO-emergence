"""Shared helpers for the live-data fetchers."""

from __future__ import annotations

import os
import pathlib
import time
from dataclasses import dataclass

import requests

ROOT = pathlib.Path(__file__).resolve().parent.parent
DATA = ROOT / "data"
RAW = DATA / "raw"
RAW.mkdir(parents=True, exist_ok=True)

UA = {
    "User-Agent": (
        "ENSO-emergence/0.1 (+https://github.com/mthiel74/ENSO-emergence) "
        "python-requests"
    )
}


@dataclass
class FetchResult:
    url: str
    saved_to: pathlib.Path
    bytes_written: int
    cached: bool


def fetch_text(
    url: str,
    out_path: pathlib.Path,
    *,
    timeout: float = 60.0,
    max_age_hours: float | None = None,
) -> FetchResult:
    """Download *url* into *out_path* as text.

    If *max_age_hours* is given and the file exists and is younger than
    that, the network is not hit.
    """
    out_path.parent.mkdir(parents=True, exist_ok=True)
    if max_age_hours is not None and out_path.exists():
        age_h = (time.time() - out_path.stat().st_mtime) / 3600.0
        if age_h < max_age_hours:
            return FetchResult(url, out_path, out_path.stat().st_size, cached=True)
    resp = requests.get(url, headers=UA, timeout=timeout)
    resp.raise_for_status()
    out_path.write_text(resp.text)
    return FetchResult(url, out_path, len(resp.content), cached=False)


def fetch_bytes(
    url: str,
    out_path: pathlib.Path,
    *,
    timeout: float = 60.0,
    max_age_hours: float | None = None,
) -> FetchResult:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    if max_age_hours is not None and out_path.exists():
        age_h = (time.time() - out_path.stat().st_mtime) / 3600.0
        if age_h < max_age_hours:
            return FetchResult(url, out_path, out_path.stat().st_size, cached=True)
    resp = requests.get(url, headers=UA, timeout=timeout, stream=True)
    resp.raise_for_status()
    n = 0
    with out_path.open("wb") as f:
        for chunk in resp.iter_content(chunk_size=1 << 15):
            f.write(chunk)
            n += len(chunk)
    return FetchResult(url, out_path, n, cached=False)


def banner(msg: str) -> None:
    print(f"\n=== {msg} ===")
