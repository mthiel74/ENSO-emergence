"""Run every live-data fetcher in one go.

Usage:
    python -m src.fetch_all
"""

from __future__ import annotations

from . import fetch_nino34, fetch_oni, fetch_iri_plume, fetch_cpc_probabilities


def main() -> None:
    fetch_nino34.main()
    fetch_oni.main()
    fetch_iri_plume.main()
    fetch_cpc_probabilities.main()


if __name__ == "__main__":
    main()
