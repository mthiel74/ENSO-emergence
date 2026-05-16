# Repo notes for Claude

## Purpose

Produce a Wolfram Community post — `community/enso_emergence.nb` — on
the developing 2026 El Niño event and the **spring predictability
barrier**. Live NOAA / IRI data + two textbook ENSO toy models
(recharge–discharge oscillator, delayed oscillator) implemented in
Wolfram Language.

## Pipeline (pure Wolfram Language — no Python)

```
wolfram/fetch_nino34.wls         ─┐
wolfram/fetch_oni.wls             ├─> data/*.csv / *.json
wolfram/fetch_iri_plume.wls       │   (live data, regenerable)
wolfram/fetch_cpc_probabilities.wls ─┘
wolfram/fetch_all.wls              one-shot driver

wolfram/load_data.wl   shared loader package
                       │
wolfram/barrier.wls    ── observed Niño 3.4 → ρ(month, lead) heatmap
wolfram/rdo.wls        ── recharge–discharge oscillator + ensembles
wolfram/delayed.wls    ── Suarez–Schopf delayed oscillator
wolfram/forecast.wls   ── overlay toy ensemble cone on IRI plume
wolfram/run_all.wls    ── one entry point, writes docs/images/*.png

community/build_notebook.wls   ── assembles enso_emergence.nb + .pdf
```

## Conventions

* Plain-text `.wls` is the source of truth; the `.nb` and `.pdf` in
  `community/` are committed *outputs* (for diff-review and so the
  Wolfram Community submission is trivial).
* Figures live in `docs/images/` only — referenced from both the README
  and the notebook.
* Bulk raw downloads stay in `data/raw/` which is git-ignored. Tidy
  small CSV / JSON files used by the Wolfram pipeline live at
  `data/*.csv` / `data/*.json` and are committed for reproducibility
  on machines without internet access at the moment of running.
* All HTTP fetches use `URLRead[HTTPRequest[...]]` and check the
  status code — `URLDownload` silently writes server error pages
  on 4xx/5xx, so don't use it.

## Commit cadence

Commit + push after each meaningful step (skeleton, fetchers,
barrier figure, each toy model, overlay, notebook). Keep messages
short and factual.
