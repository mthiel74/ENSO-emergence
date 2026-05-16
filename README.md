# ENSO Emergence and the Spring Predictability Barrier

A live, reproducible analysis of the developing El Niño event
flagged by NOAA's **El Niño Watch (May 2026)** —
~82 % probability of onset May–July 2026, ~96 % probability of
persistence through DJF 2026–27 — combined with a from-scratch
implementation of the two textbook coupled ocean–atmosphere toy
models used to *explain* why springtime ENSO forecasts are so
notoriously unreliable.

## What the project does

1. **Pulls live data** from authoritative sources
   - NOAA / NCEI ERSSTv5 Niño 3.4 monthly SST anomalies (back to 1950)
   - NOAA CPC Oceanic Niño Index (3-month running mean of Niño 3.4)
   - IRI / CPC ENSO forecast plume (the most recent dynamical +
     statistical model spread)
   - NOAA CPC official ENSO probability table (the current Watch)
2. **Computes the spring predictability barrier** directly from the
   observational record: lagged auto-correlation $\rho(m, \tau)$ of
   the Niño 3.4 anomaly as a function of calendar month $m$ and lead
   time $\tau$. The collapse of $\rho$ when the lead crosses
   April–May is the barrier.
3. **Implements two canonical toy models** of ENSO in pure Wolfram
   Language
   - the **recharge–discharge oscillator** of Jin (1997) — a 2-D
     deterministic + stochastic system in equatorial-mean SST
     anomaly $T$ and thermocline depth $h$;
   - the **delayed oscillator** of Suarez & Schopf (1988) — a scalar
     delay-differential equation with a wave-reflection delay.
4. **Generates ensemble forecasts** from each toy model with
   stochastic westerly-wind-burst forcing, and shows the *seasonal*
   collapse of the ensemble cone exactly where the observed barrier
   sits.
5. **Overlays the toy-model cone on the live IRI plume**, side by
   side with the NOAA probability bars.
6. Produces a self-contained **Wolfram Community notebook** as the
   end-product (see `community/`).

## Repository layout

| path           | what lives there                                              |
| ---            | ---                                                           |
| `src/`         | Python fetchers for the live NOAA / IRI feeds                 |
| `data/`        | cached raw + tidy CSV / JSON pulled by `src/` (git-ignored bulk) |
| `wolfram/`     | `.wls` scripts implementing the toy models + plots            |
| `community/`   | the buildable Wolfram Community notebook + its `.wls` source  |
| `docs/images/` | rendered figures referenced from the notebook and the README  |
| `tests/`       | sanity checks (data shape, model conservation laws)           |

## Reproducing

```sh
# 1. Fetch the live data (writes into data/)
python -m src.fetch_all

# 2. Run the analysis + render figures (writes into docs/images/)
wolframscript -file wolfram/run_all.wls

# 3. Build the community notebook (writes community/enso_emergence.nb)
wolframscript -file community/build_notebook.wls
```

## Status

Active. Target publication window: before the next NOAA CPC ENSO
diagnostic discussion update.

## Related projects

* [Contiguous-Cartograms](https://github.com/mthiel74/Contiguous-Cartograms)
* [streetview360](https://github.com/mthiel74/streetview360)
* [SofaProblem](https://github.com/mthiel74/SofaProblem)
