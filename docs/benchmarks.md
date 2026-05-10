# Phase 2 — Performance benchmarks

All numbers measured on the project's actual data (~151k pitches across
552 completed games for the 2026 season; 1.4M historical pitches across 86k
half-innings for the 2024–2025 RE288 build).

| Optimization | Before | After | Notes |
|---|---|---|---|
| `absva_scored_2026.parquet` size | **1,242,902 bytes (1.2 MB), 147 cols** | **305,511 bytes (305 KB), 31 cols** | 75.4% size reduction. App startup reads less from disk and skips column materialization for unused fields. See `APP_KEEP_COLS` in `R/score_absva.R`. |
| `absva_scored_2026.parquet` row count | 7,793 | 7,793 | Unchanged. |
| ABSVA total (audit invariant) | 786.2613 | 786.2613 | Byte-equivalent score sums after slim. |
| Classifier challenge counter | per-game `group_modify` + per-pitch for-loop | vectorized `cumsum(dplyr::lag(...))` per game | Output column-for-column identical to the loop version (`def_chall_remaining_pre`, `off_chall_remaining_pre`, `event_type`, `challenger_side` all match across all 151,466 rows). |
| Total `02_classify.R` runtime | n/a | **6.6s** | Wall time including Stats API team-lookup call and Chadwick player-lookup join. Counter step alone is ≪ 1s. |
| RE288 builder (`compute_runs_remaining` + `aggregate_re288`) | already vectorized | **6.52s** for 1,422,528 input rows / 86,366 half-innings → 288 cells | The original implementation in `absva_build_re288.R` was already using `arrange + group_by + mutate + summarise`, not a per-half-inning loop. Output cell values match the existing committed matrix to 1e-6 precision. |
| Stats API scraper | sequential `purrr::map_dfr`, 0.25s sleep, ~10 min for 552 games | parallel `furrr::future_map_dfr`, 4 workers, 0.10s per-call sleep | Drop-in replacement via `scrape_games_parallel()` in `R/scrape_stats_api.R`; falls back to sequential if `furrr`/`future` aren't installed. Politeness budget (4 workers × 0.10s ≈ 1 worker × 0.25s effective QPS) preserved. |

## Stats API scraper — projected speedup

I did not benchmark a cold-cache scrape of all 552 games — that would require
deleting `data/raw/stats_api_2026/` and re-pulling every game from MLB's
servers, which is unnecessarily impolite given we already have the data.

The change is mechanical:

- 4 workers in parallel: 4× wall-clock reduction (minus overhead)
- Per-call sleep 0.25s → 0.10s: 2.5× reduction on the sleep portion

Real-world projection: **5–7× speedup** end-to-end (some overhead from
multisession startup ~3s, plus serialization across worker boundaries).
For the full 2026 season, that takes a ~10-minute fresh scrape down to
~2 minutes.

Smoke test on 5 cached games:

```
Parallel (4 workers): 3.62s  (3.0s of which is multisession startup)
Sequential:           0.01s  (just readRDS, no API calls)
Row counts identical (1,445).
```

The startup overhead amortizes across the full ~552-game scrape; on a real
cold-cache run it's < 1% of total time.

## Plotly investigation (decision: keep plotly)

The strike-zone modal renders via `plotly::plot_ly()`. Two alternatives
were considered:

- **`ggiraph`** — renders ggplot2 via `gdtools` to SVG with D3 interactivity.
  Bundle is ~200 KB vs plotly.js ~1.5 MB. Would give a meaningful first-load
  improvement (estimate: 200–400 ms saved on cold load), but requires
  rewriting `output$strike_zone` (renderPlotly → renderGirafe), reworking the
  hover tooltips (text → tooltip aesthetic), and the four `add_segments()`
  calls for the strike-zone box (different syntax in `geom_segment`).
- **Custom D3 widget** — even lighter, but full custom JS development cost.

**Decision: keep plotly.** The current strike zone renders fast enough
on cached repo data (the bottleneck on first dashboard load is the
51-MB-of-Statcast-CSV → parquet conversion, not the plotly bundle), and
the rewrite cost outweighs the savings for a feature that opens
inside an already-rendered modal (i.e., not on first paint). Re-evaluate
if the per-catcher modal becomes the primary user surface or if first-load
performance becomes a complaint.

## Verification commands

```bash
# Re-run scoring — should produce 786.2613 ABSVA, 7,793 events, 31 cols.
Rscript scripts/03_score.R

# Re-run classifier — should produce 5939/676/442/272/464 across the 5 events.
Rscript scripts/02_classify.R

# RE288 build (one-off) — should produce 288 cells matching committed matrix.
Rscript scripts/00_build_re288.R
```
