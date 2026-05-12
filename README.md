# ABSVA — ABS Value Added

An open-source metric that quantifies catcher performance under MLB's
Automatic Ball-Strike (ABS) challenge system. Built from publicly available
Statcast pitch data and MLB Stats API play-by-play.

The repo contains:

- An R Shiny dashboard with a sortable catcher leaderboard, KPIs, and a
  per-catcher modal that shows the strike zone, event-type breakdown, and
  league percentile bars.
- A reproducible four-stage pipeline that scrapes raw data, classifies each
  pitch into one of five ABSVA events, builds a 288-cell run-expectancy
  matrix from 2024-2025 history, and computes per-event run values.

## What ABSVA measures

Each called pitch is one of five **events** from the catcher's perspective:

| Event | Sign | Meaning |
|---|:---:|---|
| `frame`    | + | True ball, called strike, no challenge |
| `challenge_win`    | + | Catcher challenges a ball; Hawk-Eye overturns to strike |
| `overturned_frame`    | 0 | True ball called strike; batter challenges and Hawk-Eye reverses the catcher's frame (valued at 0) |
| `missed_ABS_opportunity`     | – | True strike called ball, no challenge, defense had ≥1 challenge available |
| `challenge_lost`    | – | Catcher challenges, loses (fixed −0.057 penalty) |

Each event's run value is computed against the RE288 matrix
(base × out × count). ABSVA is the sum of run values across a catcher's
events; ABSVA/100 is the rate-stat per 100 pitches caught.

See [docs/methodology.md](docs/methodology.md) for the full write-up.

## Repo layout

```
absva/
├── app/                   Shiny app (global, ui, server)
├── R/                     Pipeline functions (sourced by scripts/)
├── scripts/               Top-level executables — run in order
│   ├── 00_build_re288.R   One-time historical RE288 build
│   ├── 01_scrape_season.R Pull Statcast + Stats API for current season
│   ├── 02_classify.R      Per-pitch event classification + names
│   ├── 03_score.R         Apply RE288, write scored + leaderboard parquets
│   └── diagnostics/       One-off audits and validators
├── data/
│   ├── processed/         What the app reads (committed)
│   ├── intermediate/      Pipeline artifacts (gitignored)
│   ├── raw/               Scrape caches + logs (gitignored)
│   └── reference/         Static reference (heatmap PNG, etc.)
└── docs/
    └── methodology.md
```

## Install

Requires R ≥ 4.4. Clone the repo and restore the package environment:

```bash
git clone <repo-url>
cd absva/
Rscript -e 'install.packages("renv"); renv::restore()'
```

The first restore takes ~5 minutes (it installs `arrow`, `bs4Dash`, etc.).

## Run the pipeline

```bash
Rscript scripts/00_build_re288.R    # one-time, ~10 min on cold cache
Rscript scripts/01_scrape_season.R  # pulls current season; resumable
Rscript scripts/02_classify.R       # ~30s
Rscript scripts/03_score.R          # ~10s
```

Each script is **resumable**: re-running skips work whose output already
exists on disk. On a typical day after the cache is built, only new dates
need scraping (~30s).

## Launch the app

From the repo root:

```bash
Rscript -e 'shiny::runApp("app")'
```

The app reads the parquets in `data/processed/` and renders at
`http://localhost:<port>`.

## Data sources

- **Statcast** — pitch-level data via `baseballsavant.mlb.com/statcast_search/csv`
  (no `baseballr` dependency; baseballr's column-mismatch bugs caused issues).
- **MLB Stats API** — play-by-play from `statsapi.mlb.com/api/v1/game/{pk}/playByPlay`,
  filtered to `review_type == "MJ"` (ABS challenges only; excludes manager
  replay challenges, which are `"MA"`).
- **Chadwick Bat Register** — for player_id → name lookups (cached locally).

The Stats API stores reviewDetails in three different locations
(`playEvent.pitch.review`, `playEvent.action.review`, and `play.reviewDetails`);
the scraper walks all three.

## Validation

The build is gated on three benchmarks:

- League-wide challenge count matches Opta's published count (~2,160 through
  May 3 of the 2026 season; scaled for the current date).
- Overturn rate is ~53.2% league-wide.
- RE288 cells fall within 0.18 runs of Tango's published reference values
  (`scripts/diagnostics/validate_re288.R` checks this).

## License

MIT. See [LICENSE](LICENSE).
