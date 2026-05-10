# ABSVA — Methodology

## What the metric measures

ABSVA (ABS Value Added) is a catcher run-value metric built around MLB's
Automatic Ball-Strike (ABS) challenge system. It assigns a run value to each
called pitch based on:

- whether the pitch was a true ball or true strike (per Hawk-Eye geometry),
- what the umpire called pre-challenge,
- whether either team challenged, and
- if challenged, whether Hawk-Eye overturned the call.

ABSVA is the sum of those run values across a catcher's pitches.

## The 5-event taxonomy (catcher perspective)

| Event | Sign | Trigger | Run-value formula |
|---|:---:|---|---|
| `framing_steal`    | + | True ball, called strike, no challenge                              | `RE(after_ball) − RE(after_strike)` |
| `challenge_win`    | + | Catcher challenges a called ball; Hawk-Eye overturns to strike      | `RE(after_ball) − RE(after_strike)` |
| `overturned_frame`    | 0 | True ball called strike; batter challenges and Hawk-Eye reverses    | `0` (per spec — the catcher's frame got noticed) |
| `miss_penalty`     | – | True strike called ball, no challenge, defense had ≥1 challenge left | `RE(after_strike) − RE(after_ball)` |
| `bad_challenge`    | – | Catcher challenges, loses                                           | `−0.057` (fixed; calibrated below) |

The RE288 matrix gives expected runs scored by the offense from the current
state to the end of the half-inning. Catcher run value is the **negative** of
the offense impact, so when a catcher converts ball → strike (good for the
defense), the offense's `RE` drops and the catcher's RV is positive.

## Trust hierarchy

- **Pitches that were challenged**: trust Hawk-Eye's verdict
  (`is_overturned`). The ABS system is the ground truth for these.
- **Pitches that were NOT challenged**: trust the geometric `abs_true_strike`
  flag derived from `plate_x`, `plate_z`, and the per-batter strike zone
  (`sz_top`, `sz_bot`). The ABS rulebook half-width is 8.5 inches = 0.7083 ft.

## Challenge availability

Each team starts a game with **2 challenges**. A successful (overturned)
challenge is **free**; an unsuccessful one decrements the counter. The
classifier walks each game pitch-by-pitch, tracking each side's remaining
challenges before each pitch, so that `miss_penalty` only fires when the
defense actually had a challenge available to use.

## RE288 construction

The 288-cell matrix is `base × out × count`:

- 8 base configurations: `000, 100, 010, 001, 110, 101, 011, 111`
- 3 out states: `0, 1, 2`
- 12 count states: `0-0, 0-1, 0-2, 1-0, 1-1, 1-2, 2-0, 2-1, 2-2, 3-0, 3-1, 3-2`

Built empirically from 2024-2025 regular-season Statcast data. For each
pitch we compute *runs scored by the batting team from this pitch to the end
of the half-inning*, then average across all pitches in the same state.

Implementation note: Statcast's `post_bat_score` is unreliable in 2025+ data;
the builder works around this by deriving total-runs-in-half-inning from the
`bat_score` jump between consecutive same-team half-innings (see
`R/build_re288.R`).

The matrix is validated against five published Tango RE288 cells; cells
within 0.18 runs are considered acceptable (year-over-year run-environment
drift is on the order of 0.05-0.10).

## `bad_challenge` calibration

The fixed −0.057 penalty comes from:

- **Mean catcher gain per `challenge_win`**: ~0.107 runs (computed from the
  RE swing in the count where the catcher correctly challenged).
- **Win rate per challenge attempted**: ~0.532 league-wide (matches Opta's
  published overturn rate).

`0.107 × 0.532 ≈ 0.057`.

This represents the option value of a held challenge: a catcher who burns a
challenge on a real ball is giving up the expected realized gain of using
that challenge later in the same game.

## Data sources & quirks

| Source | Endpoint | Use |
|---|---|---|
| Statcast (current season) | `baseballsavant.mlb.com/statcast_search/csv` | Pitch-level Statcast for 2026 |
| Statcast (historical) | same | 2024-2025 RE288 input — pulled day-by-day to dodge the 25,000-row truncation cap |
| MLB Stats API | `statsapi.mlb.com/api/v1/schedule` | Game discovery, team IDs |
| MLB Stats API | `statsapi.mlb.com/api/v1/game/{pk}/playByPlay` | Per-pitch reviewDetails |
| Chadwick Bat Register | via `baseballr::chadwick_player_lu()` | player_id → name |

**reviewDetails lives in 3 places** in the play-by-play JSON; the scraper
walks all three:

1. `playEvents[[j]].reviewDetails` with a `pitchNumber` (original location)
2. `playEvents[[j]].reviewDetails` without a `pitchNumber` (mid-AB action;
   attribute to the most recent prior pitch)
3. `play.reviewDetails` (at-bat-level; attribute to the last pitch of the AB)

After extraction, all reviews are filtered to `review_type == "MJ"` (ABS
challenges) — the system also surfaces `"MA"` records (manager replay
challenges) that must NOT be included in ABSVA.

## Qualifying threshold

For percentile rankings and rate-stat (`absva_per_100`) leaderboards, the
app filters to catchers with **≥ 50 pitches caught** to avoid backups with
single-digit denominators showing up as outliers.

## Known limitations

- The current scraper pulls Statcast in weekly chunks. Each weekly chunk is
  capped at 25,000 rows; the historical RE288 builder uses daily slices to
  stay well under this cap, but the in-season scraper hasn't been audited
  for the cap (volume per week is well below 25k as of 2026).
- The classifier's challenge counter currently walks pitches sequentially
  per game (a Phase 2 vectorization is planned).
- `overturned_frame` is valued at 0 by spec. An alternative formulation that
  credits the catcher with the swing (since the framing did fool the
  umpire) is under consideration.
