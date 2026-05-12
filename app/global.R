# =============================================================================
# global.R — ABSVA Dashboard
# =============================================================================
# Loaded once at app startup. Defines:
#   - Data loading (events + leaderboard parquets)
#   - Color palette and event metadata
#   - League percentile pre-computation
#   - Helper functions used across ui/server
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(plotly)
  library(htmltools)
  library(here)
})


# ── 1. Theme / palette ───────────────────────────────────────────────────────

# Dark athletic aesthetic, matching the mockup
THEME <- list(
  bg_app       = "#0F1419",
  bg_card      = "#1A2027",
  bg_card_hi   = "#142028",
  border       = "#2A3441",
  text_primary = "#F3F4F6",
  text_muted   = "#9CA3AF",
  text_dim     = "#6B7280",
  text_faint   = "#4B5563"
)

# Event-type colors (broadcast palette: red = strike secured, blue = strike lost)
EVENT_COLORS <- c(
  frame = "#E24B4A",   # mid red
  challenge_win = "#A32D2D",   # dark red
  overturned_frame = "#888780",   # gray (zero RV)
  missed_ABS_opportunity  = "#378ADD",   # mid blue
  challenge_lost = "#185FA5"    # dark blue
)

# Display labels for events
EVENT_LABELS <- c(
  frame = "Frame",
  challenge_win = "Challenge win",
  overturned_frame = "Overturned frame",
  missed_ABS_opportunity  = "Missed ABS Opportunity",
  challenge_lost = "Challenge Lost"
)

# Sign per event type (for color rules in tables)
EVENT_SIGN <- c(
  frame =  1L,
  challenge_win =  1L,
  overturned_frame =  0L,
  missed_ABS_opportunity  = -1L,
  challenge_lost = -1L
)


# ── 2. Load data ─────────────────────────────────────────────────────────────

leaderboard_raw <- read_parquet(here("data", "processed", "absva_leaderboard_2026.parquet"))
events_raw      <- read_parquet(here("data", "processed", "absva_scored_2026.parquet"))

cat(sprintf("[global.R] Loaded leaderboard: %d catchers\n", nrow(leaderboard_raw)))
cat(sprintf("[global.R] Loaded events:      %d rows\n",   nrow(events_raw)))


# ── 3. League percentile pre-computation ─────────────────────────────────────
# Filter to qualifying catchers (50+ pitches caught) for percentile ranks.
# This avoids backups with 5 pitches showing as "100th percentile" outliers.

MIN_PITCHES_QUALIFY <- 50

leaderboard <- leaderboard_raw %>%
  filter(pitches_caught >= MIN_PITCHES_QUALIFY) %>%
  mutate(
    frame_per100        = 100 * rv_frame       / pitches_caught,
    challenge_win_per100  = 100 * rv_challenge_win / pitches_caught,
    missed_ABS_opportunity_per100   = 100 * rv_missed_ABS_opportunity  / pitches_caught,
    challenge_lost_per100  = 100 * rv_challenge_lost / pitches_caught,
    # Rate metrics for "skill-only" percentiles
    challenge_total       = n_challenge_win + n_challenge_lost,
    challenge_win_rate    = ifelse(challenge_total > 0,
                                   n_challenge_win / challenge_total,
                                   NA_real_),
    challenge_lost_rate    = 100 * n_challenge_lost / pitches_caught,
    missed_ABS_opportunity_rate     = 100 * n_missed_ABS_opportunity  / pitches_caught
  )

# Helper: percentile within a vector (1-99), where higher value = higher pct.
# For "lower-is-better" stats we'll invert before calling this.
pct_rank <- function(x) {
  pmax(1L, pmin(99L, as.integer(round(100 * rank(x, ties.method = "average") / length(x)))))
}

leaderboard <- leaderboard %>%
  mutate(
    pct_absva          = pct_rank(absva),
    pct_absva_per100   = pct_rank(absva_per_100),
    pct_frame        = pct_rank(frame_per100),
    pct_challenge_rate = pct_rank(coalesce(challenge_win_rate, 0)),
    # For "avoid bad challenges" and "avoid miss penalties":
    # lower rate is better, so invert.
    pct_avoid_bad      = pct_rank(-challenge_lost_rate),
    pct_avoid_miss     = pct_rank(-missed_ABS_opportunity_rate)
  )

cat(sprintf("[global.R] Qualifying catchers (%d+ pitches): %d\n",
            MIN_PITCHES_QUALIFY, nrow(leaderboard)))


# ── 4. League average breakdown (per-100 basis, for "vs avg" column) ─────────
# Used in the modal's event-breakdown table.

LEAGUE_AVG_PER100 <- list(
  frame = mean(100 * leaderboard$rv_frame       / leaderboard$pitches_caught, na.rm = TRUE),
  challenge_win = mean(100 * leaderboard$rv_challenge_win / leaderboard$pitches_caught, na.rm = TRUE),
  missed_ABS_opportunity  = mean(100 * leaderboard$rv_missed_ABS_opportunity  / leaderboard$pitches_caught, na.rm = TRUE),
  challenge_lost = mean(100 * leaderboard$rv_challenge_lost / leaderboard$pitches_caught, na.rm = TRUE),
  total         = mean(leaderboard$absva_per_100, na.rm = TRUE)
)


# ── 5. Helper: format a signed number for display ────────────────────────────

fmt_signed <- function(x, digits = 1) {
  if (is.na(x)) return("—")
  sign_chr <- if (x > 0) "+" else if (x < 0) "−" else ""
  paste0(sign_chr, format(abs(round(x, digits)), nsmall = digits))
}

# Color a number based on sign — used for inline HTML in the leaderboard
fmt_signed_html <- function(x, digits = 1) {
  if (is.na(x)) return("<span style='color:#6B7280'>—</span>")
  color <- if (x > 0) THEME$ev_pos else if (x < 0) THEME$ev_neg else THEME$text_muted
  color <- if (x > 0) "#E24B4A" else if (x < 0) "#378ADD" else "#6B7280"
  sprintf("<span style='color:%s;font-variant-numeric:tabular-nums;'>%s</span>",
          color, fmt_signed(x, digits))
}


# ── 6. Helper: filter events for a given catcher (used in modal) ─────────────

events_for_catcher <- function(fielder_id) {
  events_raw %>%
    filter(fielder_2 == fielder_id,
           event_type %in% names(EVENT_COLORS))
}


# ── 7. ABS rule book strike zone constants ───────────────────────────────────
# Half-width: 8.5 inches = 0.7083 ft (each side of plate center)
# Vertical: per-batter sz_top / sz_bot from Statcast

ABS_HALF_WIDTH    <- 0.7083
BORDERLINE_BUFFER <- 0.25     # ~3 inches around the zone is "borderline"


cat("[global.R] Setup complete.\n")
