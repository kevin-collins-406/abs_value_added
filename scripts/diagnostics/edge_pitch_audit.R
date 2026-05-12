#!/usr/bin/env Rscript
# =============================================================================
# scripts/diagnostics/edge_pitch_audit.R
#
# Diagnostic Run 1 — Edge pitch distribution audit.
#
# Hypothesis: frame events (true ball -> called strike) should cluster
# WITHIN ~3 inches of the strike zone edge. Pitches landing 6+ inches outside
# the zone are not realistic framing — no MLB umpire would call them strikes.
# If many frame events have large `dist_outside`, ABSVA is crediting
# catchers for calls that aren't really attributable to framing. Same logic
# (inverted) for missed_ABS_opportunity events landing far INSIDE the zone.
#
# Reads:  data/processed/absva_scored_2026.parquet
# Writes: stdout summary + data/intermediate/edge_audit_suspicious.csv
# =============================================================================

suppressPackageStartupMessages({
  library(here);  library(arrow); library(dplyr); library(glue); library(tibble)
})
source(here::here("R", "utils.R"))
ensure_data_dirs()

ABS_HALF_WIDTH <- 0.7083   # ft (= 8.5 in, ABS rule book)

events <- arrow::read_parquet(data_path("processed", "absva_scored_2026.parquet"))
cat(glue("Loaded {nrow(events)} scored events.\n\n"))

# Signed distance to the nearest strike-zone edge (inches).
#   positive = outside the zone
#   negative = inside the zone
ev <- events |>
  dplyr::filter(!is.na(plate_x), !is.na(plate_z), !is.na(sz_top), !is.na(sz_bot)) |>
  dplyr::mutate(
    dx_out = pmax(0, abs(plate_x) - ABS_HALF_WIDTH),
    dz_out = pmax(0, sz_bot - plate_z, plate_z - sz_top),
    outside_in    = sqrt(dx_out^2 + dz_out^2) * 12,
    inside_margin = pmin(ABS_HALF_WIDTH - abs(plate_x),
                         plate_z - sz_bot, sz_top - plate_z) * 12,
    is_inside = outside_in == 0,
    signed_in = ifelse(is_inside, -inside_margin, outside_in)
  )

# ── Summary table ───────────────────────────────────────────────────────────
cat("=== Signed distance to zone edge, by event type (inches) ===\n")
cat("    positive = pitch outside zone, negative = inside zone\n\n")
print(ev |>
  dplyr::group_by(event_type) |>
  dplyr::summarise(
    n      = dplyr::n(),
    median = round(median(signed_in), 2),
    mean   = round(mean(signed_in),   2),
    p10    = round(quantile(signed_in, 0.10), 2),
    p90    = round(quantile(signed_in, 0.90), 2),
    .groups = "drop"
  ))

# ── frame: how far OUTSIDE the zone? ────────────────────────────────
cat("\n=== frame — distance OUTSIDE zone (inches) ===\n")
fs <- ev |> dplyr::filter(event_type == "frame")
buckets <- c(0, 1, 2, 3, 4, 6, 8, 12, Inf)
print(tibble::tibble(
  bucket = levels(cut(fs$outside_in, buckets, right = FALSE)),
  n      = as.integer(table(cut(fs$outside_in, buckets, right = FALSE))),
  pct    = round(100 * as.numeric(table(cut(fs$outside_in, buckets, right = FALSE))) / nrow(fs), 1)
))
cat(glue("\nWithin 3in of zone edge: {sum(fs$outside_in < 3)} of {nrow(fs)} ({round(100*mean(fs$outside_in < 3),1)}%)\n"))
cat(glue(">= 6in outside (suspicious): {sum(fs$outside_in >= 6)} ({round(100*mean(fs$outside_in >= 6),1)}%)\n"))
sus_rv <- sum(fs$event_run_value[fs$outside_in >= 6], na.rm = TRUE)
total_rv <- sum(fs$event_run_value, na.rm = TRUE)
cat(glue("Run value attributed to suspicious framing: {round(sus_rv, 2)} of {round(total_rv, 2)} total ({round(100*sus_rv/total_rv, 1)}%)\n"))

# ── missed_ABS_opportunity: how far INSIDE the zone? ──────────────────────────────────
cat("\n=== missed_ABS_opportunity — distance INSIDE zone (inches) ===\n")
mp <- ev |>
  dplyr::filter(event_type == "missed_ABS_opportunity") |>
  dplyr::mutate(inside_in = -signed_in)
mp_buckets <- c(0, 1, 2, 3, 4, 6, Inf)
print(tibble::tibble(
  bucket = levels(cut(mp$inside_in, mp_buckets, right = FALSE)),
  n      = as.integer(table(cut(mp$inside_in, mp_buckets, right = FALSE))),
  pct    = round(100 * as.numeric(table(cut(mp$inside_in, mp_buckets, right = FALSE))) / nrow(mp), 1)
))
cat(glue("\nWithin 3in of zone edge: {sum(mp$inside_in < 3)} of {nrow(mp)} ({round(100*mean(mp$inside_in < 3),1)}%)\n"))
cat(glue(">= 6in inside (suspicious): {sum(mp$inside_in >= 6)} ({round(100*mean(mp$inside_in >= 6),1)}%)\n"))

# ── Top 20 worst-offender frame pitches ─────────────────────────────
cat("\n=== Top 20 frame events farthest outside the zone ===\n")
top <- fs |>
  dplyr::arrange(dplyr::desc(outside_in)) |>
  dplyr::select(game_date, catcher_name, pitcher_name, batter_name,
                plate_x, plate_z, sz_top, sz_bot,
                balls, strikes, outside_in, event_run_value) |>
  head(20)
print(top, n = 20)

# ── Persist the suspicious set for downstream review ────────────────────────
sus_path <- data_path("intermediate", "edge_audit_suspicious.csv")
fs |>
  dplyr::filter(outside_in >= 6) |>
  dplyr::arrange(dplyr::desc(outside_in)) |>
  dplyr::select(game_pk, game_date, catcher_name, pitcher_name, batter_name,
                plate_x, plate_z, sz_top, sz_bot,
                balls, strikes, outside_in, event_run_value) |>
  utils::write.csv(sus_path, row.names = FALSE)
cat(glue("\nSuspicious framing rows -> {sus_path}\n"))
