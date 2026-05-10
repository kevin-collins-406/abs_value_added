#!/usr/bin/env Rscript
# =============================================================================
# scripts/03_score.R — Apply RE288 to classified events; write per-event
# scored parquet and per-catcher leaderboard.
#
# Reads:
#   data/processed/re288_matrix.parquet
#   data/intermediate/absva_classified_2026.parquet
# Writes:
#   data/processed/absva_scored_2026.parquet
#   data/processed/absva_leaderboard_2026.parquet
# =============================================================================

suppressPackageStartupMessages({
  library(here);   library(arrow);   library(dplyr);  library(tidyr)
  library(purrr);  library(stringr); library(glue);   library(tibble)
})

source(here::here("R", "utils.R"))
source(here::here("R", "score_absva.R"))

RE288_PATH       <- data_path("processed",    "re288_matrix.parquet")
EVENTS_PATH      <- data_path("intermediate", "absva_classified_2026.parquet")
SCORED_PATH      <- data_path("processed",    "absva_scored_2026.parquet")
LEADERBOARD_PATH <- data_path("processed",    "absva_leaderboard_2026.parquet")

re288  <- arrow::read_parquet(RE288_PATH)
events <- arrow::read_parquet(EVENTS_PATH)

cat(glue("RE288 cells: {nrow(re288)}    Events: {nrow(events)}\n\n"))

scored <- compute_event_run_values(events, re288)
cat(glue("Events scored: {nrow(scored)}\n"))

leaderboard <- aggregate_to_leaderboard(scored, events)

# Slim the per-event parquet to what the Shiny app actually uses (147 -> ~28
# columns). The leaderboard parquet is already small enough.
scored_slim <- slim_for_app(scored)
cat(glue("Slim: {ncol(scored)} cols -> {ncol(scored_slim)} cols\n"))

arrow::write_parquet(scored_slim, SCORED_PATH)
arrow::write_parquet(leaderboard, LEADERBOARD_PATH)

cat(glue("\nSaved:\n  {SCORED_PATH}\n  {LEADERBOARD_PATH}\n"))

cat("\n=== Run value distribution by event type ===\n")
print(scored |>
        dplyr::group_by(event_type) |>
        dplyr::summarise(
          n      = dplyr::n(),
          mean   = round(mean(event_run_value, na.rm = TRUE), 4),
          median = round(median(event_run_value, na.rm = TRUE), 4),
          .groups = "drop"
        ))

cat("\n=== Top 10 catchers by ABSVA (min 50 pitches) ===\n")
print(leaderboard |>
        dplyr::filter(pitches_caught >= 50) |>
        dplyr::select(catcher_name, pitches_caught, absva, absva_per_100) |>
        head(10))
