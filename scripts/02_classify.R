#!/usr/bin/env Rscript
# =============================================================================
# scripts/02_classify.R — Classify each pitch into one of the 5 ABSVA event
# types and attach catcher/batter/pitcher names.
#
# Reads:  data/intermediate/absva_master_2026.parquet
# Writes: data/intermediate/absva_classified_2026.parquet
# =============================================================================

suppressPackageStartupMessages({
  library(here);   library(arrow);   library(dplyr);  library(tidyr)
  library(purrr);  library(httr);    library(jsonlite)
  library(glue);   library(tibble)
})

source(here::here("R", "utils.R"))
source(here::here("R", "classify_events.R"))

INPUT_PATH    <- data_path("intermediate", "absva_master_2026.parquet")
OUTPUT_PATH   <- data_path("intermediate", "absva_classified_2026.parquet")
CHADWICK_PATH <- data_path("raw",          "chadwick_player_lookup.rds")

cat(glue("Loading {INPUT_PATH}...\n"))
master <- arrow::read_parquet(INPUT_PATH)
cat(glue("  Rows: {nrow(master)}    Cols: {ncol(master)}\n\n"))

# (1) Map description -> original_call
master <- master |> add_original_call()

# (2) Build team lookup, attach defense/offense team ids
cat("Building team lookup from Stats API schedule...\n")
team_lookup <- fetch_team_lookup(range(master$game_date))
master <- master |>
  dplyr::left_join(team_lookup, by = "game_pk") |>
  add_team_sides()

# (3) Per-game challenge counters
cat("Computing per-game challenge counters...\n")
master <- master |> add_challenge_counter()

# (4) Determine challenger side
master <- master |> add_challenger_side()

# (5) Classify events
master <- master |> classify_event_type()

# (6) Attach player names
cat("Attaching player names...\n")
player_lookup <- load_player_lookup(CHADWICK_PATH)
master <- master |> add_player_names(player_lookup)

# (7) Save
arrow::write_parquet(master, OUTPUT_PATH)
cat(glue("\nSaved -> {OUTPUT_PATH}\n"))

# Diagnostics
cat("\n=== Event-type distribution ===\n")
print(master |>
        dplyr::count(event_type, sort = TRUE) |>
        dplyr::mutate(pct = round(100 * n / sum(n), 2)))

cat("\n=== Challenge classification ===\n")
print(master |> dplyr::filter(has_review) |> dplyr::count(event_type, sort = TRUE))
cat(glue("\nTotal challenges: {sum(master$has_review, na.rm = TRUE)}\n"))
