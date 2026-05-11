#!/usr/bin/env Rscript
# =============================================================================
# scripts/01_scrape_season.R — Scrape current-season Statcast + Stats API data,
# join into a master parquet at data/intermediate/.
#
# Resumable: skips Statcast weeks and per-game pbp files that already exist.
# =============================================================================

suppressPackageStartupMessages({
  library(here);   library(arrow);   library(dplyr);  library(tidyr)
  library(purrr);  library(readr);   library(httr);   library(jsonlite)
  library(glue);   library(tibble);  library(lubridate)
})

source(here::here("R", "utils.R"))
source(here::here("R", "scrape_statcast.R"))
source(here::here("R", "scrape_stats_api.R"))

ensure_data_dirs()

# ── Configuration ───────────────────────────────────────────────────────────
SEASON       <- 2026
SEASON_START <- as.Date("2026-03-27")
SEASON_END   <- min(Sys.Date() - 1, as.Date(glue("{SEASON}-10-31")))

SC_DIR      <- data_path("raw",          glue("statcast_{SEASON}"), "weekly")
PBP_DIR     <- data_path("raw",          glue("stats_api_{SEASON}"), "games")
MASTER_PATH <- data_path("intermediate", glue("absva_master_{SEASON}.parquet"))
LOG_PATH    <- data_path("raw",          glue("scraper_log_{SEASON}.txt"))

dir.create(SC_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(PBP_DIR, showWarnings = FALSE, recursive = TRUE)

log_msg(glue("=== ABSVA scrape | {SEASON_START} -> {SEASON_END} ==="), LOG_PATH)

# ── (1) Statcast weekly pulls ───────────────────────────────────────────────
pull_statcast_weeks(SEASON_START, SEASON_END, SEASON, SC_DIR, log_path = LOG_PATH)

# ── (2) Discover games ──────────────────────────────────────────────────────
log_msg("Discovering game_pks...", LOG_PATH)
all_games <- fetch_schedule(SEASON_START, SEASON_END)
completed <- all_games |> dplyr::filter(status == "Final")
log_msg(glue("Completed games: {nrow(completed)}"), LOG_PATH)

# ── (3) Per-game scrape (parallel if furrr available) ───────────────────────
log_msg(glue("Stats API: scraping {nrow(completed)} games..."), LOG_PATH)
pbp_all <- scrape_games_parallel(completed$game_pk, PBP_DIR,
                                 workers = 4, log_path = LOG_PATH)
log_msg(glue("Extracted {nrow(pbp_all)} pitches; raw reviews: {sum(pbp_all$has_review, na.rm=TRUE)}"), LOG_PATH)

# ── (4) Filter to ABS challenges (review_type == "MJ") ──────────────────────
pbp_all <- filter_to_abs_challenges(pbp_all)
log_msg(glue("Reviews after MJ filter: {sum(pbp_all$has_review)}"), LOG_PATH)

# ── (5) Concatenate Statcast and join ───────────────────────────────────────
log_msg("Concatenating Statcast...", LOG_PATH)
sc_all <- concat_statcast(SC_DIR)
log_msg(glue("Statcast rows: {nrow(sc_all)}"), LOG_PATH)

api_join_cols <- pbp_all |>
  dplyr::select(game_pk, at_bat_number, pitch_number,
                api_description, api_code,
                has_review, review_source, review_type, review_result,
                abs_result, is_overturned, challenge_team)

master <- sc_all |>
  dplyr::left_join(api_join_cols, by = c("game_pk", "at_bat_number", "pitch_number")) |>
  add_abs_true_strike()

arrow::write_parquet(master, MASTER_PATH)
log_msg(glue("Master saved -> {MASTER_PATH} ({nrow(master)} rows)"), LOG_PATH)

cat(glue("\n=== Summary ===\n",
         "Games:      {nrow(completed)}\n",
         "Pitches:    {nrow(master)}\n",
         "Challenges: {sum(master$has_review %||% FALSE, na.rm = TRUE)}\n"))
