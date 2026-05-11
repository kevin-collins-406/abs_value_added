#!/usr/bin/env Rscript
# =============================================================================
# scripts/00_build_re288.R — Construct the RE288 run-expectancy matrix from
# 2024-2025 historical Statcast data. Run once before the live-season pipeline.
#
# Reads/writes:
#   data/raw/statcast_re288_daily/    daily Savant CSVs (cached, resumable)
#   data/intermediate/statcast_re288_input.parquet
#   data/processed/re288_matrix.parquet
#   data/intermediate/re288_matrix_wide.parquet
# =============================================================================

suppressPackageStartupMessages({
  library(here);   library(arrow);   library(dplyr);  library(tidyr)
  library(purrr);  library(readr);   library(httr);   library(jsonlite)
  library(glue);   library(tibble);  library(lubridate)
})

source(here::here("R", "utils.R"))
source(here::here("R", "build_re288.R"))

ensure_data_dirs()

DAY_DIR    <- data_path("raw",          "statcast_re288_daily")
INPUT_PATH <- data_path("intermediate", "statcast_re288_input.parquet")
OUT_PATH   <- data_path("processed",    "re288_matrix.parquet")
WIDE_PATH  <- data_path("intermediate", "re288_matrix_wide.parquet")
LOG_PATH   <- data_path("raw",          "re288_pull_log.txt")

# 2024 + 2025 regular-season ranges
SEASONS <- list(
  list(year = 2024, start = "2024-03-28", end = "2024-09-29"),
  list(year = 2025, start = "2025-03-27", end = "2025-09-28")
)

log_msg("=== RE288 build start ===", LOG_PATH)

# (1) Pull missing daily slices (resumable; no-op if cache complete).
date_lists <- purrr::map(SEASONS, ~ seq.Date(as.Date(.x$start), as.Date(.x$end), by = "1 day"))
all_dates  <- do.call(c, date_lists)
log_msg(glue("Targeted dates: {length(all_dates)}"), LOG_PATH)
pull_summary <- pull_re288_daily(all_dates, DAY_DIR, log_path = LOG_PATH)
log_msg(glue("Pulled {pull_summary$pulled}, skipped {pull_summary$skipped}, flagged {pull_summary$flagged}"), LOG_PATH)

# (2) Concatenate
log_msg("Concatenating daily files...", LOG_PATH)
all_data <- concatenate_re288_daily(DAY_DIR)
arrow::write_parquet(all_data, INPUT_PATH)
log_msg(glue("Total rows: {nrow(all_data)} -> {INPUT_PATH}"), LOG_PATH)

# (3) Compute runs_remaining per pitch
log_msg("Computing runs-to-end-of-half-inning...", LOG_PATH)
df_with_runs <- compute_runs_remaining(all_data)
log_msg(glue("After cleaning: {nrow(df_with_runs)} rows"), LOG_PATH)

# (4) Aggregate to 288-cell matrix
re288 <- aggregate_re288(df_with_runs)
arrow::write_parquet(re288, OUT_PATH)
arrow::write_parquet(re288_to_wide(re288), WIDE_PATH)

log_msg(glue("Cells: {nrow(re288)} (expected 288). Saved -> {OUT_PATH}"), LOG_PATH)
cat(glue("\nDone. Matrix saved to {OUT_PATH} ({nrow(re288)} cells)\n"))
