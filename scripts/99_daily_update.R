#!/usr/bin/env Rscript
# =============================================================================
# scripts/99_daily_update.R — Daily refresh of ABSVA parquets.
#
# Pipeline (all idempotent / resumable):
#   1. Detect last cached Statcast date from `data/raw/statcast_2026/weekly/`.
#   2. If yesterday <= last cached, exit 0 (no-op).
#   3. Otherwise, delete the most recent (probably partial) weekly file so it
#      re-pulls with current data, then:
#        a. run scripts/01_scrape_season.R   (Statcast + Stats API, resumable)
#        b. run scripts/02_classify.R         (full-season re-classify)
#        c. run scripts/03_score.R            (RE288 apply + leaderboard)
#   4. Append one line to data/raw/.update_log.txt with timestamp + delta rows.
#   5. Exit 0 on success, non-zero on any failure.
#
# Designed for unattended GitHub Actions cron use. Re-running within seconds
# of a successful run is a NO_OP (no log line beyond the no-op record).
# =============================================================================

suppressPackageStartupMessages({
  library(here);   library(arrow);   library(dplyr);  library(glue)
  library(purrr);  library(tibble);  library(lubridate)
})
source(here::here("R", "utils.R"))

SEASON      <- 2026
LOG_PATH    <- data_path("raw", ".update_log.txt")
SC_DIR      <- data_path("raw",          glue("statcast_{SEASON}"), "weekly")
MASTER_PATH <- data_path("intermediate", glue("absva_master_{SEASON}.parquet"))

ts_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

emit <- function(status, body) {
  line <- glue("{ts_iso()} | {status} | {body}")
  log_msg(line, LOG_PATH)
}

# ── 1. Count master rows BEFORE refresh (for delta logging) ─────────────────
n_rows_before <- if (file.exists(MASTER_PATH)) {
  nrow(arrow::read_parquet(MASTER_PATH))
} else {
  0L
}

# ── 2. Determine last cached scrape date from filename pattern ──────────────
target <- Sys.Date() - 1L
sc_files <- list.files(SC_DIR, pattern = "^sc_.*\\.csv$", full.names = TRUE)

if (length(sc_files) > 0) {
  end_re      <- ".*sc_[0-9-]+_([0-9-]+)\\.csv$"
  end_dates   <- as.Date(sub(end_re, "\\1", basename(sc_files)))
  last_cached <- max(end_dates, na.rm = TRUE)
} else {
  last_cached <- NA
  end_dates   <- as.Date(character(0))
}

# ── 3. Idempotency check: cache up to date ──────────────────────────────────
if (!is.na(last_cached) && last_cached >= target) {
  emit("NO_OP", glue("cache up to {last_cached} >= target {target}"))
  quit(status = 0)
}

# ── 4. Drop the most recent (probably partial) weekly file ──────────────────
if (length(sc_files) > 0) {
  latest_file <- sc_files[which.max(end_dates)]
  file.remove(latest_file)
  emit("REFRESH", glue("removed partial weekly file: {basename(latest_file)}"))
}

# ── 5. Run the three pipeline stages in series ──────────────────────────────
run_stage <- function(label, script_path) {
  message(glue("\n=== {label} ==="))
  tryCatch({
    sys.source(script_path, envir = new.env(parent = globalenv()))
    TRUE
  }, error = function(e) {
    emit("FAIL", glue("{label}: {conditionMessage(e)}"))
    FALSE
  })
}

ok <- run_stage("scrape",   here::here("scripts", "01_scrape_season.R")) &&
      run_stage("classify", here::here("scripts", "02_classify.R")) &&
      run_stage("score",    here::here("scripts", "03_score.R"))

if (!ok) quit(status = 1)

# ── 6. Log delta and exit clean ─────────────────────────────────────────────
n_rows_after <- nrow(arrow::read_parquet(MASTER_PATH))
delta        <- n_rows_after - n_rows_before

emit("OK", glue("scraped through {target} | added {delta} rows | total {n_rows_after}"))
quit(status = 0)
