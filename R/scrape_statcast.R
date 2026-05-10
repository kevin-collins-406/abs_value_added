# =============================================================================
# R/scrape_statcast.R — Pull regular-season Statcast pitch data from
# baseballsavant.mlb.com (CSV endpoint), in weekly slices.
# =============================================================================

build_statcast_url <- function(start_date, end_date, season) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfGT=R%7CPO%7CS%7CE%7C",
    "&hfSea=", season, "%7C",
    "&player_type=batter",
    "&game_date_gt=", start_date,
    "&game_date_lt=", end_date,
    "&min_pitches=0&min_results=0&min_abs=0",
    "&type=details"
  )
}

# Pull weekly slices into out_dir; resumable (skips files already present).
pull_statcast_weeks <- function(start_date, end_date, season, out_dir,
                                sleep_sec = 1, log_path = NULL) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  weeks <- tibble::tibble(start = seq.Date(start_date, end_date, by = "7 days"))
  weeks$end <- pmin(weeks$start + 6, end_date)

  log_msg(glue::glue("Statcast: {nrow(weeks)} weekly chunks."), log_path)

  purrr::walk2(weeks$start, weeks$end, function(s, e) {
    out_file <- file.path(out_dir, glue::glue("sc_{s}_{e}.csv"))
    if (file.exists(out_file)) {
      log_msg(glue::glue("  skip {s} -> {e} (cached)"), log_path)
      return(invisible())
    }
    log_msg(glue::glue("  pull {s} -> {e}..."), log_path)
    url <- build_statcast_url(s, e, season)
    res <- tryCatch(
      readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
      error = function(err) {
        log_msg(glue::glue("    FAIL: {conditionMessage(err)}"), log_path); NULL
      }
    )
    if (!is.null(res)) {
      readr::write_csv(res, out_file)
      log_msg(glue::glue("    OK: {nrow(res)} rows"), log_path)
    }
    Sys.sleep(sleep_sec)
  })
  invisible(weeks)
}

# Concatenate all CSVs in a directory into one tibble.
concat_statcast <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  purrr::map_dfr(files, ~ readr::read_csv(.x, show_col_types = FALSE, progress = FALSE))
}
