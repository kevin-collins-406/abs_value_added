# =============================================================================
# R/build_re288.R — Construct the RE288 (24 base-out × 12 count) run-expectancy
# matrix from 2024-2025 historical Statcast pitch data.
#
# RE288 = expected runs scored from now to the end of the half-inning, given
# the current (base, out, balls, strikes) state.
#
# Pulled day-by-day to dodge Savant's 25,000-row truncation cap.
# =============================================================================

# Per-day Savant Statcast URL.
build_re288_day_url <- function(date) {
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfGT=R%7C",
    "&hfSea=", lubridate::year(as.Date(date)), "%7C",
    "&player_type=batter",
    "&game_date_gt=", date,
    "&game_date_lt=", date,
    "&min_pitches=0&min_results=0&min_abs=0",
    "&type=details"
  )
}

RE288_KEEP_COLS <- c(
  "game_pk", "game_year", "game_date", "game_type",
  "inning", "inning_topbot",
  "at_bat_number", "pitch_number",
  "balls", "strikes",
  "on_1b", "on_2b", "on_3b",
  "outs_when_up",
  "bat_score",
  "description", "events"
)

RE288_COL_TYPES <- function() {
  readr::cols_only(
    game_pk        = readr::col_double(),
    game_year      = readr::col_integer(),
    game_date      = readr::col_date(),
    game_type      = readr::col_character(),
    inning         = readr::col_integer(),
    inning_topbot  = readr::col_character(),
    at_bat_number  = readr::col_integer(),
    pitch_number   = readr::col_integer(),
    balls          = readr::col_integer(),
    strikes        = readr::col_integer(),
    on_1b          = readr::col_double(),
    on_2b          = readr::col_double(),
    on_3b          = readr::col_double(),
    outs_when_up   = readr::col_integer(),
    bat_score      = readr::col_integer(),
    description    = readr::col_character(),
    events         = readr::col_character()
  )
}

# Pull each day's Statcast slice into out_dir (resumable). Returns counts.
pull_re288_daily <- function(dates, out_dir, sleep_sec = 0.5, log_path = NULL) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  n_pulled <- 0L; n_skipped <- 0L; n_flagged <- 0L
  purrr::walk(dates, function(d) {
    out_file <- file.path(out_dir, glue::glue("sc_{d}.csv"))
    if (file.exists(out_file)) {
      n_skipped <<- n_skipped + 1L
      return(invisible())
    }
    url <- build_re288_day_url(d)
    res <- tryCatch(
      readr::read_csv(url, show_col_types = FALSE, progress = FALSE),
      error = function(e) {
        log_msg(glue::glue("  FAIL {d}: {conditionMessage(e)}"), log_path); NULL
      }
    )
    if (is.null(res)) return(invisible())
    res <- res |> dplyr::select(dplyr::any_of(RE288_KEEP_COLS))
    if (nrow(res) >= 25000) {
      log_msg(glue::glue("  WARN {d}: returned {nrow(res)} rows (truncation cap hit)"), log_path)
      n_flagged <<- n_flagged + 1L
    }
    readr::write_csv(res, out_file)
    n_pulled <<- n_pulled + 1L
    if (n_pulled %% 25 == 0) {
      log_msg(glue::glue("  progress: pulled {n_pulled}, skipped {n_skipped}"), log_path)
    }
    Sys.sleep(sleep_sec)
  })
  list(pulled = n_pulled, skipped = n_skipped, flagged = n_flagged)
}

# Concatenate all daily CSVs in a directory using fixed col types.
concatenate_re288_daily <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  ct <- RE288_COL_TYPES()
  reads <- purrr::map(files, function(f) {
    tryCatch(
      readr::read_csv(f, col_types = ct, progress = FALSE),
      error   = function(e) NULL,
      warning = function(w) suppressWarnings(readr::read_csv(f, col_types = ct, progress = FALSE))
    )
  })
  dplyr::bind_rows(reads)
}

# Compute runs-to-end-of-half-inning for each pitch (preserving Phase 1 logic;
# Phase 2 will vectorize the half-inning lookup).
compute_runs_remaining <- function(df) {
  df <- df |>
    dplyr::mutate(
      base_state     = base_state_string(on_1b, on_2b, on_3b),
      count_state    = paste0(balls, "-", strikes),
      half_inning_id = paste(game_pk, inning, inning_topbot, sep = "_")
    ) |>
    dplyr::filter(
      !is.na(balls), !is.na(strikes),
      balls %in% 0:3, strikes %in% 0:2,
      !is.na(outs_when_up), outs_when_up %in% 0:2,
      !is.na(bat_score)
    )

  df <- df |>
    dplyr::arrange(game_pk, inning, dplyr::desc(inning_topbot), at_bat_number, pitch_number) |>
    dplyr::group_by(half_inning_id) |>
    dplyr::mutate(
      half_start_score     = dplyr::first(bat_score),
      runs_already_in_half = bat_score - half_start_score
    ) |>
    dplyr::ungroup()

  half_totals <- df |>
    dplyr::group_by(game_pk, inning, inning_topbot) |>
    dplyr::summarise(
      half_inning_id       = dplyr::first(half_inning_id),
      half_start_score     = dplyr::first(half_start_score),
      last_pitch_bat_score = dplyr::last(bat_score),
      .groups = "drop"
    ) |>
    dplyr::arrange(game_pk, inning_topbot, inning) |>
    dplyr::group_by(game_pk, inning_topbot) |>
    dplyr::mutate(
      next_half_start_score = dplyr::lead(half_start_score),
      total_runs_in_half    = pmax(0, next_half_start_score - half_start_score)
    ) |>
    dplyr::ungroup()

  df |>
    dplyr::left_join(
      half_totals |> dplyr::select(half_inning_id, total_runs_in_half),
      by = "half_inning_id"
    ) |>
    dplyr::filter(!is.na(total_runs_in_half)) |>
    dplyr::mutate(runs_remaining = total_runs_in_half - runs_already_in_half) |>
    dplyr::filter(runs_remaining >= 0)
}

# Aggregate per-pitch runs_remaining to a 288-cell RE matrix.
aggregate_re288 <- function(df) {
  df |>
    dplyr::group_by(base_state, outs_when_up, count_state) |>
    dplyr::summarise(
      n_pitches = dplyr::n(),
      re_value  = mean(runs_remaining),
      .groups = "drop"
    ) |>
    dplyr::arrange(outs_when_up, base_state, count_state)
}

# Pivot a long matrix to wide (count_state in columns) for human inspection.
re288_to_wide <- function(re288) {
  re288 |>
    dplyr::select(-n_pitches) |>
    tidyr::pivot_wider(names_from = count_state, values_from = re_value) |>
    dplyr::arrange(outs_when_up, base_state)
}
