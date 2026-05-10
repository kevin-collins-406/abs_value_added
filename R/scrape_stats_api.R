# =============================================================================
# R/scrape_stats_api.R — MLB Stats API play-by-play scraper.
#
# Walks reviewDetails in THREE locations per the data-source spec:
#   a) playEvents[[j]]$reviewDetails WITH pitchNumber       (original)
#   b) playEvents[[j]]$reviewDetails WITHOUT pitchNumber    (mid-AB action)
#   c) play$reviewDetails                                   (at-bat-level)
# Then filters to review_type == "MJ" (ABS challenges; excludes manager
# replay challenges).
# =============================================================================

build_review_fields <- function(rd, source_label) {
  list(
    has_review     = TRUE,
    review_source  = source_label,
    review_type    = rd$reviewType      %||% NA_character_,
    review_result  = rd$reviewResult    %||% NA_character_,
    abs_result     = rd$absResult       %||% NA_character_,
    is_overturned  = rd$isOverturned    %||% NA,
    challenge_team = rd$challengeTeamId %||% NA_integer_
  )
}

empty_review <- list(
  has_review     = FALSE,
  review_source  = NA_character_,
  review_type    = NA_character_,
  review_result  = NA_character_,
  abs_result     = NA_character_,
  is_overturned  = NA,
  challenge_team = NA_integer_
)

# Extract one tibble of pitch rows from a single play, walking all 3 review
# locations and tagging the source.
extract_pitches <- function(play, gpk) {
  pe_list <- play$playEvents
  if (length(pe_list) == 0) return(tibble::tibble())

  pitch_indices <- which(purrr::map_lgl(pe_list, ~ !is.null(.x$pitchNumber)))
  if (length(pitch_indices) == 0) return(tibble::tibble())

  pitches <- purrr::map_dfr(pitch_indices, function(j) {
    pe <- pe_list[[j]]
    rev <- if (!is.null(pe$reviewDetails)) {
      build_review_fields(pe$reviewDetails, "playEvent.pitch")
    } else {
      empty_review
    }
    tibble::tibble(
      game_pk         = gpk,
      at_bat_number   = play$atBatIndex + 1L,
      pitch_number    = pe$pitchNumber,
      pe_index        = j,
      inning          = play$about$inning %||% NA_integer_,
      half_inning     = play$about$halfInning %||% NA_character_,
      api_description = pe$details$description %||% NA_character_,
      api_code        = pe$details$code %||% NA_character_,
      !!!rev
    )
  })

  # Pass 2: action-level review (no pitchNumber) -> attribute to prior pitch.
  for (j in seq_along(pe_list)) {
    pe <- pe_list[[j]]
    if (is.null(pe$reviewDetails) || !is.null(pe$pitchNumber)) next
    prior_idx <- max(pitch_indices[pitch_indices < j], -Inf)
    if (is.infinite(prior_idx)) next
    rev <- build_review_fields(pe$reviewDetails, "playEvent.action")
    pitches <- pitches |>
      dplyr::mutate(dplyr::across(
        names(rev),
        ~ ifelse(pe_index == prior_idx & !has_review, rev[[dplyr::cur_column()]], .)
      ))
    pitches$has_review[pitches$pe_index == prior_idx & pitches$review_source == "playEvent.action"] <- TRUE
  }

  # Pass 3: play-level review -> attribute to the last pitch.
  if (!is.null(play$reviewDetails)) {
    last_pitch <- max(pitches$pe_index)
    rev <- build_review_fields(play$reviewDetails, "play.level")
    target <- pitches$pe_index == last_pitch & !pitches$has_review
    if (any(target)) {
      for (nm in names(rev)) pitches[[nm]][target] <- rev[[nm]]
    }
  }

  pitches |> dplyr::select(-pe_index)
}

# Discover all completed regular-season games + team ids in a date range.
fetch_schedule <- function(start_date, end_date) {
  url <- glue::glue(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1",
    "&startDate={start_date}&endDate={end_date}&gameType=R"
  )
  sched <- jsonlite::fromJSON(
    httr::content(httr::GET(url), as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  purrr::map_dfr(sched$dates, function(d) {
    purrr::map_dfr(d$games, function(g) {
      tibble::tibble(
        game_pk      = g$gamePk,
        game_date    = as.Date(d$date),
        status       = g$status$abstractGameState %||% NA_character_,
        home_team_id = g$teams$home$team$id      %||% NA_integer_,
        away_team_id = g$teams$away$team$id      %||% NA_integer_
      )
    })
  })
}

# Scrape one game's play-by-play, caching to disk under out_dir/pbp_<pk>.rds.
scrape_one_game <- function(game_pk, out_dir, api_sleep = 0.25, log_path = NULL) {
  out_file <- file.path(out_dir, glue::glue("pbp_{game_pk}.rds"))
  if (file.exists(out_file)) return(readRDS(out_file))

  url <- glue::glue("https://statsapi.mlb.com/api/v1/game/{game_pk}/playByPlay")
  res <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (is.null(res) || httr::http_error(res)) {
    log_msg(glue::glue("    FAIL game_pk={game_pk}"), log_path)
    return(NULL)
  }
  pbp <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"),
                            simplifyVector = FALSE)
  rows <- purrr::map_dfr(pbp$allPlays, ~ extract_pitches(.x, game_pk))
  saveRDS(rows, out_file)
  Sys.sleep(api_sleep)
  rows
}

# Scrape many games. If `furrr` is installed and `workers > 1`, runs in
# parallel with a reduced per-call sleep (so total QPS stays polite). Falls
# back to sequential purrr::map_dfr otherwise.
#
# Default 4 workers × 0.10s sleep ≈ same QPS as 1 worker × 0.25s, so this
# stays inside MLB's politeness budget while finishing 4× faster.
scrape_games_parallel <- function(game_pks, out_dir, workers = 4,
                                  parallel_sleep = 0.10, sequential_sleep = 0.25,
                                  log_path = NULL) {
  if (workers > 1 && requireNamespace("furrr", quietly = TRUE) &&
      requireNamespace("future", quietly = TRUE)) {
    log_msg(glue::glue("Parallel scrape: {workers} workers, {parallel_sleep}s sleep"), log_path)
    old_plan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)
    rows <- furrr::future_map_dfr(
      game_pks,
      function(gpk) scrape_one_game(gpk, out_dir, api_sleep = parallel_sleep, log_path = log_path),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    log_msg(glue::glue("Sequential scrape ({sequential_sleep}s sleep)"), log_path)
    rows <- purrr::map_dfr(seq_along(game_pks), function(i) {
      if (i %% 50 == 0) log_msg(glue::glue("  progress: {i}/{length(game_pks)}"), log_path)
      scrape_one_game(game_pks[i], out_dir, api_sleep = sequential_sleep, log_path = log_path)
    })
  }
  rows
}

# Filter raw reviews to ABS challenges only (review_type == "MJ").
filter_to_abs_challenges <- function(df) {
  df |>
    dplyr::mutate(
      .is_abs = has_review & review_type == "MJ",
      has_review     = dplyr::if_else(.is_abs, has_review,     FALSE),
      review_source  = dplyr::if_else(.is_abs, review_source,  NA_character_),
      review_type    = dplyr::if_else(.is_abs, review_type,    NA_character_),
      review_result  = dplyr::if_else(.is_abs, review_result,  NA_character_),
      abs_result     = dplyr::if_else(.is_abs, abs_result,     NA_character_),
      is_overturned  = dplyr::if_else(.is_abs, is_overturned,  NA),
      challenge_team = dplyr::if_else(.is_abs, challenge_team, NA_integer_)
    ) |>
    dplyr::select(-.is_abs)
}

# Add the geometric ABS-zone strike flag (half-width = 0.7083 ft per ABS rules).
add_abs_true_strike <- function(master, half_width = 0.7083) {
  master |>
    dplyr::mutate(
      abs_true_strike = !is.na(plate_x) & !is.na(plate_z) &
                        !is.na(sz_top)  & !is.na(sz_bot)  &
                        abs(plate_x) <= half_width &
                        plate_z >= sz_bot & plate_z <= sz_top
    )
}
