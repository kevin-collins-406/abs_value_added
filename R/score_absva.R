# =============================================================================
# R/score_absva.R — Apply RE288 to ABSVA events to compute per-pitch run
# values, then aggregate to per-catcher leaderboards.
#
# Run-value formulas (catcher perspective):
#   framing_steal   RV = RE(after_ball) - RE(after_strike)   (positive)
#   challenge_win   RV = RE(after_ball) - RE(after_strike)   (positive)
#   overturned_frame   RV = 0                                   (per spec)
#   miss_penalty    RV = RE(after_strike) - RE(after_ball)   (negative)
#   bad_challenge   RV = -0.057                              (fixed penalty)
#
# (RE288 is offense-perspective; catcher RV is the offense impact, negated.)
# =============================================================================

BAD_CHALLENGE_PENALTY <- -0.057   # = mean(challenge_win RV) × overturn_rate

# Columns the Shiny app needs from the scored parquet. Anything outside this
# whitelist is dropped by slim_for_app() — keeps the parquet small and app
# startup fast. Add columns here if the app starts referencing new fields.
APP_KEEP_COLS <- c(
  # Identifiers
  "game_pk", "game_date", "at_bat_number", "pitch_number",
  "fielder_2", "catcher_name", "batter_name", "pitcher_name",
  # Event + run value
  "event_type", "event_run_value",
  "re_after_strike", "re_after_ball",
  # Pitch geometry / count / outs / bases
  "plate_x", "plate_z", "sz_top", "sz_bot",
  "balls", "strikes", "outs_when_up",
  "on_1b", "on_2b", "on_3b", "base_state",
  # Game context (needed for the modal's game-filter dropdown)
  "home_team", "away_team", "inning_topbot",
  # Challenge metadata (for modal context)
  "has_review", "is_overturned", "abs_result", "review_type",
  # Geometric ABS-zone flag
  "abs_true_strike"
)

# Drop columns not in APP_KEEP_COLS. Tolerates missing columns (from upstream
# schema drift) by using any_of().
slim_for_app <- function(scored) {
  scored |> dplyr::select(dplyr::any_of(APP_KEEP_COLS))
}

add_strike <- function(count) {
  m <- stringr::str_split_fixed(count, "-", 2)
  balls   <- as.integer(m[, 1])
  strikes <- as.integer(m[, 2])
  dplyr::if_else(strikes >= 2, "strikeout", paste0(balls, "-", strikes + 1L))
}

add_ball <- function(count) {
  m <- stringr::str_split_fixed(count, "-", 2)
  balls   <- as.integer(m[, 1])
  strikes <- as.integer(m[, 2])
  dplyr::if_else(balls >= 3, "walk", paste0(balls + 1L, "-", strikes))
}

# Walk advancement: forced runners advance, non-forced stay.
advance_walk <- function(base_state) {
  dplyr::case_when(
    base_state == "000" ~ "100",
    base_state == "100" ~ "110",
    base_state == "010" ~ "110",
    base_state == "001" ~ "101",
    base_state == "110" ~ "111",
    base_state == "101" ~ "111",
    base_state == "011" ~ "111",
    base_state == "111" ~ "111",
    TRUE                ~ NA_character_
  )
}

walk_runs_scored <- function(base_state) {
  dplyr::if_else(base_state == "111", 1, 0)
}

# Vectorized RE288 lookup table.
make_re_lookup <- function(re288) {
  re288 |> dplyr::select(base_state, outs_when_up, count_state, re_value)
}

get_re <- function(base_state, outs, count, re_lookup) {
  tibble::tibble(base_state = base_state, outs_when_up = outs, count_state = count) |>
    dplyr::left_join(re_lookup, by = c("base_state", "outs_when_up", "count_state")) |>
    dplyr::pull(re_value)
}

# Compute event_run_value for the 5 ABSVA event types. Returns one row per
# scoring event (no_event rows are excluded).
compute_event_run_values <- function(events, re288) {
  re_lookup <- make_re_lookup(re288)

  ev <- events |>
    dplyr::filter(event_type %in% c("framing_steal", "challenge_win",
                                    "overturned_frame", "miss_penalty", "bad_challenge")) |>
    dplyr::mutate(
      pre_count  = paste0(balls, "-", strikes),
      base_state = base_state_string(on_1b, on_2b, on_3b),
      strike_count_or_outcome = add_strike(pre_count),
      ball_count_or_outcome   = add_ball(pre_count)
    )

  ev |>
    dplyr::mutate(
      re_after_strike = dplyr::case_when(
        strike_count_or_outcome != "strikeout" ~
          get_re(base_state, outs_when_up, strike_count_or_outcome, re_lookup),
        strike_count_or_outcome == "strikeout" & outs_when_up == 2 ~ 0,
        strike_count_or_outcome == "strikeout" & outs_when_up <  2 ~
          get_re(base_state, outs_when_up + 1L, "0-0", re_lookup)
      ),
      re_after_ball = dplyr::case_when(
        ball_count_or_outcome != "walk" ~
          get_re(base_state, outs_when_up, ball_count_or_outcome, re_lookup),
        ball_count_or_outcome == "walk" ~
          get_re(advance_walk(base_state), outs_when_up, "0-0", re_lookup) +
          walk_runs_scored(base_state)
      ),
      event_run_value = dplyr::case_when(
        event_type == "framing_steal" ~ re_after_ball  - re_after_strike,
        event_type == "challenge_win" ~ re_after_ball  - re_after_strike,
        event_type == "overturned_frame" ~ 0,
        event_type == "miss_penalty"  ~ re_after_strike - re_after_ball,
        event_type == "bad_challenge" ~ BAD_CHALLENGE_PENALTY,
        TRUE ~ NA_real_
      )
    )
}

# Aggregate scored events to the per-catcher leaderboard.
aggregate_to_leaderboard <- function(scored, all_events) {
  pitches_per_catcher <- all_events |>
    dplyr::filter(!is.na(fielder_2)) |>
    dplyr::count(fielder_2, catcher_name, name = "pitches_caught")

  scored |>
    dplyr::group_by(fielder_2, catcher_name) |>
    dplyr::summarise(
      n_framing_steal  = sum(event_type == "framing_steal"),
      n_challenge_win  = sum(event_type == "challenge_win"),
      n_overturned_frame  = sum(event_type == "overturned_frame"),
      n_miss_penalty   = sum(event_type == "miss_penalty"),
      n_bad_challenge  = sum(event_type == "bad_challenge"),
      rv_framing       = sum(event_run_value[event_type == "framing_steal"], na.rm = TRUE),
      rv_challenge_win = sum(event_run_value[event_type == "challenge_win"], na.rm = TRUE),
      rv_miss_penalty  = sum(event_run_value[event_type == "miss_penalty"],  na.rm = TRUE),
      rv_bad_challenge = sum(event_run_value[event_type == "bad_challenge"], na.rm = TRUE),
      absva            = sum(event_run_value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(pitches_per_catcher, by = c("fielder_2", "catcher_name")) |>
    dplyr::mutate(absva_per_100 = round(100 * absva / pitches_caught, 3)) |>
    dplyr::arrange(dplyr::desc(absva))
}
