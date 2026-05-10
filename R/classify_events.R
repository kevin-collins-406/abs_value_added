# =============================================================================
# R/classify_events.R — Per-pitch classification into the 5 ABSVA event types,
# plus player-name attachment from the Chadwick register.
#
# Taxonomy (catcher-only):
#   framing_steal   (+)  True ball, called strike, batter does NOT challenge
#   challenge_win   (+)  Called ball -> catcher challenges -> overturned to strike
#   overturned_frame   (+)  True ball called strike -> batter challenges and WINS
#   miss_penalty    (-)  True strike called ball, catcher does NOT challenge
#                        (only counts when catcher had challenges available)
#   bad_challenge   (-)  Catcher challenges a true ball -> loses the challenge
#
# Trust hierarchy:
#   - For challenged pitches, trust Hawk-Eye's verdict (is_overturned).
#   - For unchallenged pitches, trust the geometric `abs_true_strike` flag.
# =============================================================================

# Map Statcast `description` -> coarse "original_call" buckets.
add_original_call <- function(df) {
  df |>
    dplyr::mutate(
      original_call = dplyr::case_when(
        description == "called_strike"     ~ "strike",
        description %in% c("ball","blocked_ball") ~ "ball",
        description == "automatic_ball"    ~ "auto_ball",
        description == "automatic_strike"  ~ "auto_strike",
        description == "hit_into_play"     ~ "in_play",
        description == "hit_by_pitch"      ~ "hbp",
        description %in% c("foul","foul_tip","foul_bunt","bunt_foul_tip",
                           "swinging_strike","swinging_strike_blocked",
                           "missed_bunt") ~ "swing",
        TRUE                               ~ "other"
      )
    )
}

# Build a game_pk -> (home_team_id, away_team_id) lookup from the schedule API.
fetch_team_lookup <- function(date_range) {
  url <- glue::glue(
    "https://statsapi.mlb.com/api/v1/schedule?sportId=1",
    "&startDate={date_range[1]}&endDate={date_range[2]}&gameType=R"
  )
  sched <- jsonlite::fromJSON(
    httr::content(httr::GET(url), as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  purrr::map_dfr(sched$dates, function(d) {
    purrr::map_dfr(d$games, function(g) {
      tibble::tibble(
        game_pk      = g$gamePk,
        home_team_id = g$teams$home$team$id %||% NA_integer_,
        away_team_id = g$teams$away$team$id %||% NA_integer_
      )
    })
  }) |>
    dplyr::distinct(game_pk, .keep_all = TRUE)
}

# Add defense_team_id and offense_team_id columns based on inning_topbot.
add_team_sides <- function(df) {
  df |>
    dplyr::mutate(
      defense_team_id = dplyr::if_else(inning_topbot == "Top", home_team_id, away_team_id),
      offense_team_id = dplyr::if_else(inning_topbot == "Top", away_team_id, home_team_id)
    )
}

# Vectorized challenge counter — replaces the per-game per-pitch for-loop with
# a cumsum over each side's failed challenges. Logic: each team starts with 2
# challenges; a successful (overturned) challenge is FREE; a failed one
# decrements. So challenges remaining for side X (BEFORE this pitch) =
# 2 - count(failed challenges by X strictly before this pitch).
add_challenge_counter <- function(df) {
  df |>
    dplyr::arrange(game_pk, at_bat_number, pitch_number) |>
    dplyr::mutate(
      .home_failed = has_review & !is.na(challenge_team) &
                     challenge_team == home_team_id &
                     !isTRUE_vec(is_overturned),
      .away_failed = has_review & !is.na(challenge_team) &
                     challenge_team == away_team_id &
                     !isTRUE_vec(is_overturned)
    ) |>
    dplyr::group_by(game_pk) |>
    dplyr::mutate(
      .home_used_pre = dplyr::lag(cumsum(dplyr::coalesce(.home_failed, FALSE)), default = 0L),
      .away_used_pre = dplyr::lag(cumsum(dplyr::coalesce(.away_failed, FALSE)), default = 0L),
      .home_remaining = pmax(0L, 2L - as.integer(.home_used_pre)),
      .away_remaining = pmax(0L, 2L - as.integer(.away_used_pre)),
      def_chall_remaining_pre = dplyr::case_when(
        is.na(home_team_id) | is.na(away_team_id) ~ NA_integer_,
        inning_topbot == "Top" ~ .home_remaining,
        TRUE                   ~ .away_remaining
      ),
      off_chall_remaining_pre = dplyr::case_when(
        is.na(home_team_id) | is.na(away_team_id) ~ NA_integer_,
        inning_topbot == "Top" ~ .away_remaining,
        TRUE                   ~ .home_remaining
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.home_failed, -.away_failed,
                  -.home_used_pre, -.away_used_pre,
                  -.home_remaining, -.away_remaining)
}

# Helper: vectorized version of isTRUE() — TRUE iff the input is the logical
# value TRUE (NA and NULL collapse to FALSE).
isTRUE_vec <- function(x) !is.na(x) & x == TRUE

# Tag each challenge as defense-side or offense-side issued.
add_challenger_side <- function(df) {
  df |>
    dplyr::mutate(
      challenger_side = dplyr::case_when(
        !has_review                       ~ NA_character_,
        challenge_team == defense_team_id ~ "defense",
        challenge_team == offense_team_id ~ "offense",
        TRUE                              ~ NA_character_
      )
    )
}

# Classify each pitch into one of the 5 ABSVA event types (or "no_event").
classify_event_type <- function(df) {
  df |>
    dplyr::mutate(
      event_type = dplyr::case_when(
        original_call %in% c("auto_ball","auto_strike","in_play","hbp","swing","other") ~
          "no_event",

        # Catcher (defense) challenged
        has_review & challenger_side == "defense" &  is_overturned ~ "challenge_win",
        has_review & challenger_side == "defense" & !is_overturned ~ "bad_challenge",

        # Batter (offense) challenged
        has_review & challenger_side == "offense" &  is_overturned ~ "overturned_frame",
        has_review & challenger_side == "offense" & !is_overturned ~ "no_event",

        # Unchallenged called pitches
        !has_review & original_call == "strike" & !abs_true_strike ~ "framing_steal",
        !has_review & original_call == "ball"   &  abs_true_strike & def_chall_remaining_pre > 0  ~ "miss_penalty",
        !has_review & original_call == "ball"   &  abs_true_strike & def_chall_remaining_pre == 0 ~ "no_event",

        TRUE ~ "no_event"
      )
    )
}

# Build a player_id -> player_name lookup, cached from the Chadwick register.
load_player_lookup <- function(cache_path) {
  if (!file.exists(cache_path)) {
    if (!requireNamespace("baseballr", quietly = TRUE)) install.packages("baseballr")
    message("Downloading Chadwick player register (one-time, ~1.6M rows)...")
    chadwick <- baseballr::chadwick_player_lu()
    saveRDS(chadwick, cache_path)
  } else {
    chadwick <- readRDS(cache_path)
  }
  chadwick |>
    dplyr::filter(!is.na(key_mlbam)) |>
    dplyr::transmute(
      player_id   = as.integer(key_mlbam),
      player_name = paste(name_first, name_last),
      last_year   = mlb_played_last
    ) |>
    dplyr::arrange(dplyr::desc(last_year)) |>
    dplyr::distinct(player_id, .keep_all = TRUE) |>
    dplyr::select(player_id, player_name)
}

# Attach catcher / batter / pitcher names from a player_id lookup.
add_player_names <- function(df, player_lookup) {
  df |>
    dplyr::left_join(player_lookup |> dplyr::rename(catcher_name = player_name),
                     by = c("fielder_2" = "player_id")) |>
    dplyr::left_join(player_lookup |> dplyr::rename(batter_name = player_name),
                     by = c("batter" = "player_id")) |>
    dplyr::left_join(player_lookup |> dplyr::rename(pitcher_name = player_name),
                     by = c("pitcher" = "player_id"))
}
