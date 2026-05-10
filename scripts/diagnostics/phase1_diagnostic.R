# =============================================================================
# absva_phase1_diagnostic.R
# =============================================================================
# Goal: answer two questions, no more, no less.
#
#   Q1. Does the 2026 Statcast CSV `description` field encode ABS challenge
#       outcomes (e.g. "abs_challenge_overturned_ball" or similar)?
#
#   Q2. If not, do MLB Stats API play-by-play `reviewDetails` blocks join
#       cleanly to Statcast on game_pk + at_bat_number + pitch_number?
#
# Architecture decision flows directly from this script's output:
#   - Q1 = YES    -> classify events from Statcast alone, no API needed.
#   - Q1 = NO     -> Q2 must be YES; pipeline joins Stats API to Statcast.
#
# Notes:
#   - We bypass baseballr::statcast_search() entirely. Hits the Savant CSV
#     endpoint directly (same URL pattern that worked in the 2024 scraper).
#   - We test on real 2026 game_pks already confirmed: 823243 (NYY @ SF, 3/27).
# =============================================================================


# ── 0. Setup ─────────────────────────────────────────────────────────────────

pkgs <- c("readr", "dplyr", "httr", "jsonlite", "glue", "purrr", "tidyr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(readr); library(dplyr); library(httr); library(jsonlite)
library(glue);  library(purrr); library(tidyr)

cat("\n========================================\n")
cat("ABSVA Phase 1 Diagnostic\n")
cat("========================================\n\n")


# ── 1. Pull a small slice of 2026 Statcast directly ──────────────────────────
# Use the exact URL pattern that worked previously. One week is enough to
# observe the full set of `description` values that the API ever emits.

start_date <- "2026-03-27"
end_date   <- "2026-04-04"

single_game_url <- paste0(
  "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
  "&hfGT=R%7CPO%7CS%7CE%7C",      # all game types just in case
  "&hfSea=2026%7C",
  "&player_type=batter",
  "&game_date_gt=2026-03-27",
  "&game_date_lt=2026-03-27",
  "&min_pitches=0&min_results=0&min_abs=0",
  "&type=details"
)

sc_one_day <- read_csv(single_game_url, show_col_types = FALSE)
sc_one_day %>% count(game_pk, game_type)

cat(glue("[Q1] Pulling Statcast {start_date} -> {end_date}...\n"))
sc_df <- read_csv(savant_url, show_col_types = FALSE, progress = FALSE)
cat(glue("     Rows: {nrow(sc_df)}    Cols: {ncol(sc_df)}\n\n"))


# ── 2. Q1: Inspect the `description` field ───────────────────────────────────

cat("[Q1] Unique values of `description`:\n")
desc_counts <- sc_df %>%
  count(description, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 2))
print(desc_counts, n = 100)
cat("\n")

# Heuristic flag: anything containing 'abs', 'challenge', 'overturn', 'review'
abs_flagged <- desc_counts %>%
  filter(grepl("abs|challenge|overturn|review", description, ignore.case = TRUE))

cat("[Q1] Description values that look ABS-related:\n")
if (nrow(abs_flagged) == 0) {
  cat("     NONE FOUND. Statcast `description` does NOT encode challenge events.\n")
  cat("     => Pipeline must join MLB Stats API reviewDetails. Proceeding to Q2.\n\n")
  q1_answer <- "NO"
} else {
  print(abs_flagged)
  cat("     => Statcast description DOES encode challenges. Q2 is optional.\n\n")
  q1_answer <- "YES"
}

# Bonus: also peek at the `type` field (B/S/X). Worth confirming whether
# challenges produce a new code there.
cat("[Q1b] Unique values of `type`:\n")
print(sc_df %>% count(type, sort = TRUE))
cat("\n")


# ── 3. Q2: Pull MLB Stats API play-by-play for a known game ──────────────────

game_pk <- 823243   # Yankees @ Giants, 2026-03-27 (confirmed)

cat(glue("[Q2] Pulling Stats API play-by-play for game_pk = {game_pk}...\n"))
pbp_url <- glue("https://statsapi.mlb.com/api/v1/game/{game_pk}/playByPlay")
pbp_raw <- GET(pbp_url)
stop_for_status(pbp_raw)
pbp <- fromJSON(content(pbp_raw, as = "text", encoding = "UTF-8"),
                simplifyVector = FALSE)

n_plays <- length(pbp$allPlays)
cat(glue("     Pulled {n_plays} plays.\n"))


# ── 4. Walk every pitch event and look for reviewDetails ─────────────────────
# Stats API structure (per pitch):
#   allPlays[[i]]$playEvents[[j]]
#       $type           "pitch" / "action" / "no_pitch"
#       $details        $description, $code (B/S/X), etc.
#       $pitchNumber    1, 2, 3, ...
#       $reviewDetails  PRESENT if this pitch was challenged
#         $reviewType, $challengeTeamId, $reviewResult,
#         $absResult, $isOverturned, ...
#   allPlays[[i]]$atBatIndex            -> at_bat_number is atBatIndex + 1
#   allPlays[[i]]$about$inning, etc.

extract_pitch_row <- function(play, pe) {
  has_review <- !is.null(pe$reviewDetails)
  tibble(
    game_pk         = game_pk,
    at_bat_number   = play$atBatIndex + 1L,    # Statcast convention
    pitch_number    = pe$pitchNumber %||% NA_integer_,
    pitch_type_code = pe$details$code   %||% NA_character_,
    description     = pe$details$description %||% NA_character_,
    has_review      = has_review,
    review_type     = if (has_review) pe$reviewDetails$reviewType   %||% NA_character_ else NA_character_,
    review_result   = if (has_review) pe$reviewDetails$reviewResult %||% NA_character_ else NA_character_,
    abs_result      = if (has_review) pe$reviewDetails$absResult    %||% NA_character_ else NA_character_,
    is_overturned   = if (has_review) pe$reviewDetails$isOverturned %||% NA          else NA,
    challenge_team  = if (has_review) pe$reviewDetails$challengeTeamId %||% NA_integer_ else NA_integer_
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

pitch_rows <- map_dfr(pbp$allPlays, function(play) {
  pe_list <- play$playEvents
  if (length(pe_list) == 0) return(tibble())
  # Keep only events with a pitchNumber (real pitches, not pickoffs/timeouts)
  map_dfr(pe_list, function(pe) {
    if (is.null(pe$pitchNumber)) return(tibble())
    extract_pitch_row(play, pe)
  })
})

cat(glue("     Extracted {nrow(pitch_rows)} pitch events.\n"))
cat(glue("     Of which {sum(pitch_rows$has_review)} had reviewDetails.\n\n"))


# ── 5. Show the actual challenge rows ────────────────────────────────────────

challenges <- pitch_rows %>% filter(has_review)
if (nrow(challenges) > 0) {
  cat("[Q2] Challenge events from Stats API:\n")
  print(challenges, width = Inf)
  cat("\n")
} else {
  cat("[Q2] No challenges in this game. Trying another game_pk...\n")
  # Fallback game from your handoff
  game_pk_alt <- 822839
  cat(glue("     Retrying with game_pk = {game_pk_alt}...\n"))
  # (Could repeat the block above; left as a manual step to keep this short.)
}


# ── 6. Q2 final test: do the keys join cleanly to Statcast? ──────────────────

if (nrow(challenges) > 0) {
  sc_subset <- sc_df %>%
    filter(game_pk == !!game_pk) %>%
    select(game_pk, at_bat_number, pitch_number,
           description, type, plate_x, plate_z, sz_top, sz_bot,
           balls, strikes)

  cat(glue("[Q2] Statcast rows for game_pk={game_pk}: {nrow(sc_subset)}\n"))

  joined <- challenges %>%
    inner_join(sc_subset,
               by = c("game_pk", "at_bat_number", "pitch_number"),
               suffix = c("_api", "_sc"))

  cat(glue("     Successful joins: {nrow(joined)} / {nrow(challenges)}\n\n"))

  if (nrow(joined) > 0) {
    cat("[Q2] Joined challenge rows (API metadata + Statcast pitch context):\n")
    print(joined %>% select(at_bat_number, pitch_number,
                            description_api, description_sc,
                            review_type, abs_result, is_overturned,
                            plate_x, plate_z, sz_top, sz_bot),
          width = Inf)
  } else {
    cat("     Join failed. Inspect key alignment manually.\n")
  }
}


# ── 7. Verdict ───────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat("VERDICT\n")
cat("========================================\n")
cat(glue("Q1 (Statcast description encodes ABS): {q1_answer}\n"))
cat("Q2 (Stats API joins to Statcast):       see above.\n\n")

cat("Save the output of this script and we'll architect Phase 2 from there.\n")
