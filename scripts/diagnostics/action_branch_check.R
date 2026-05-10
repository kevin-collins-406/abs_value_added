# =============================================================================
# absva_action_branch_check.R
# =============================================================================
# Quick check: was the playEvent.action branch supposed to capture anything,
# and if so, did it?
#
# We re-walk a few games and look specifically at action events with
# review_type == "MJ" (i.e. ABS challenges that landed in action format,
# not the MA manager-replay one we filtered).
# =============================================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(glue); library(purrr)
  library(dplyr); library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Sample 20 games spread across the season
set.seed(42)
games_to_check <- c(
  823243, 822839, 823893, 823162, 823324, 823971, 824216, 824946,
  # add 12 more from later in the season — pick from your scrape
  # if you don't know game_pks offhand, here are some plausible later-season ones:
  825100, 825250, 825400, 825550, 825700, 825850, 826000, 826150,
  826300, 826450, 826600, 826750
)

scan_for_action_reviews <- function(game_pk) {
  url <- glue("https://statsapi.mlb.com/api/v1/game/{game_pk}/playByPlay")
  res <- tryCatch(GET(url), error = function(e) NULL)
  if (is.null(res) || http_error(res)) return(tibble())

  pbp <- fromJSON(content(res, as = "text", encoding = "UTF-8"),
                  simplifyVector = FALSE)

  hits <- list()
  for (play in pbp$allPlays) {
    for (j in seq_along(play$playEvents)) {
      pe <- play$playEvents[[j]]
      # Only interested in action events (no pitchNumber) with reviewDetails
      if (is.null(pe$reviewDetails)) next
      if (!is.null(pe$pitchNumber)) next
      hits[[length(hits) + 1]] <- tibble(
        game_pk     = game_pk,
        ab          = play$atBatIndex + 1L,
        pe_index    = j,
        review_type = pe$reviewDetails$reviewType %||% NA_character_,
        is_overturned = pe$reviewDetails$isOverturned %||% NA
      )
    }
  }
  if (length(hits) == 0) tibble() else bind_rows(hits)
}

cat("\nScanning 20 games for action-event reviews...\n")
results <- map_dfr(games_to_check, function(g) {
  cat(".")
  scan_for_action_reviews(g)
})
cat("\n\n")

cat("Action-event reviews found across sample:\n")
if (nrow(results) == 0) {
  cat("  ZERO. The playEvent.action source is genuinely empty (or near-empty)\n")
  cat("  for ABS challenges. Possibility A confirmed. v2 numbers stand.\n")
} else {
  print(results %>% count(review_type, sort = TRUE))
  cat("\nFull listing:\n")
  print(results, n = Inf)
  cat("\nIf any have review_type == 'MJ', the v2 extractor likely missed them.\n")
}
