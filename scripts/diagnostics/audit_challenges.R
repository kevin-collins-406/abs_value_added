# =============================================================================
# absva_challenge_audit.R
# =============================================================================
# Goal: figure out why we found 1587 challenges when the league total
# should be closer to 2200+ through this date range.
#
# Hypothesis A: reviewDetails sometimes appears in actionEvents not playEvents
# Hypothesis B: reviewDetails sometimes appears at the play level not pitch level
# Hypothesis C: challenges without a pitchNumber are being filtered out
# =============================================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(glue); library(purrr)
  library(dplyr); library(tibble); library(tidyr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# Pick a few games from your scrape, spaced across the season
test_games <- c(
  823243,   # Opening Day NYY @ SF (3 challenges confirmed earlier)
  822839,   # Opening Day OAK @ TOR
  823893    # Opening Day COL @ MIA
)

# Function to walk the WHOLE play-by-play structure looking for
# `reviewDetails` ANYWHERE — not just where we currently extract from.
find_all_reviews <- function(game_pk) {
  url <- glue("https://statsapi.mlb.com/api/v1/game/{game_pk}/playByPlay")
  pbp <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"),
                  simplifyVector = FALSE)

  hits <- list()

  for (i in seq_along(pbp$allPlays)) {
    play <- pbp$allPlays[[i]]

    # Hypothesis B: reviewDetails at play level
    if (!is.null(play$reviewDetails)) {
      hits[[length(hits) + 1]] <- tibble(
        game_pk = game_pk,
        location = "play.reviewDetails",
        at_bat_number = play$atBatIndex + 1L,
        pitch_number = NA_integer_,
        is_overturned = play$reviewDetails$isOverturned %||% NA,
        review_type = play$reviewDetails$reviewType %||% NA_character_
      )
    }

    # Walk playEvents
    for (j in seq_along(play$playEvents)) {
      pe <- play$playEvents[[j]]

      # Has reviewDetails?
      if (!is.null(pe$reviewDetails)) {
        loc <- if (!is.null(pe$pitchNumber)) "playEvent.pitch.review" else "playEvent.action.review"
        hits[[length(hits) + 1]] <- tibble(
          game_pk = game_pk,
          location = loc,
          at_bat_number = play$atBatIndex + 1L,
          pitch_number = pe$pitchNumber %||% NA_integer_,
          is_overturned = pe$reviewDetails$isOverturned %||% NA,
          review_type = pe$reviewDetails$reviewType %||% NA_character_
        )
      }
    }

    # Hypothesis A: actionEvents at play level
    if (!is.null(play$actionEvents)) {
      for (ae in play$actionEvents) {
        if (!is.null(ae$reviewDetails)) {
          hits[[length(hits) + 1]] <- tibble(
            game_pk = game_pk,
            location = "actionEvent.reviewDetails",
            at_bat_number = play$atBatIndex + 1L,
            pitch_number = NA_integer_,
            is_overturned = ae$reviewDetails$isOverturned %||% NA,
            review_type = ae$reviewDetails$reviewType %||% NA_character_
          )
        }
      }
    }
  }

  if (length(hits) == 0) return(tibble())
  bind_rows(hits)
}

cat("\n=== Challenge structure audit ===\n\n")

audit <- map_dfr(test_games, function(g) {
  cat(glue("Scanning game_pk = {g}...\n"))
  find_all_reviews(g)
})

cat("\nReview occurrences by location:\n")
print(audit %>% count(location, sort = TRUE))

cat("\nFull listing:\n")
print(audit, n = Inf, width = Inf)

# Now compare to what our existing extractor would have caught
# (i.e. only playEvent.pitch.review)
captured <- audit %>% filter(location == "playEvent.pitch.review") %>% nrow()
total    <- nrow(audit)

cat(glue("\nCaptured by current extractor: {captured} / {total}\n"))
cat(glue("Missed: {total - captured}\n\n"))

if (total > captured) {
  cat("==> Need to expand extractor to cover other locations.\n")
} else {
  cat("==> Extractor logic is complete; undercount is from elsewhere.\n")
}
