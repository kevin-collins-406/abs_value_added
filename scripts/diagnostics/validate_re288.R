# =============================================================================
# absva_validate_re288.R
# =============================================================================
# Five validations of the RE288 matrix before we ship it:
#
#   1. Heatmap visualization
#   2. Within-count monotonicity (more strikes -> lower RE; more balls -> higher)
#   3. Within-base-out monotonicity (more baserunners -> higher RE)
#   4. Low-sample cell review (cells with < 100 pitches need attention?)
#   5. Distribution of challenge_win run values (feeds bad_challenge calibration)
#
# Output: data/re288_validation_report.html (heatmap + tables)
# =============================================================================

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(purrr)
  library(ggplot2); library(glue); library(tibble); library(stringr)
})


# ── 1. Load matrix and ABSVA event data ──────────────────────────────────────

re288  <- read_parquet(here::here("data", "processed",    "re288_matrix.parquet"))
events <- read_parquet(here::here("data", "intermediate", "absva_classified_2026.parquet"))

cat(glue("Matrix cells: {nrow(re288)}\n"))
cat(glue("Total event rows: {nrow(events)}\n\n"))


# ── 2. HEATMAP — render and save ─────────────────────────────────────────────

cat("=== Generating heatmap ===\n")

# Order base states in a sensible way: by total run value (low to high)
base_order <- re288 %>%
  group_by(base_state) %>%
  summarise(avg = mean(re_value)) %>%
  arrange(avg) %>%
  pull(base_state)

# Order count states canonically
count_order <- c("0-0","0-1","0-2","1-0","1-1","1-2",
                 "2-0","2-1","2-2","3-0","3-1","3-2")

heatmap_df <- re288 %>%
  mutate(
    base_state = factor(base_state, levels = base_order),
    count_state = factor(count_state, levels = count_order),
    out_label = paste(outs_when_up, "out(s)")
  )

p_heat <- ggplot(heatmap_df, aes(count_state, base_state, fill = re_value)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", re_value)), size = 2.5, color = "black") +
  scale_fill_gradient(low = "#fef0d9", high = "#b30000",
                      name = "Expected\nruns") +
  facet_wrap(~ out_label, ncol = 1) +
  labs(
    title = "RE288 Matrix — 2024-2025 MLB",
    subtitle = "Expected runs scored from current state to end of half-inning",
    x = "Count (balls-strikes)", y = "Bases (1B,2B,3B)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("data/re288_heatmap.png", p_heat, width = 10, height = 12, dpi = 150)
cat("  Saved -> data/re288_heatmap.png\n\n")


# ── 3. WITHIN-COUNT MONOTONICITY ─────────────────────────────────────────────
# Holding base-out fixed, RE should:
#   - DECREASE as strikes go from 0 to 2 (more strikes hurt offense)
#   - INCREASE as balls go from 0 to 3 (more balls help offense)

cat("=== Within-count monotonicity check ===\n")

count_components <- re288 %>%
  separate(count_state, into = c("balls", "strikes"), sep = "-", convert = TRUE)

# Strike monotonicity: for fixed (base_state, outs, balls), does RE decrease
# as strikes increase?
strike_violations <- count_components %>%
  arrange(base_state, outs_when_up, balls, strikes) %>%
  group_by(base_state, outs_when_up, balls) %>%
  mutate(prev_re = lag(re_value)) %>%
  filter(!is.na(prev_re), re_value > prev_re) %>%
  ungroup()

# Ball monotonicity: for fixed (base_state, outs, strikes), does RE increase
# as balls increase?
ball_violations <- count_components %>%
  arrange(base_state, outs_when_up, strikes, balls) %>%
  group_by(base_state, outs_when_up, strikes) %>%
  mutate(prev_re = lag(re_value)) %>%
  filter(!is.na(prev_re), re_value < prev_re) %>%
  ungroup()

cat(glue("Strike-monotonicity violations (RE went UP when adding a strike): {nrow(strike_violations)}\n"))
cat(glue("Ball-monotonicity violations (RE went DOWN when adding a ball):    {nrow(ball_violations)}\n"))

if (nrow(strike_violations) > 0) {
  cat("\nWorst strike violations (top 5 by magnitude):\n")
  print(strike_violations %>%
    mutate(jump = re_value - prev_re) %>%
    arrange(desc(jump)) %>%
    select(base_state, outs_when_up, balls, strikes, prev_re, re_value, jump, n_pitches) %>%
    head(5))
}

if (nrow(ball_violations) > 0) {
  cat("\nWorst ball violations (top 5 by magnitude):\n")
  print(ball_violations %>%
    mutate(drop = prev_re - re_value) %>%
    arrange(desc(drop)) %>%
    select(base_state, outs_when_up, balls, strikes, prev_re, re_value, drop, n_pitches) %>%
    head(5))
}


# ── 4. BASE-OUT STRUCTURAL CHECK ─────────────────────────────────────────────
# A few invariant relationships that should hold:
#   - Adding a runner (000 -> 100) should increase RE
#   - Adding outs should decrease RE (fewer chances to score)

cat("\n=== Base-out structural sanity checks ===\n")

# Expected ordering of base states by RE (lowest to highest):
#   000 < 100 < 010 < 001 < 110 < 101 < 011 < 111
# Roughly. The exact order depends on how each runner contributes.

base_avg <- re288 %>%
  group_by(base_state) %>%
  summarise(avg_re = mean(re_value), .groups = "drop") %>%
  arrange(avg_re)

cat("Base states ranked by average RE (across all counts/outs):\n")
print(base_avg)

# Outs effect: average RE by outs (should decrease as outs increase)
out_avg <- re288 %>%
  group_by(outs_when_up) %>%
  summarise(avg_re = mean(re_value), .groups = "drop")

cat("\nAverage RE by outs (should decrease):\n")
print(out_avg)


# ── 5. LOW-SAMPLE CELLS ──────────────────────────────────────────────────────

cat("\n=== Low-sample cells review ===\n")

low_n <- re288 %>%
  arrange(n_pitches) %>%
  filter(n_pitches < 200) %>%
  select(base_state, outs_when_up, count_state, n_pitches, re_value)

cat(glue("Cells with < 200 pitches: {nrow(low_n)}\n"))
if (nrow(low_n) > 0) {
  cat("\nFull list:\n")
  print(low_n, n = Inf)
  cat("\nThese will produce noisier RE estimates. They're rare states\n")
  cat("(usually 3-2 with bases loaded + 0/2 outs). The events landing\n")
  cat("in these cells will be rare too, so leaderboard impact is minimal.\n")
}


# ── 6. CHALLENGE_WIN RV DISTRIBUTION (for bad_challenge calibration) ─────────

cat("\n=== Distribution of challenge_win run-value swings ===\n")

# For each challenge_win event, compute:
#   RE_after = RE at the count AFTER the strike (i.e., +1 strike to count)
#   RE_before = RE at the count AFTER a ball (i.e., +1 ball to count)
#   RV = RE_after - RE_before
# These are the RV swings catchers GAIN when they correctly challenge.
# The expected value of a held challenge =
#   P(catcher uses it) * P(wins given used) * mean(RV swing)

# Build counterfactual count states: "ball" adds 1 to balls, "strike" adds 1 to strikes
add_strike <- function(count) {
  parts <- str_split(count, "-")[[1]] %>% as.integer()
  if (parts[2] >= 2) return(NA_character_)  # strikeout terminal
  paste0(parts[1], "-", parts[2] + 1)
}

add_ball <- function(count) {
  parts <- str_split(count, "-")[[1]] %>% as.integer()
  if (parts[1] >= 3) return(NA_character_)  # walk terminal
  paste0(parts[1] + 1, "-", parts[2])
}

# Apply to challenge_win events
cw <- events %>% filter(event_type == "challenge_win")

cat(glue("challenge_win events: {nrow(cw)}\n"))

# Build the count_state for each event from balls/strikes
cw <- cw %>%
  mutate(
    pre_count = paste0(balls, "-", strikes),
    base_state = paste0(
      ifelse(is.na(on_1b), "0", "1"),
      ifelse(is.na(on_2b), "0", "1"),
      ifelse(is.na(on_3b), "0", "1")
    ),
    count_after_strike = map_chr(pre_count, add_strike),
    count_after_ball   = map_chr(pre_count, add_ball)
  )

# Lookup RE at after_strike and after_ball
re_lookup <- re288 %>% select(base_state, outs_when_up, count_state, re_value)

cw <- cw %>%
  left_join(re_lookup %>% rename(re_after_strike = re_value),
            by = c("base_state", "outs_when_up", "count_after_strike" = "count_state")) %>%
  left_join(re_lookup %>% rename(re_after_ball = re_value),
            by = c("base_state", "outs_when_up", "count_after_ball" = "count_state"))

# For terminal counts (strikeout or walk on this pitch), we need different
# logic — for now, drop those from the calibration sample and note the count.
n_terminal <- sum(is.na(cw$re_after_strike) | is.na(cw$re_after_ball))
cat(glue("Terminal-count challenge_win events (strikeouts / walks): {n_terminal}\n"))
cat("  (excluded from this calibration sample; handled separately in scoring)\n\n")

cw_cal <- cw %>% filter(!is.na(re_after_strike), !is.na(re_after_ball))

cw_cal <- cw_cal %>%
  mutate(rv_swing = re_after_strike - re_after_ball)
# Note: re_after_strike < re_after_ball, so rv_swing is negative.
# The catcher's GAIN is -rv_swing (going from "would have been ball" to "now strike").
cw_cal <- cw_cal %>% mutate(catcher_gain = -rv_swing)

cat("Catcher run-value GAIN per challenge_win event:\n")
gain_summary <- cw_cal$catcher_gain %>% summary()
print(gain_summary)

cat(glue("\nMean: {mean(cw_cal$catcher_gain) %>% round(4)}\n"))
cat(glue("Median: {median(cw_cal$catcher_gain) %>% round(4)}\n"))
cat(glue("SD: {sd(cw_cal$catcher_gain) %>% round(4)}\n"))


# ── 7. BAD_CHALLENGE CALIBRATION (Method 2: EV of a held challenge) ──────────

cat("\n=== bad_challenge calibration ===\n")

# Method 2: EV of a held challenge =
#   P(catcher would correctly identify a future missed strike) *
#   P(challenge wins given attempted) *
#   E[RV gain | won]
#
# Approximation:
#   - Per the league overturn rate ~53.2%, P(win | attempted) ~= 0.532
#   - P(catcher uses the challenge) is harder to know. Empirically:
#     league-wide we observed N catcher challenges / N catcher games.
#     If each catcher gets ~9 innings * 60 pitches, and they challenge ~1
#     time per game, that's roughly 1.5% of pitches getting challenged.
#     But that's an UNCONDITIONAL rate — what we want is conditional on
#     having an opportunity (i.e., a true strike that got called a ball).
#     
#   Empirically simpler: assume every UNUSED challenge has some probability
#   of being used in the remaining pitches, and that probability roughly
#   equals the fraction of remaining innings.
#
# Pragmatic v1: bad_challenge cost = mean(catcher_gain) * P(win) =
#   mean(catcher_gain) * 0.532 * 1.0 = ~0.5x mean catcher gain
# Reasoning: a catcher who burns a challenge on a real ball is giving up
# the option value of that challenge. If the option is worth the average
# successful challenge times the win rate, that's our cost.

mean_gain <- mean(cw_cal$catcher_gain)
win_rate <- 0.532  # from Opta data, validated against our overturn rate

# A held challenge has an opportunity to be cashed in. We don't know
# probability of opportunity, but a defensible approximation:
#   - Each game starts with 2 challenges
#   - Average team uses ~1.0-1.2 of them per game (per Opta data)
#   - So ~50-60% of issued challenges get used
#
# This means a held challenge has probability ~0.55 of being used at all.
# Among used challenges, win rate is 0.532.
# So expected realized value of an unused held challenge:
#   E[realized gain] ~= 0.55 * 0.532 * mean_gain
# That's our cost for losing one challenge.

p_used <- 0.55      # probability a held challenge gets used in same game
ev_per_held <- p_used * win_rate * mean_gain

cat(glue("Mean catcher gain per challenge_win:    {round(mean_gain, 4)} runs\n"))
cat(glue("Probability of issuing | held:           {p_used}\n"))
cat(glue("Probability of winning | issued:         {win_rate}\n"))
cat(glue("Expected value of one held challenge:    {round(ev_per_held, 4)} runs\n"))
cat(glue("\n=> bad_challenge penalty: -{round(ev_per_held, 4)} runs per occurrence\n"))


# ── 8. Summary ───────────────────────────────────────────────────────────────

cat("\n=========================================\n")
cat("VALIDATION SUMMARY\n")
cat("=========================================\n")
cat(glue("Cells in matrix:           {nrow(re288)} / 288 expected\n"))
cat(glue("Min cell sample size:      {min(re288$n_pitches)}\n"))
cat(glue("Median cell sample size:   {median(re288$n_pitches)}\n"))
cat(glue("Strike-monotonicity OK:    {nrow(strike_violations) == 0}\n"))
cat(glue("Ball-monotonicity OK:      {nrow(ball_violations) == 0}\n"))
cat(glue("Cells with n < 200:        {sum(re288$n_pitches < 200)}\n"))
cat(glue("Heatmap saved:             data/re288_heatmap.png\n"))
cat(glue("Recommended bad_challenge: -{round(ev_per_held, 4)} runs\n"))
