# =============================================================================
# R/utils.R — shared helpers across the ABSVA pipeline
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Resolve a path inside the repo's data/ tree.
data_path <- function(...) here::here("data", ...)

# Append a timestamped line to message() and (optionally) a log file.
log_msg <- function(msg, log_path = NULL) {
  ts   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] ", msg)
  message(line)
  if (!is.null(log_path)) write(line, log_path, append = TRUE)
  invisible(line)
}

# Encode the (1B, 2B, 3B) state as a 3-character "0"/"1" string.
base_state_string <- function(on_1b, on_2b, on_3b) {
  paste0(
    ifelse(is.na(on_1b), "0", "1"),
    ifelse(is.na(on_2b), "0", "1"),
    ifelse(is.na(on_3b), "0", "1")
  )
}
