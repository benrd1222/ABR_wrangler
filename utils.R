# Utilities for the ABR wrangler that could be useful to reuse
require(tidyverse)

# Make table out of list of ABR threshold tables
# takes in a list of threshold data for a number of ABR samples and concatenates into a table
ABR_thresh_table <- function(tl) {
  n <- length(tl)

  table <- data.frame(tl[[1]])
  for (i in 2:n) {
    table <- table |>
      full_join(data.frame(tl[[i]]), by = "X1")
  }

  colnames(table) <- c("Frequency (kHz)", names(tl))
  return(table)
}

DP_thresh_table <- function(tl) {
  n <- length(tl)

  table <- data.frame(tl[[1]])
  for (i in 2:n) {
    table <- table |>
      full_join(data.frame(tl[[i]]), by = "f2")
  }

  colnames(table) <- c("f2", names(tl))
  return(table)
}

# Given a level and a peak and a list of analyzed ABR data, extract relevant data
# and return the dataframe
ABR_extract_amplitude <- function(level, peak, pl) {
  n <- length(pl)

  # TODO:
  # some logic to determine the peak chosen, ensure it's an integer, then just glue it together, while you're at it check that the level is numeric
  to_select <- str_glue("P{peak}.Amplitude")

  table <- data.frame(pl[[1]]) |>
    select(Level, frequency, {{ to_select }}) |>
    filter(Level == level) |>
    select(!Level)

  for (i in 2:n) {
    new <- data.frame(pl[[i]]) |>
      select(Level, frequency, {{ to_select }}) |>
      filter(Level == level) |>
      select(!Level)

    table <- table |>
      full_join(new, by = "frequency")
  }

  colnames(table) <- c("Frequency", names(pl))
  return(table)
}

# will be just about the same as ABR_extract_peak, so could maybe make the latency
# or peak data extraction be a parameter
ABR_extract_latency <- function(level, peak, pl) {
  n <- length(pl)

  # steal the logic from above or combine the functions
  to_select <- str_glue("P{peak}.Latency")

  table <- data.frame(pl[[1]]) |>
    select(Level, frequency, {{ to_select }}) |>
    filter(Level == level) |>
    select(!Level)

  for (i in 2:n) {
    new <- data.frame(pl[[i]]) |>
      select(Level, frequency, {{ to_select }}) |>
      filter(Level == level) |>
      select(!Level)

    table <- table |>
      full_join(new, by = "frequency")
  }

  colnames(table) <- c("Frequency", names(pl))
  return(table)
}


ABR_wave <- function() {}

DP_wave <- function() {}
