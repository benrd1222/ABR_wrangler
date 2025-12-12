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

# Given a frequency, a peak, and a list of analyzed ABR data, extract relevant data
# across all levels
ABR_extract <- function(
  level = FALSE,
  freq = FALSE,
  peak,
  amp_switch = TRUE,
  pl
) {
  n <- length(pl)

  # logic to check if the input is fine and check the switch to determine which
  # variable to extract
  if (amp_switch == TRUE & is.numeric(peak)) {
    to_select <- str_glue("P{peak}.Amplitude")
  } else if (amp_switch == FALSE & is.numeric(peak)) {
    to_select <- str_glue("P{peak}.Latency")
  }

  # now how do we split the filtering action
  if (level == FALSE & freq != FALSE) {
    # do the frequency based filtering
    table <- data.frame(pl[[1]]) |>
      select(Level, frequency, {{ to_select }}) |>
      filter(frequency == freq) |>
      select(!frequency)

    for (i in 2:n) {
      new <- data.frame(pl[[i]]) |>
        select(Level, frequency, {{ to_select }}) |>
        filter(frequency == freq) |>
        select(!frequency)

      table <- table |>
        full_join(new, by = "Level")
    }

    colnames(table) <- c("Level", names(pl))
    return(table)
  } else if (level != FALSE & freq == FALSE) {
    # if we want to join by level
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

  stop("Something went wrong!")
}


# Takes in a dataframe for a specific sample, a frequency and a level a returns a
# the waveform for that one measurement as a vector
ABR_wave_extract <- function(freq, level, dw) {
  level <- as.character(level)

  out <- dw |>
    select(frequency, {{ level }}) |>
    filter(frequency == freq) |>
    select(-frequency)

  return(out)
}
