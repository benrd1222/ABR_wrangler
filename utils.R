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
# Given a frequency, a peak, and a list of analyzed ABR data, extract relevant data # across all levels
# This is probably better suited for an S4 OOP package, we want a generic and then
# changes to the method called depending on the parameters passed
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

# OBSOLETE: keeping until I determine that the above function works properly
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

# Needs to be able to deal with missing entries for a whole frequency
# this is an absolute pain for some reason, need to step through it for the example case giving me trouble
ABR_wave <- function(freq, level, wl) {
  n <- length(wl)

  # logic on valid frequency

  # level has to be made a character so if it doesn't match it will error through
  # dplyr
  level <- as.character(level)
  out <- c()

  out <- wl[[1]] |>
    select(frequency, {{ level }}) |>
    filter(frequency == freq) |>
    select(-frequency)

  for (i in 2:n) {
    new <- wl[[i]] |>
      select(frequency, {{ level }}) |>
      filter(frequency == freq) |>
      select(-frequency)

    # check if there is data for this frequency
    if (length(new) > 0) {
      #if there is data ensure that it has the right number of row
      dif <- abs(nrow(new) - nrow(out))
      if (nrow(new) < nrow(out)) {
        add <- rep(NA, dif)
        new <- c(new, add)
      } else if (nrow(new) > nrow(out)) {
        #check dimension of previous data add rows of NA's equal to the diff
        add_mat <- matrix(NA, nrow = dim(out)[1], ncol = dim(out)[2])
        out <- rbind(out, add_mat)
      }
    } else {
      new <- rep(NA, nrow(out))
    }

    out <- cbind(out, new)
  }

  out <- as.data.frame(out)
  colnames(out) <- c(names(wl))
  return(out)
}

DP_wave <- function() {}

# could be useful to have a function that just yanks all of the unique levels and frequencies from the entire dataset, defintely could make this an object
out <- wl[[1]] |>
  select(frequency, {{ level }}) |>
  filter(frequency == freq) |>
  select(-frequency)

for (i in 2:n) {
  new <- wl[[i]] |>
    select(frequency, {{ level }}) |>
    filter(frequency == freq) |>
    select(-frequency)

  dif <- abs(length(out) - length(new))
  if (length(new) < length(out)) {
    add <- rep(0, dif)
    new <- c(new, add)
  } else if (length(new) > length(out)) {
    # ahhh this is faulty logic because out becomes a matrix past i==1
    add <- rep(0, dif)
    out <- c(out, add)
  }

  out <- cbind(out, new)
}
