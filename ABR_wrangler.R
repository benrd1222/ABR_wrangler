# Load Libraries ----
library(tidyverse)
source("utils.R")

# Find all sample subdirectories ----

# that the number of ABR and DP files present in the subdirectory matches the
# expected amount listed in "Experiment State.txt"
projDir <- "./Data" #TODO: think about how to run this script for a user- snakemake could be easy

message("Finding samples...")

samples <- list.dirs(path = projDir, full.names = FALSE)
samples <- samples[-1] # may not need if we call this within the project directory

message(str_glue("Found all samples: "))
message(str_glue("{samples}", .sep = " "))

# generalize everything below to be performed per each sample

# These lists include the relevant data for each sample in the project
ABR_thresholds <- vector("list", length(samples))
ABR_peaks <- vector("list", length(samples))
ABR_waveforms <- vector("list", length(samples))
DP_thresholds <- vector("list", length(samples))
DP_peaks <- vector("list", length(samples))

names(ABR_thresholds) <- samples
names(ABR_peaks) <- samples
names(ABR_waveforms) <- samples
names(DP_thresholds) <- samples
names(DP_peaks) <- samples

for (i in 1:length(samples)) {
  cur_sample <- str_glue("{projDir}/{samples[i]}")

  valid_sample(projDir, cur_sample)

  ABR_analyzed_fp <- list.files(
    cur_sample,
    pattern = "^ABR.*\\.txt$"
  )

  ABR_wave_fp <- list.files(
    cur_sample,
    pattern = "^ABR.*\\d$"
  )

  DP_analyzed_fp <- list.files(
    cur_sample,
    pattern = "^DP.*\\.txt$"
  )

  DP_wave_fp <- list.files(
    cur_sample,
    pattern = "^DP.*\\d$"
  )
  DP_wave_fp <- DP_wave_fp[1]

  for (j in 1:num_ABRs) {
    # extract ABR data, adding the threshold to the sample threshold data
    cur_file <- str_glue("{cur_sample}/{ABR_analyzed_fp[j]}")
    header <- readLines(cur_file, n = 6)

    #extract info about frequency and threshold from the header
    freq <- str_extract(header[2], "\\d+\\.\\d+")
    threshold <- str_extract(header[1], "\\d+\\.\\d+")
    threshold <- c(freq, threshold)
    rm(header)

    # Read the data as a tsv skipping the header, add frequecny for the data
    ABR_analyzed <- read.delim(cur_file, sep = "\t", skip = 6)
    ABR_analyzed <- ABR_analyzed |>
      mutate(frequency, rep(freq, n()))

    rm(cur_file)

    cur_file <- str_glue("{cur_sample}/{ABR_wave_fp[j]}")
    header <- readLines(cur_file, n = 7)

    levels <- unlist(str_extract_all(header[6], "\\d+"))

    ABR_wave <- read_delim(cur_file, delim = "\t", skip = 7, col_names = FALSE)
    colnames(data) <- levels
    ABR_wave <- ABR_wave |>
      mutate(frequency, rep(freq, n()))

    # if j=1 add dataframe to the ABR_peaks list, else colbind
    # writing the data to organized format
    if (j == 1) {
      ABR_thresholds$cur_sample <- threshold
      ABR_peaks$cur_sample <- ABR_analyzed
      ABR_waveforms$cur_sample <- ABR_wave
    } else {
      ABR_thresholds$cur_sample <- rbind(ABR_thresholds$cur_sample, threshold)
      ABR_peaks$cur_sample <- rbind(ABR_peaks$cur_sample, ABR_analyzed)
      ABR_waveforms$cur_sample <- rbind(ABR_waveforms$cur_sample, ABR_wave)
    }
  }

  # The DP files come pre-stacked, so I just need to extract once per sample and add it to the list
  cur_file <- str_glue("{cur_sample}/{DP_analyzed_fp[i]}")

  DP_thresh <- read_delim(cur_file, delim = "\t")

  cur_file <- str_glue("{cur_sample}/{DP_wave_fp[i]}")
  # Header row 6 is the colnames, but rows 1-5 and row 7 are not needed so, we need
  # to read all the lines as data first then subset then read the data as a delim
  DP_wave <- readLines(cur_file)
  DP_wave <- data[-c(1:5, 7)]
  DP_wave <- read_delim(I(data), delim = "\t")

  # save the output to it's sample name
  DP_thresholds <- DP_thresh
  DP_peaks <- DP_wave
}

# this can then be stored to the sample name in the list for further post-processing

# the process will include getting user input and then putting it all together in
# excel sheet like Luis wants for prism usage

# User input ----

# Prompt the user for how many peaks deep they would like the wrangled data to include
# this refers to which peaks to determine rawwaveforms for
# Prompt the user for the dB levels they would like amplitudes and latencies to
# be wrangled for

# ABR thresholds ----
# Make ABR threshold table by reading the top few lines of each ABR-analyzed.txt
# file, cut the headers from each file and turn the remaining matrix into a table
# named with the frequency

# DP thresholds
# make the DP thresholds by merging the DP-samplename-1-analyzed.txt file for each
# sample

#Peak Organization
