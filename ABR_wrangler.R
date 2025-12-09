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
N <- length(samples)

message(str_glue("Found all samples: "))
message(str_glue("{samples}", .sep = " "))

# generalize everything below to be performed per each sample

# These lists include the relevant data for each sample in the project
ABR_thresholds <- vector("list", N)
ABR_peaks <- vector("list", N)
ABR_waveforms <- vector("list", N)
DP_thresholds <- vector("list", N)
DP_peaks <- vector("list", N)

names(ABR_thresholds) <- samples
names(ABR_peaks) <- samples
names(ABR_waveforms) <- samples
names(DP_thresholds) <- samples
names(DP_peaks) <- samples

for (i in 1:N) {
  cur_sample <- str_glue("{projDir}/{samples[i]}")
  message(str_glue("Currently working on {cur_sample}"))

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

  expected_size <- readLines(str_glue(
    "{cur_sample}/Experiment State.txt"
  ))

  num_ABRs <- as.numeric(str_extract(expected_size[3], "[0-9]+"))
  num_DPs <- as.numeric(str_extract(expected_size[4], "[0-9]+"))

  if (
    num_ABRs != length(ABR_analyzed_fp) || num_DPs != length(DP_analyzed_fp)
  ) {
    stop(
      "The wrangler did not find the expected number of ABR or DP files for sample {cur_sample}"
    )
  }

  if (num_DPs > 1) {
    stop(
      "We can not handle more than one DP reading at this time, sample {cur_sample} has {num_DPs} readings"
    )
  }

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
      mutate(frequency = rep(freq, n()))

    # filter the trailing empty data column

    rm(cur_file)

    cur_file <- str_glue("{cur_sample}/{ABR_wave_fp[j]}")
    header <- readLines(cur_file, n = 7)

    levels <- unlist(str_extract_all(header[6], "\\d+"))

    ABR_wave <- read.delim(cur_file, sep = "\t", skip = 7, header = FALSE)
    colnames(ABR_wave) <- levels
    ABR_wave <- ABR_wave |>
      mutate(frequency = rep(freq, n()))

    # if j=1 add dataframe to the ABR_peaks list, else colbind
    # writing the data to organized format
    if (j == 1) {
      ABR_thresholds[[samples[i]]] <- threshold
      ABR_peaks[[samples[i]]] <- ABR_analyzed
      ABR_waveforms[[samples[i]]] <- ABR_wave
    } else {
      ABR_thresholds[[samples[i]]] <- rbind(
        ABR_thresholds[[samples[i]]],
        threshold
      )
      ABR_peaks[[samples[i]]] <- bind_rows(
        ABR_peaks[[samples[i]]],
        ABR_analyzed
      )
      ABR_waveforms[[samples[i]]] <- bind_rows(
        ABR_waveforms[[samples[i]]],
        ABR_wave
      )
    }
  }

  # The DP files come pre-stacked, so I just need to extract once per sample and add it to the list
  message(str_glue("On to the DP data"))
  message(str_glue("The current file is {cur_sample}/{DP_analyzed_fp}"))
  cur_file <- str_glue("{cur_sample}/{DP_analyzed_fp}")

  DP_thresh <- read_delim(cur_file, delim = "\t")

  message(str_glue("The current file is {cur_sample}/{DP_wave_fp}"))
  cur_file <- str_glue("{cur_sample}/{DP_wave_fp}")
  # Header row 6 is the colnames, but rows 1-5 and row 7 are not needed so, we need
  # to read all the lines as data first then subset then read the data as a delim
  DP_wave <- readLines(cur_file)
  DP_wave <- DP_wave[-c(1:5, 7)]
  DP_wave <- read_delim(I(DP_wave), delim = "\t")

  # save the output to it's sample name
  DP_thresholds[[samples[i]]] <- DP_thresh
  DP_peaks[[samples[i]]] <- DP_wave

  rm(list = c('ABR_analyzed', 'ABR_wave', 'DP_thresh', 'DP_wave'))
}

# User input ----
# TODO:
# Prompt the user for how many peaks deep they would like the wrangled data to include
# this refers to which peaks to determine rawwaveforms for
# Prompt the user for the dB levels they would like amplitudes and latencies to
# be wrangled for

# Merge and format ----
# Merge all samples in list ABR_thresholds, merge by col 1
ABR_thresh_exp <- ABR_thresh_table(ABR_thresholds)

DP_thresh_exp <- DP_thresh_table(DP_thresholds)

# write to excel

# wrap this in a loop for the level the user wants peaks and latencies for,
# just directly write them in order as sheets to the excel
ABR_amplitudes <- ABR_extract_amplitude(80, 1, ABR_peaks)
ABR_latencies <- ABR_extract_latency(80, 1, ABR_peaks)

# now Luis also wants the amplitude extraction along all levels for each of the
# of the frequencies as it's own sheet, could make the extraction functions
# polymorphic: e.g. if given a level and a peak, extract the information across
# all frequencies, but if given a frequency and a peak, extract it across all levels
# actually seems quite easy just a bit of logic to determine the function to filter by...
