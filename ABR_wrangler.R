# Load Libraries ----
library(tidyverse)
library(writexl)
source("utils.R")

# Find all sample subdirectories ----

# that the number of ABR and DP files present in the subdirectory matches the
# expected amount listed in "Experiment State.txt"
projDir <- "./Data" #TODO: think about how to run this script for a user- snakemake could be easy
exportDir <- projDir

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

# TODO: keep a running vector of all unique levels and frequencies seen in the ABR data
unique_freqs <- c() # want to keep track of the unique frequencies seen
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
    freq <- as.numeric(str_extract(header[2], "\\d+\\.\\d+"))
    threshold <- as.numeric(str_extract(header[1], "\\d+\\.\\d+"))
    threshold <- c(freq, threshold)
    rm(header)

    # add frequency to the vector if not already there
    if (!(freq %in% unique_freqs)) {
      unique_freqs <- append(unique_freqs, freq)
    }

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

  # For DP data we just need to iterate throught the samples and look at one file each
  cur_file <- str_glue("{cur_sample}/{DP_analyzed_fp}")

  DP_thresh <- read_delim(cur_file, delim = "\t")

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
# Prompt the user for override of basic functioning

# Should default to reasonable values which is the base way of doing it

# Merge and format ----
# Merge all samples in list ABR_thresholds, merge by col 1
# so looks like write, excel writes all of the sheets at once from a list,
# which means we just need to name the entries in a list and append the data,
# then we can export at the very end
export_xl <- list()

export_xl$ABR_thresholds <- ABR_thresh_table(ABR_thresholds)

export_xl$DP_thresholds <- DP_thresh_table(DP_thresholds)

# write to excel ----

levels = c(80, 70)
for (l in levels) {
  sheet_name <- str_glue("{l}dB Amplitudes")

  export_xl[[sheet_name]] <- ABR_extract(level = l, peak = 1, pl = ABR_peaks)

  sheet_name <- str_glue("{l}dB Latencies")

  export_xl[[sheet_name]] <- ABR_extract(
    level = l,
    peak = 1,
    amp_switch = FALSE,
    pl = ABR_peaks
  )
}

# Outputs the amplitudes by level for the unique frequencies found in the original
for (f in unique_freqs) {
  sheet_name <- str_glue("Amplitudes@{f}kHz")

  # This filtering is to stay consistent with user desired output
  tmp <- ABR_extract(freq = f, peak = 1, pl = ABR_peaks) |>
    filter(Level %% 10 == 0) |>
    arrange(desc(Level))

  export_xl[[sheet_name]] <- tmp
}

# now we similarly want to extract the waveforms at 80 dB for each sample across
# frequencies, needs to be it's own loop for order of the sheets

# could technically allow for the input of the waveforms into the ABR_extract function
# if there was an easy way to tell the datastructures apart. For now I will
# allocate it to it's own function
for (f in unique_freqs) {
  sheet_name <- str_glue("ABR Waveforms@{f}kHz")
  export_xl[[sheet_name]] <- ABR_wave(level = 80, freq = f, wl = ABR_waveforms)
}

# we also want a version that extracts within one sample all of the levels passed

# similar loop for DP

# now we also need to grab the waveforms across all levels for a specific frequency for each sample, likely each sample needs to be it's own sheet for his
# prism workflow

write_xlsx(export_xl, str_glue("{exportDir}/wrangler_output.xlsx"))
