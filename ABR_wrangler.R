# Load Libraries ----
library(tidyverse)

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

ABR_thresholds <- list()
ABR_peaks <- list()
DP_thresholds <- list()
DP_peaks <- list()

for (i in 1:length(samples)) {
  ABR_analyzed_fp <- list.files(
    str_glue("{projDir}/{samples[i]}"),
    pattern = "^ABR.*\\.txt$"
  )

  ABR_wave_fp <- list.files(
    str_glue("{projDir}/{samples[i]}"),
    pattern = "^ABR.*\\d$"
  )

  DP_analyzed_fp <- list.files(
    str_glue("{projDir}/{samples[i]}"),
    pattern = "^DP.*\\.txt$"
  )

  DP_wave_fp <- list.files(
    str_glue("{projDir}/{samples[i]}"),
    pattern = "^DP.*\\d$"
  )
  DP_wave_fp <- DP_wave_fp[1]

  # now we can validate that the files expected to be here have been found
  raw_experiment <- readLines(str_glue(
    "{projDir}/{samples[i]}/Experiment State.txt"
  ))

  num_ABRs <- as.numeric(str_extract(raw_experiment[3], "[0-9]+"))
  num_DPs <- as.numeric(str_extract(raw_experiment[4], "[0-9]+"))

  # can use these numbers as the iterators for the internal loop
  if (
    num_ABRs != length(ABR_analyzed_fp) || num_DPs != length(DP_analyzed_fp)
  ) {
    stop(
      "The wrangler did not find the expected number of ABR or DP files for sample {sample[i]}"
    )
  }
}

cur_sample <- str_glue("{projDir}/{samples[1]}")

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

# for each frequency

cur_file <- str_glue("{cur_sample}/{ABR_analyzed_fp[1]}")
header <- readLines(cur_file, n = 6)

#extract info about frequency and threshold from the header
freq <- str_extract(header[2], "\\d+\\.\\d+")
threshold <- str_extract(header[1], "\\d+\\.\\d+")

# Read the data as a tsv skipping the header
data <- read.delim(cur_file, sep = "\t", skip = 6)

#temp stored until stored in the list under its respective frequency name

cur_file <- str_glue("{cur_sample}/{ABR_wave_fp[1]}")
header <- readLines(cur_file, n = 7)

# Extract the levels from the header at header[6] with some str_extract_all call
levels <- unlist(str_extract_all(header[6], "\\d+"))

data <- read_delim(cur_file, delim = "\t", skip = 7, col_names = FALSE)
colnames(data) <- levels

# then run a similar process on the DP data

cur_file <- str_glue("{cur_sample}/{DP_analyzed_fp[1]}")

data <- read_delim(cur_file, delim = "\t")
# this just gets saved as the samples DP threshold

cur_file <- str_glue("{cur_sample}/{DP_wave_fp[1]}")
# Header row 6 is the colnames, but rows 1-5 and row 7 are not needed so, we need
# to read all the lines as data first then subset then read the data as a delim
data <- readLines(cur_file)
data <- data[-c(1:5, 7)]

# The following then reads nicely
data <- read_delim(I(data), delim = "\t")

# this can then be stored to the sample name in the list for further post-processing

# Now we need to implement all of the above in a generalized fashion to read any
# number of ABRs and DPs performed for any number of samples as that gives us all
# of the raw data parsed correctly

# need a list for each type of data I'm trying to look at ABRs analyzed and wave
# and DP analyzed and wave

# User input ----

# Prompt the user for how many peaks deep they would like the wrangled data to include
# this refers to which peaks to determine rawwaveforms for
# Prompt the user for the dB levels they would like amplitudes and latencies to
# be wrangled for

# ABR thresholds ----
# Make ABR threshold table by reading the top few lines of each ABR-analyzed.txt
# file, cut the headers from each file and turn the remaining matrix into a table
# named with the frequency

# DP thresholds ----
# make the DP thresholds by merging the DP-samplename-1-analyzed.txt file for each
# sample

# Peak Organization ----
