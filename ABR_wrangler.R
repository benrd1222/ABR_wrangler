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

unique_freqs <- c() # want to keep track of the unique frequencies seen
unique_levels <- c()
unique_f2 <- c()

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

    #keep track of unique levels
    cur_levels <- unique(ABR_analyzed$Level)
    unique_levels <- unique(c(unique_levels, cur_levels))

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

  # save the unique f2 frequencies
  cur_f2 <- unique(DP_wave$`f2(Hz)`)
  unique_f2 <- unique(c(unique_f2, cur_f2))

  # save the output to it's sample name
  DP_thresholds[[samples[i]]] <- DP_thresh
  DP_peaks[[samples[i]]] <- DP_wave

  rm(
    list = c(
      'ABR_analyzed',
      'ABR_wave',
      'DP_thresh',
      'DP_wave',
      'cur_file',
      'cur_levels',
      'cur_sample',
      'expected_size',
      'freq',
      'header',
      'num_ABRs',
      'num_DPs',
      'threshold'
    )
  )
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

# may want this to be a user input
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
rm(tmp)

# now we similarly want to extract the waveforms at 80 dB for each sample across
# frequencies, needs to be it's own loop for order of the sheets

# could technically allow for the input of the waveforms into the ABR_extract function
# if there was an easy way to tell the datastructures apart. For now I will
# allocate it to it's own function
# TODO: consider moving this sample collating logic into the main function
# as that is what the rest of the functions in utils.R do
for (f in unique_freqs) {
  sheet_name <- str_glue("AllSample_ABRWaveforms@{f}kHz")
  tmp_list <- list()

  for (s in samples) {
    df_tmp <- ABR_waveforms[[s]]
    tmp_list[[s]] <- ABR_wave_extract(level = 80, freq = f, dw = df_tmp)
  }

  tmp_list_clean <- map(tmp_list, function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(numeric(0))
    }
    return(df[[1]]) # only one column we need
  })

  # maximum length across all samples
  max_len <- max(map_int(tmp_list_clean, length))

  df_out <- tmp_list_clean |>
    map(
      ~ {
        length(.x) <- max_len
        return(.x)
      }
    ) |>
    as_tibble()

  export_xl[[sheet_name]] <- df_out
}
rm(tmp_list, tmp_list_clean, df_out)

# Extracting the full set of waveforms per sample across all levels for a specific frequency
# TODO: see if users want a specific frequency and or less levels combined
wave_specific_f <- unique_freqs[1]

# the data of ABR_waveforms is already cleaned and setup to be exported in this format
# just add another loop if he wants all of the frequencies on seperate sheets,
# or just export the entire ABR_waveforms list
for (s in samples) {
  sheet_name <- str_glue("{s}_ABRWaveforms@{wave_specific_f}kHz")
  export_xl[[sheet_name]] <- ABR_waveforms[[s]] |>
    filter(frequency == wave_specific_f)
}

# similar loop for DP
# lastly we need to organize the DP waveform data
# need to keep track of the DP f2 frequencies in the main loop
# then here we just loop over the unique frequencies and take 2f2-f1 from each sample and exportl
# I don't think this is even ugly enough to need a helper function
for (f in unique_f2) {
  sheet_name <- str_glue("DP(2f1-f2)@{f}Hz")
  to_select <- "2f1-f2(dB)"
  out <- data.frame()

  for (s in samples) {
    new <- DP_peaks[[s]] |>
      select(`:dB`, `f2(Hz)`, {{ to_select }}) |>
      filter(`f2(Hz)` == f) |>
      select(-`f2(Hz)`)

    if (is_empty(out)) {
      out <- new
    } else {
      out <- out |>
        full_join(new, by = ":dB")
    }
  }
  colnames(out) <- c("Level", samples)

  export_xl[[sheet_name]] <- out
}

write_xlsx(export_xl, str_glue("{exportDir}/wrangler_output.xlsx"))
