# Utilities for the ABR wrangler that could be useful to resuse

# Validates the available files for a single sample, testing the number of available
# ABR and DP files against the experiment state file
valid_sample <- function(projDir, cur_sample) {
  expected_size <- readLines(str_glue(
    "{projDir}/{cur_sample}/Experiment State.txt"
  ))

  num_ABRs <- as.numeric(str_extract(expected_size[3], "[0-9]+"))
  num_DPs <- as.numeric(str_extract(expected_size[4], "[0-9]+"))

  # can use these numbers as the iterators for the internal loop
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
}
