# Project Outline

My goal is to extend the usage  of Gunseli's ABR script. 

The main goal is to take the nonsensically structured data from an ABR experiment 
which should have some descriptive labeled sample, then rewrite the raw data within 
the subdirectories per each sample into a singular sheet for prism visualization.

## Raw Data structure

The raw data from the noise booth is a form of tsv, that has the information needed
for the ABR Analyzer Notebook to be able to graph and prompt the user for defining 
thresholds and peaks.

The script we are trying to make organizes the data from these seperate -analyzed.txt
files and converts it into a spreadsheet with sheets that are useful for common 
graphpad prism visualization workflows.
ounsure what data goes into this?) looks to be more than just at each frequency

## Desired Structure

It looks like Luis wants aggregation of all samples
* a summary sheet with the thresholds for each frequency for ABR and theshold summary
  for DPOAE
* Latencies and amplitudes for a certain dB level (currently hardcoded to only be 70 and 80 dB)
* Amplitudes across all levels for each frequency (for some reason excluding 40 kHz)
* ABR P1 waveform comes from the raw data
  * this is also where he wants the functionality extension to include the option 
  to include outputs for more peaks if desired.
* Similarly the DPOAE waveform which is the (2f2-f1) for each frequency from the raw data

# Outline of Solution

Bounds:
* samples: find from subdir names or config if snakemake
* frequencies measured: find from files within each subdir at a specific spot in the tsv
* dB range covered: find similar to frequencies
* ABR and DPOAE data present: can easily read expected files to find from the ExperimentState.txt 
  file present in the proj dir
* Peaks of interest: max or User defined
* dB levels of interest: User defined

Actionables:
1. Outline the structure of data and the best way to merge for multi-workflow usage
2. Define the datastructures you want to use to get the different endpoints
3. Tests: use the analyzer-output.xlsx and match the output to the different sheets
to test certain functions

Honestly this is simple enough we might not even need tests

# Notes on Gunseli's Approach
GetDPThresholds()
GetABRThresholds()
GetRawDPData()
GetRawABRData()
GetAmpSumm()
GetAnalyzedABRData(run_dirs)
fieldatdB(level_db, fieldname)
analyzedp(rootdir)
GetThreshold(dB,signal, noise)
GetDPData(fname)

All of these work in a general order from given a project directory
1. determining the number of samples in the project directory
    a. retaining only the directories relevant to samples
2. Aggregate the ABR data
  a. here is where there is also a user intensive analyzedp() call which
  bring up a user interaface to select thresholds
  b. which also relies on GetThreshold() to generate an initial estimate
  for the use to correct and GetDPdata() generate the data to feed to
  GetThreshold
3. Summarize thresholds of ABRs write to .xlsx
4. Same thing for DPOAE
5. A lot of repeated work for the different decibel thresholds *note that I would like to make this a loop*
    a. calls fileddatDB(numeric_decibel, "P1Ampltidue") to get amplitude looks to be hard coding for only the amplidtude
    b. calls fieldatdB(numeric_decibel, 'P1Latency') to get latencies
6. GetAmpSumm: Create sheets summarizing the amplitudes at each frequency
7. Extract raw waveforms: a little confused on these fucntions
    a. GetRawABRData
    b. GetRawDPDAta



Notes on matlab functions:
writetable() is used to write data to an excel spreadsheet on a specific
sheet as follows.
writetable(data,fp.xlsx,"Sheet","Sheet_Name")

step 5 in the process seems to be where a lot of the hardcoded nature of
the code comes from
we could offer the user a choice to tell us how many amplitudes they want
exported based on the initial function call. Then pre-generate a bunch of
names based on that number

this will require a few more checks relevant to the size of the
datastructure from ABRSoftware to ensure that the user can be wrong
mainly if they input a number greater than the number of available peaks
we simply ignore them and just do the max. Could also include an argument
for the decibel thresholds they care about because right now it is only
outputting those for 70 and 80 decibels


I don't think I need to mess with the threshold functions too much just
ensure that they are working

need to understand the get raw data better

need to understand why it is grabbing 80 and 70 decibels hardcoded


