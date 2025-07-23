%% Unit Tests

% testing on dataset given here 
% C:\Users\benjad\University of Michigan Dropbox\Benjamin Daniel\ABRs for Ben

% Functions to test include

% GetDPThresholds()
% GetABRThresholds()
% GetRawDPData()
% GetRawABRData()
% GetAmpSumm()
% GetAnalyzedABRData(run_dirs)
% fieldatdB(level_db, fieldname)
% analyzedp(rootdir)
% GetThreshold(dB,signal, noise)
% GetDPData(fname)

% All of these work in a general order from given a project directory
% 1. determining the number of samples in the project directory
%     a. retaining only the directories relevant to samples
% 2. Aggregate the ABR data
%   a. here is where there is also a user intensive analyzedp() call which
%   bring up a user interaface to select thresholds
%   b. which also relies on GetThreshold() to generate an initial estimate
%   for the use to correct and GetDPdata() generate the data to feed to
%   GetThreshold
% 3. Summarize thresholds of ABRs write to .xlsx
% 4. Same thing for DPOE
% 5. A lot of repeated work for the different decibel thresholds *note that I would like to make this a loop*
%     a. calls fileddatDB(numeric_decibel, "P1Ampltidue") to get amplitude looks to be hard coding for only the 1 amplidtude
%     b. calls fieldatdB(numeric_decibel, 'P1Latency') to get latencies
% 6. GetAmpSumm: Create sheets summarizing the amplitudes at each frequency
% 7. Extract raw waveforms: a little confused on these fucntions
%     a. GetRawABRData
%     b. GetRawDPDAta



% Notes on matlab functions:
% writetable() is used to write data to an excel spreadsheet on a specific
% sheet as follows.
%writetable(data,fp.xlsx,"Sheet","Sheet_Name")

% step 5 in the process seems to be where a lot of the hardcoded nature of
% the code comes from
% we could offer the user a choice to tell us how many amplitudes they want
% exported based on the initial function call. Then pregenerate a bunch of
% names based on that number

% this will require a few more checks relevant to the size of the
% datastructure from ABRSoftware to ensure that the user can be wrong
% mainly if they input a number greater than the number of available peaks
% we simply ignore them and just do the max. Could also include an argument
% for the decibel thresholds they care about because right now it is only
% outputting those for 70 and 80 decibels


% I don't think I need to mess with the threshold functions too much just
% ensure that they are working

% need to understand the get raw data better

% need to understand why it is grabbing 80 and 70 decibels



function [test_T] = 