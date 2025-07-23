function [AllData] = abr_dp_analyzer_v05(verbose)

if (verbose)
    disp("Verbose mode enabled")
end

originaldir = pwd;

disp("Please select the root directory containing the runs you wish to analyze")
rootdir = uigetdir;
cd(rootdir)
warning('off','MATLAB:table:ModifiedAndSavedVarnames')
output_f = fullfile(rootdir,'analyzer-output.xlsx');
if (isfile(output_f))
    delete(output_f)
end


% Get list of subdirectories of the selected root directory
d = dir(rootdir);
idir=[d(:).isdir]; % logical vector of whether this is a directory or file
dirs = {d(idir).name}; % extract only directories
dirs(ismember(dirs,{'.','..'})) = []; % remove . and .. from the list of directories



% All ABR data imported in table form
all_T = GetAnalyzedABRData(dirs);
if (verbose)
    disp("Finished uploading ABR analysis data")
end

% Summarize Thresholds and output to sheet
threshold_summary = GetABRThresholds();
writetable(threshold_summary,output_f, 'Sheet', 'ABR Thresholds')
if (verbose)
    disp("Threshold summary complete")
end

if (verbose)
    disp("Opening DP Analyzer")
end
DP_summ = GetDPThresholds();
writetable(DP_summ, output_f, 'Sheet', 'DP Thresholds')

% Create 80dB Amplitudes sheet
amp_summ_80 = fieldatdB(80, 'P1Amplitude');
writetable(amp_summ_80,output_f, 'Sheet', '80dB Amplitudes')
if (verbose)
    disp("80dB amplitude summary complete")
end

% Create 80dB Latencies sheet
lat_summ_80 = fieldatdB(80, 'P1Latency');

writetable(lat_summ_80,output_f, 'Sheet', '80dB Latencies')

if (verbose)
    disp("80dB latency summary complete")
end

% Create 70dB Latencies sheet
lat_summ_70 = fieldatdB(70, 'P1Latency');

writetable(lat_summ_70,output_f, 'Sheet', '70dB Latencies')

if (verbose)
    disp("70dB latency summary complete")
end

% Create sheets summarizing the amplitudes at each frequency
Mamp_summ = GetAmpSumm;
for key = Mamp_summ.keys
    writetable(Mamp_summ(key{:}),output_f, 'Sheet', sprintf("Amp@%.2fHz", key{:}))
end
if (verbose)
    disp("Amplitude sheets complete")
end

% Create Summary_70dB_Amplitudes sheet
amp_summ_70 = fieldatdB(70, 'P1Amplitude');

writetable(amp_summ_70,output_f, 'Sheet', 'Summary_70dB_Amplitudes')
if (verbose)
    disp("70dB amplitude summary complete")
end
% Create sheets for waveform at each frequency

ABRRaw = GetRawABRData;
for key = ABRRaw.keys
    writetable(ABRRaw(key{:}),output_f, 'Sheet', sprintf("ABR P1 Waveform@%.2fHz", key{:}))
end
if (verbose)
    disp("Raw data extraction complete")
end

DPRaw = GetRawDPData;
for key = DPRaw.keys
    writetable(DPRaw(key{:}),output_f, 'Sheet', sprintf("DP (2f2-f1) Waveform@%.2fkHz", key{:}/1000))
end


cd(originaldir)

if (verbose)
    disp("ABR/DP analysis complete")
end

%% functions

    function DP_summ = GetDPThresholds()
        AllData = analyzedp(rootdir);
        DP_summ = table();
        for run = AllData
            run_t = table();
            for k = 1:length(run{:}.data)
                subrundata = run{:}.data{k};
                th = 0;
                if (isfield(subrundata, 'userthresh'))
                    th = subrundata.userthresh;
                elseif (isfield(subrundata, 'mthresh'))
                    th = subrundata.mthresh;
                else
                    th = subrundata.autothresh;
                end
                temp_t = table(subrundata.f2, th,'VariableNames',{'f2',run{:}.runname});
                run_t = [run_t; temp_t];
            end
            if (isempty(DP_summ))
                DP_summ = run_t;
            else
                DP_summ = outerjoin(DP_summ, run_t, 'MergeKeys', true);
            end
        end

    end
    function [threshold_summary] = GetABRThresholds()
        threshold_summary = table;

        for i=1:height(all_T)
            % For each run, create a new table with the frequency/threshold pairs
            name = all_T.run_name{i};
            freqs_merge = table;
            freqs_merge.('Frequency (kHz)') = all_T.run_data{i}.freq;

            freqs_merge.(name) = all_T.run_data{i}.thresh;
            if i == 1
                threshold_summary = freqs_merge;
            else
                % Merge the table to the last one, combining frequency columns
                threshold_summary = outerjoin(threshold_summary, freqs_merge, 'MergeKeys', true);
            end
        end

    end


    function [DPRaw] = GetRawDPData()
        DPRaw = containers.Map('KeyType','double','ValueType','any');
        for i = 1:length(AllData)
            for j = 1:length(AllData{i}.data)
                sr = AllData{i}.data{j};
                sr_ad = table(sr.dB, sr.signal, 'VariableNames',{'dB' AllData{i}.runname});
                freq = sr.f2;
                if(~isKey(DPRaw,freq) || isempty(DPRaw(freq)))
                    DPRaw(freq) = sr_ad;
                else
                    % Merge the table to the last one
                    DPRaw(freq) = outerjoin(DPRaw(freq), sr_ad, 'MergeKeys', true);
                end
            end
        end
    end

    function [Mraw] = GetRawABRData()
        Mraw = containers.Map('KeyType','double','ValueType','any');

        for run = dirs
            run_name = run{:};
            working_d = fullfile(rootdir,run_name);
            cd(working_d)

            raw_subrun_data = {dir("ABR*").name};
            % Remove analysis files from the list
            raw_subrun_data = raw_subrun_data(~contains(raw_subrun_data,"analyzed"));
            for subrun = raw_subrun_data
                subrun_name = subrun{:};

                fid = fopen(subrun_name);
                freq = -1;
                levels = [];
                num_lines_read = 0;
                while true
                    num_lines_read = num_lines_read + 1;
                    line = fgetl(fid);
                    if startsWith(line, ':SW EAR:')
                        freq = strsplit(line, '\t');
                        freq = strsplit(freq{2},":");
                        freq = str2double(freq{2});

                    elseif startsWith(line, ':LEVELS:')
                        levels = strsplit(line, ":");
                        levels = strsplit(levels{3},";"); %levels are semicolon delimited

                        for i = 1:length(levels)
                            %convert the levels to double
                            levels{i} = str2double(levels{i});
                        end
                    elseif startsWith(line, ':DATA')
                        break;
                    end
                end

                fclose(fid);



                loc_80 = find([levels{:}] == 80);
                if ~isempty(loc_80)
                    data=readtable(subrun_name,'NumHeaderLines',num_lines_read,'VariableNamingRule','modify','delimiter','\t');
                    data_80 = data(:,loc_80);
                    data_80 = renamevars(data_80, 1, sprintf("%s-%.2fkHz", run_name, freq));
                    if(~isKey(Mraw,freq) || isempty(Mraw(freq)))
                        Mraw(freq) = data_80;
                    else
                        % Merge the table to the last one
                        Mraw(freq) = [Mraw(freq) data_80];
                    end
                    if (verbose)
                        fprintf("Extracted raw data from %s \n", subrun_name)
                    end
                end
            end
        end
    end

    function [Mamp_summ] = GetAmpSumm()
        Mamp_summ = containers.Map('KeyType','double','ValueType','any');

        for i = 1:height(all_T)
            run_name = all_T.run_name{i};
            run_data = all_T.run_data{i};
            for j = 1:height(run_data)
                amps_merge = table;
                freq = run_data.freq(j);
                name_freq = sprintf("%s-%.2fkHz",run_name, freq); %Generate name for run/freq

                % Extract rows of p1amp where the Level(dB) is divisible by 10
                lind = (mod(run_data.data{j}.Level,10) == 0);
                amps_merge.level = run_data.data{j}.Level(lind);
                amps_merge.(name_freq) = all_T.run_data{i}.data{j}.P1Amplitude(lind);

                if(~isKey(Mamp_summ,freq) || isempty(Mamp_summ(freq)))
                    Mamp_summ(freq) = amps_merge;
                else
                    % Merge the table to the last one, combining db columns
                    Mamp_summ(freq) = outerjoin(Mamp_summ(freq), amps_merge, 'MergeKeys', true);
                end

            end
        end
    end

    function [all_T] = GetAnalyzedABRData(run_dirs)
        all_T = table;
        for run = run_dirs
            % Change directories to current run and extract list of analyzed abrs
            run_name = run{:};
            if (verbose)
                fprintf("Getting analyzed data from %s \n",run_name)
            end
            working_d = fullfile(rootdir,run_name);
            cd(working_d)

            % subruns have analyzed txt of the form ABR__analyzed
            analyzed_subruns = {dir("ABR*analyzed*").name};

            run_data = table;
            for subrun = analyzed_subruns
                subrun_name = subrun{:};


                fid = fopen(subrun_name);
                freq = -1;
                thresh = -1;
                num_lines_read = 0;

                % Read analyzed file and extract data from lines containing
                % threshold and frequency information
                while true
                    num_lines_read = num_lines_read + 1;
                    line = fgetl(fid); %Get another line from file

                    if startsWith(line, 'Threshold (dB SPL)')
                        thresh = strsplit(line, ':');
                        thresh = str2double(thresh{2});


                    elseif startsWith(line, 'Frequency (kHz)')
                        freq = strsplit(line, ':');
                        freq = str2double(freq{2});

                    elseif startsWith(line, 'Level')
                        break;
                    end
                end

                lines_to_skip = num_lines_read - 1;

                fclose(fid);

                % Extract data from tab-delimited file,
                % allowing matlab to change names of vars
                dataT=readtable(subrun_name,'NumHeaderLines',lines_to_skip,'VariableNamingRule','modify','delimiter','\t');

                c={};
                c{1, 1} = run_name;
                c{1, 2} = freq;
                c{1, 3} = thresh;
                c{1, 4} = dataT;

                T1=cell2table(c,'VariableNames',{'name', 'freq','thresh','data'});

                % append the data from this subrun to run_data
                run_data=[run_data;T1];
            end

            % Add data from this run to the main table all_T
            to_add = cell2table({run_name, run_data},'VariableNames',{'run_name', 'run_data'});
            all_T = [all_T;to_add];
        end
    end

    function amp_summ = fieldatdB(level_db, fieldname)
        amp_summ = table;

        for i = 1:height(all_T)
            run_name = all_T.run_name{i};
            run_data = all_T.run_data{i};

            % Empty table with a column for freq and a column for P1amp named after
            % the run
            amps_merge = array2table(zeros(0,2),'VariableNames',{'Frequency',run_name});

            for j = 1:height(run_data)
                freq = run_data.freq(j);

                lind = (run_data.data{j}.Level == level_db);
                if (sum(lind) > 0)
                    amp = run_data.data{j}.(fieldname)(lind);
                    amps_merge = [amps_merge;{freq,amp}];
                end
            end

            if i == 1
                amp_summ = amps_merge;
            else
                % Merge the table to the last one, combining frequency columns
                amp_summ = outerjoin(amp_summ, amps_merge, 'MergeKeys', true);
            end

        end
    end
end


function [AllData] = analyzedp(rootdir)
AllData = {};

% Get list of subdirectories of the selected root directory
d = dir(rootdir);
idir=[d(:).isdir]; % logical vector of whether this is a directory or file
dirs = {d(idir).name}; % extract only directories
dirs(ismember(dirs,{'.','..'})) = []; % remove . and .. from the list of directories

Num_Runs = 0;

for subdir = dirs
    run_name = subdir{:};
    finfo = dir(fullfile(rootdir,run_name,"DP-*")); % get DP files

    finfo = finfo(~contains({finfo.name},"analyzed")); % remove analysis files

    fmeta = struct;
    fmeta.plotlims = [15    85   -30    70];
    fmeta.fname = finfo.name;
    fmeta.runname = run_name;
    fmeta.folder = finfo.folder;
    fmeta.data = GetDPData(fullfile(fmeta.folder, fmeta.fname));

    % Establish automatically determined thresholds for each subrun
    for i = 1:length(fmeta.data)
        fmeta.data{i}.autothresh = GetThreshold(fmeta.data{i}.dB, fmeta.data{i}.signal, fmeta.data{i}.noise);
    end

    saved_thresh = fullfile(fmeta.folder, strcat(fmeta.fname, '-analyzed.txt'));

    if(isfile(saved_thresh))
        T = readtable(saved_thresh);
        for i = 1:length(fmeta.data)
            fmeta.data{i}.mthresh = T.thresh(T.f2 == fmeta.data{i}.f2);
        end

    end

    AllData{end+1} = fmeta;

    Num_Runs = Num_Runs + 1;
end

Current_Run=1;

prettyblue = [57 106 177]./255;
red = [204 37 41]./255;
prettyblack = [83 81 84]./255;
prettygreen = [62 150 81]./255;
brown = [146 36 40]./255;
purple = [107 76 154]./255;

fig = uifigure('Position',[100 100 500 315]);

grid1 = uigridlayout(fig,[6 2]);
grid1.ColumnWidth = {40, 80, 80, '1x', 80, 120};
grid1.RowHeight = {'1x', 20};

% Create Panel
p = uipanel(grid1);
p.Title = 'Panel';
p.Layout.Row = 1;
p.Layout.Column = [1 6];
p.TitlePosition = 'centertop';
p.FontSize = 24;
p.FontWeight = 'bold';


% Create Button
lbutton = uibutton(grid1, 'push','ButtonPushedFcn', @PrevButtonPushed);
lbutton.Layout.Row = 2;
lbutton.Layout.Column = 2;
lbutton.Text = 'prev';


% Create Button2
rbutton = uibutton(grid1, 'push', 'ButtonPushedFcn', @NextButtonPushed);
rbutton.Layout.Row = 2;
rbutton.Layout.Column = 3;
rbutton.Text = 'next';

% Create Button3
dbutton = uibutton(grid1, 'push', 'ButtonPushedFcn', @SaveThresholds);
dbutton.Layout.Row = 2;
dbutton.Layout.Column = 5;
dbutton.Text = 'Save';

% Create Button4
dbutton = uibutton(grid1, 'push', 'ButtonPushedFcn', @SaveAndClose);
dbutton.Layout.Row = 2;
dbutton.Layout.Column = 6;
dbutton.Text = 'Save and Close';

axhandles = [];
linehandles = [];
hLineToDrag = [];
linemovementinc = 5;
threshchanged = false;

UpdateDisplayedDP(1);


    function SaveAndClose(~, ~)
        SaveThresholds();
        delete(fig);
    end

    function SaveThresholds(~,~)
        for run = AllData
            run_t = table();
            for k = 1:length(run{:}.data)
                subrundata = run{:}.data{k};
                th = 0;
                if (isfield(subrundata, 'userthresh'))
                    th = subrundata.userthresh;
                elseif (isfield(subrundata, 'mthresh'))
                    th = subrundata.mthresh;
                else
                    th = subrundata.autothresh;
                end
                temp_t = table(subrundata.f2, th,'VariableNames',{'f2','thresh'});
                run_t = [run_t; temp_t];
            end

            writetable(run_t,fullfile(run{:}.folder, strcat(run{:}.fname, '-analyzed.txt')),'Delimiter','\t');
        end

        threshchanged = false;
    end

    function NextButtonPushed(~,~)

        if (Current_Run + 1 <= Num_Runs)
            Current_Run = Current_Run + 1;
            UpdateDisplayedDP(Current_Run)
        end

        UpdateButtonsEnabled()
    end

    function PrevButtonPushed(~,~)

        if (Current_Run-1 >= 1)

            Current_Run = Current_Run - 1;
            UpdateDisplayedDP(Current_Run)
        end

        UpdateButtonsEnabled()
    end

    function UpdateButtonsEnabled()
        if(Num_Runs == 1)
            rbutton.Enable = 'off';
            lbutton.Enable = 'off';
        else
            if (Current_Run == 1)
                lbutton.Enable = 'off';
                rbutton.Enable = 'on';
            elseif Current_Run == Num_Runs
                lbutton.Enable = 'on';
                rbutton.Enable = 'off';
            else
                lbutton.Enable = 'on';
                rbutton.Enable = 'on';
            end
        end
    end

    function UpdateDisplayedDP(DP_ID)
        axhandles = [];
        linehandles = [];
        hLineToDrag = [];

        p.Title = AllData{DP_ID}.runname;
        numDP = length(AllData{DP_ID}.data);

        t=[];
        if (numDP <= 4)
            t = tiledlayout(p,2,2,'TileSpacing','Compact', 'Padding', 'compact');
        elseif numDP <= 9
            t = tiledlayout(p,3,3,'TileSpacing','Compact', 'Padding', 'compact');
        elseif numDP <= 12
            t = tiledlayout(p,3,4,'TileSpacing','Compact', 'Padding', 'compact');
        else
            t = tiledlayout(p,4,4,'TileSpacing','Compact', 'Padding', 'compact');
        end


        for data = AllData{DP_ID}.data
            ax = nexttile(t);
            hold(ax, 'on')
            plot(ax, data{:}.dB, data{:}.noise, 'Color', prettyblack, 'Marker', 'o', 'MarkerFaceColor', prettyblack,'MarkerSize', 3)
            plot(ax, data{:}.dB, data{:}.signal, 'Color', prettyblue, 'Marker', 'o', 'MarkerFaceColor', prettyblue,'MarkerSize', 3)
            axis(ax, AllData{DP_ID}.plotlims)
            title(ax,sprintf("f2: %d Hz",data{:}.f2))

            if isfield(data{:}, 'userthresh')
                linehandles = [linehandles xline(ax,data{:}.userthresh,'--k')];
            elseif isfield(data{:}, 'mthresh')
                linehandles = [linehandles xline(ax,data{:}.mthresh,'--k')];
            else
                linehandles = [linehandles xline(ax,data{:}.autothresh,'--k')];
            end
            hold(ax, 'off')

            height = (AllData{DP_ID}.plotlims(4) - AllData{DP_ID}.plotlims(3))*0.07;

            line(ax, [data{:}.autothresh data{:}.autothresh], [AllData{DP_ID}.plotlims(3) AllData{DP_ID}.plotlims(3)+height], 'Color', prettygreen, 'LineWidth', 4)
            line(ax, [data{:}.autothresh data{:}.autothresh], [AllData{DP_ID}.plotlims(4)-height AllData{DP_ID}.plotlims(4)], 'Color', prettygreen, 'LineWidth', 4)

            if(isfield(data{:}, 'mthresh') && data{:}.mthresh ~= data{:}.autothresh)
                line(ax, [data{:}.mthresh data{:}.mthresh], [AllData{DP_ID}.plotlims(3) AllData{DP_ID}.plotlims(3)+height], 'Color', purple, 'LineWidth', 4)
                line(ax, [data{:}.mthresh data{:}.mthresh], [AllData{DP_ID}.plotlims(4)-height AllData{DP_ID}.plotlims(4)], 'Color', purple, 'LineWidth', 4)

            end
            axhandles = [axhandles ax];
        end

        UpdateButtonsEnabled();
    end

set(fig,'WindowButtonDownFcn',  @mouseDown);
set(fig,'WindowButtonMotionFcn',@mouseMove);
set(fig,'WindowButtonUpFcn',    @mouseUp);
set(fig,'CloseRequestFcn',@closereq)


    function closereq(obj, ~, ~)
        if (threshchanged)
            selection = questdlg('Save threshold changes?', 'DPAnalyzer', 'Save', "Don't Save", 'Cancel','Cancel');
            switch selection,
                case 'Save'
                    SaveThresholds();
                case "Don't Save"
                    delete(obj)
                case 'Cancel'
                    return
            end
        else
            delete(obj)
        end
    end


    function mouseDown(hCursor,~)
        current_axis = GetCurrentMouseAxis(hCursor, axhandles);

        if(current_axis ~= 0)
            hLineToDrag = linehandles(current_axis);
        end
    end

    function mouseUp(hCursor,~)
        currentaxis = GetCurrentMouseAxis(hCursor, axhandles);
        if (~isempty(hLineToDrag) && currentaxis ~= 0)
            MoveCurrentLineToCursor(currentaxis);
        end
        hLineToDrag = [];

    end

    function mouseMove(hCursor,~)
        currentaxis = GetCurrentMouseAxis(hCursor, axhandles);
        if (~isempty(hLineToDrag) && currentaxis ~= 0)
            MoveCurrentLineToCursor(currentaxis);
        end
    end

    function MoveCurrentLineToCursor(currentaxis)
        hAxis = axhandles(currentaxis);
        CP = get(hAxis, 'CurrentPoint');
        xVal = CP(1,1);
        xVal = round(xVal / linemovementinc) * linemovementinc;
        set(hLineToDrag, 'Value', xVal)

        AllData{Current_Run}.data{currentaxis}.userthresh = xVal;

        threshchanged = true;
    end

    function [axis] = GetCurrentMouseAxis(hCursor, axhandles)
        figCurrentPoint = get(hCursor, 'CurrentPoint');
        position      = get(hCursor, 'Position');
        xCursor       = figCurrentPoint(1,1)/position(1,3); % normalize
        yCursor       = figCurrentPoint(1,2)/position(1,4); % normalize

        axis = 0;
        for i=1:length(axhandles)
            axisPos = get(axhandles(i), 'Position');
            minx = axisPos(1);
            miny = axisPos(2);
            maxx = minx + axisPos(3);
            maxy = miny + axisPos(4);

            if xCursor >= minx && xCursor <= maxx && yCursor >= miny && yCursor <= maxy
                axis = i;
            end
        end
    end

waitfor(fig)
end

function [thresh] = GetThreshold(dB,signal, noise)
lThresh = (signal - noise) >= 3;

last_val_one = (lThresh(1) == 1);
for i = 2:length(lThresh)
    if (last_val_one && lThresh(i) == 1)
        thresh = dB(i-1);
        return;
    else
        last_val_one = (lThresh(i) == 1);
    end
end
thresh = 0;
end

function [DPlist] = GetDPData(fname)
% Import the options of the csv file
opts=detectImportOptions(fname);
%Defines the row location of channel variable name
opts.VariableNamesLine = 6;
opts.VariableNamingRule = 'preserve';
%Specifies that the data is comma seperated
%Read the table
t = readtable(fname,opts, 'ReadVariableNames', true);

[~, ia] = unique(t.('f1(Hz)'), 'rows');
ia = [ia; height(t)+1];

DPlist=cell(1,length(ia)-1);
for k = 1:length(ia)-1
    DPlist{k}= struct;
    DPlist{k}.dB = t{ia(k):(ia(k+1)-1),1};
    DPlist{k}.f1 = t{ia(k),2};
    DPlist{k}.f2 = t{ia(k),3};
    DPlist{k}.signal = t{ia(k):(ia(k+1)-1),6};
    DPlist{k}.noise = t{ia(k):(ia(k+1)-1),7};
end

end
