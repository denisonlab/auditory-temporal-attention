%% Safety setup

%{
% Detach Snd from any shared PsychPortAudio handle left by a prior run.
try
    Snd('Close');
catch
end
%}

% Then close any remaining PsychPortAudio devices.
try
    PsychPortAudio('Close');
catch
end

sca;
clear all;
commandwindow;

%% PTB setup
% Check we are running PTB-3
AssertOpenGL;

% Skip screen tests - ONLY for demo, not for real experiments
%Screen('Preference', 'SkipSyncTests', 1);
Screen('Preference', 'SkipSyncTests', 0);

%% Basic info
% Load parameters for this experiment
p = ta_auditory_params_3;
validExperimentModes = [0 1 2 3 4 6 7 8];

if ~ismember(p.staircasing, validExperimentModes)
    error('Unsupported p.staircasing value: %d', ...
        p.staircasing)
end

%% Validate cue-display setting
p.ringOrNum = lower(char(p.ringOrNum));

if ~ismember(p.ringOrNum, {'ring','number'})
    error(['p.ringOrNum must be either ''ring'' or ''number''. ' ...
        'Current value: %s'], p.ringOrNum)
end

%% Validate fixation-dimming setting
p.grayWhen = lower(char(p.grayWhen));

if ~ismember(p.grayWhen, {'none','delay','response'})
    error(['p.grayWhen must be ''none'', ''delay'', or ''response''. ' ...
        'Current value: %s'], p.grayWhen)
end

%% Name the subject
%subjectID = 'PilotJW_VisualBlips'; %'E2_S0002_Stair3'; %'E2_S0070'; %'E2_S0070_Stair2'; %'E2_S0096_Stair2'; % 'S0007Newsession'; %'S0052Newsession';
%subjectID='test_mle';
subjectID='S0149';
run = 6; %7; %1; %6; %8; %2; %6;
%% ^^^^^ for collaborating colleagues -- all you have to do is increase run by one and run the code! - JW
%% Data organization and filenames

% Generate one timestamp for both the MAT and EDF files
sessionTime = now;

dataRoot = 'data_e3';
dataDir = fullfile(dataRoot, subjectID);
expName = 'ta-auditory';

% Determine experiment folder and one-letter EDF code
if p.staircasing == 0
    expType = 'main';
    expCode = 'M';
elseif p.staircasing == 1
    expType = 'staircasing';
    expCode = 'S';
elseif p.staircasing == 8
    expType = 'psychometric';
    expCode = 'Y';
else
    expType = 'practice';
    expCode = 'P';
end

% Create the subject's four experiment folders if they do not exist
dataFolders = {'main', 'staircasing', 'psychometric', 'practice'};

for iDataFolder = 1:numel(dataFolders)
    thisDataFolder = fullfile(dataDir, dataFolders{iDataFolder});

    if ~exist(thisDataFolder, 'dir')
        mkdir(thisDataFolder);
    end
end

% Folder for this particular session
sessionDataDir = fullfile(dataDir, expType);

% Send the EDF into the same folder as the MAT file
p.eyeDataDir = sessionDataDir;

% MAT filename:
% SXXXX_type_run0X_ta-auditory_YYYYMMDD_HHMMSS.mat
dataFile = fullfile(sessionDataDir, sprintf( ...
    '%s_%s_run%02d_%s_%s.mat', ...
    subjectID, expType, run, expName, ...
    datestr(sessionTime, 'yyyymmdd_HHMMSS')));

existingRunFiles = dir(fullfile(sessionDataDir, sprintf( ...
    '%s_%s_run%02d_%s_*.mat', ...
    subjectID, expType, run, expName)));

if ~isempty(existingRunFiles)
    error(['Data already exist for subject %s, type %s, ' ...
        'run %02d. Increase the run number or resolve the ' ...
        'existing file first.'], ...
        subjectID, expType, run)
end

% The chosen EDF convention requires a one-digit run number
if p.eyetracking && ...
        (run < 1 || run > 9 || run ~= fix(run))
    error('EyeLink EDF filename requires an integer run number from 1 to 9.')
end

% EDF filename:
% run + experiment code + DDHHMM
% Example: 2M271430.edf
eyeFile = sprintf('%d%s%s', ...
    run, expCode, datestr(sessionTime, 'ddHHMM'));

eyeDataFile = fullfile(sessionDataDir, [eyeFile '.edf']);

% Never overwrite existing MAT or EDF data
if exist(dataFile, 'file')
    error('MAT data file already exists: %s', dataFile)
end

if p.eyetracking && exist(eyeDataFile, 'file')
    error('EDF data file already exists: %s', eyeDataFile)
end


%% Initialize experiment output
expt = struct;

if p.staircasing==1 %perform staircasing
        
        %remember we are doing 2 staircases at the same time - one starting easy,
        %one starting difficult. hoping they converge!
        stairIdx_s1 = numel(p.stairs); % last step, easiest
        stairIdx_s2 = 1; % first step, most difficult
        
        %track last few for updateStaircase.m
        lastFewAcc_s1 = [];
        lastFewAcc_s2 = [];
        
        %track trial counts for each staircase
        % nTrials_s1=p.staircaseTrials;
        % nTrials_s2=p.staircaseTrials;
        
        %define empty vars to store all staircasing tilts
        tiltVals_s1=[];
        tiltVals_s2=[];
        
        completedStairTrials_s1 = 0;
        completedStairTrials_s2 = 0;
end

%% %%%% Setup: window, sound, and keyboard %%%%

%% Window
% Here we open a PTB window and see how to get several properties of the
% window.

%% Setting up the window ...
% Pick the screen on which to display the window
screenNumber = max(Screen('Screens'));

% Open a PTB window
[window, rect] = Screen('OpenWindow', screenNumber); % defaults to full screen
%[window, rect] = Screen('OpenWindow', screenNumber, [], [0 0 960 540]);

%% Getting many useful properties of the window ...
% Get x and y coordinates for the center of the window
[cx, cy] = RectCenter(rect);

%% Sound setup -- must occur before EyeLink initialization/calibration

% Close any previously open audio devices
PsychPortAudio('Close');

% Initialize low-latency PsychPortAudio
InitializePsychSound(1);

% Open the experiment's stereo output device
reqlatencyclass = 1;

pahandle = PsychPortAudio('Open', p.deviceNum, [], reqlatencyclass, p.Fs, 2);

% Allow Snd, Beeper, and EyeLink calibration sounds to share this handle
%Snd('Open', pahandle);

%% EyeLink setup
if p.eyetracking
    % eyeFile and p.eyeDataDir were created above using sessionTime
    [el, exitFlag] = rd_eyeLink('eyestart', window, eyeFile);
    if exitFlag
        return
    end

    [~, exitFlag] = rd_eyeLink('calibrate', window, el);
    if exitFlag
        return
    end
else
    eyeFile = '';
end

% Get the color code for white
white = WhiteIndex(window);  

% Get window size
[screenWidthPx, screenHeightPx] = Screen('WindowSize', window);

% Get refresh rate
flipInterval = Screen('GetFlipInterval', window); % frame duration (s)

% We will request the screen to flip half a refresh (the "slack" time) before we 
% actually want the screen to change. This helps to avoid late screen
% flips. So let's define this "slack" variable for convenience.
slack = flipInterval/2;

%{
%% Sound

% Nonfunctional on Windows

% Initialize the sound driver
%set up audio handle. make sure anything open is closed and open new ones
PsychPortAudio('Close');

InitializePsychSound(1); % 1 for precise timing

% Open audio device for low-latency output
reqlatencyclass = 1; % Level 2 means: Take full control over the audio device, even if this causes other sound applications to fail or shutdown.
%pahandle = PsychPortAudio('Open', [], [], reqlatencyclass, p.Fs, 1); % 1 = single-channel
%% write script to mitigate this, artifical deadline
%pahandle = PsychPortAudio('Open', 8, [], reqlatencyclass, p.Fs, 1); % 1 = single-channel


%pahandle = PsychPortAudio('Open', p.deviceNum, [], reqlatencyclass, p.Fs, 1); % 1 = single-channel

pahandle = PsychPortAudio('Open', p.deviceNum, [], reqlatencyclass, p.Fs, 2);

%pamaster = PsychPortAudio('Open', p.deviceNum, [], reqlatencyclass, p.Fs, 2); % 1 = single-channel
%pahandle = PsychPortAudio('OpenSlave', pamaster [, mode][, channels][, selectchannels]);


%pahandle = PsychPortAudio('Open', 0)
%{
% From MLE ta-fmri
%set up audio handle. make sure anything open is closed and open new ones
PsychPortAudio('Close');

pahandle = PsychPortAudio('Open', [], 1, 1, p.Fs, 2);
Snd('Open', pahandle); %nec for eyetracker
%}
%}

%% Keyboard
% Check all "devices" (keyboards, mice) for response input
devNum = -1;

%% Calculate stimulus dimensions (px) and position
p.screenWidthCm = Screen('DisplaySize', screenNumber)/10;
pixelsPerDegree = ang2pix(1, p.screenWidthCm, screenWidthPx, p.viewDistCm);

fixSize = p.fixSize*pixelsPerDegree;
outerRingSize = p.outerRingSize*pixelsPerDegree;
innerRingSize = p.innerRingSize*pixelsPerDegree;
eyetrackSize = (p.eyetrackSize/2) * pixelsPerDegree;
fixBreakTextOffsetPx = ...
    p.fixBreakTextOffset * pixelsPerDegree;

%% Specify how many conditions

if p.staircasing == 0
    %simple trial conditions matrix
    responseCues = [1 2]; %which target is responded to, AKA targets
    cueValidities = [1 1 1 -1 0]; % 60 valid, 20 invalid, 20 neutral
    % 5 logarithmically-spaced target axis frequencies in a conservative
    % "safe" range, starting with 800 and ending with 2400 Hz
    targetAxes = logspace(log10(800), log10(2400), 5);
        targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 5)
    targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 2)
    sweepDirs = {'up','down'};
    % sweepTilts = targetAxes*0.016; % change to proportion? .125
        % Find the most recently saved staircase session
    staircaseDir = fullfile(dataDir, 'staircasing');
    
    staircaseFiles = dir(fullfile(staircaseDir, sprintf( ...
        '%s_staircasing_run*_ta-auditory_*.mat', subjectID)));
    
    if isempty(staircaseFiles)
        error('No staircase data file found for subject %s.', subjectID)
    end
    
    [~, newestStaircaseIdx] = max([staircaseFiles.datenum]);
    
    thresholdFile = fullfile( ...
        staircaseDir, staircaseFiles(newestStaircaseIdx).name);
    expt_staircase = load(thresholdFile, 'expt');
    subjectThresholdMean = expt_staircase.expt.threshold_avg;
    sweepTilts = targetAxes*subjectThresholdMean;
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    
    %generate all condition combinations
    trials = fullfact([nResponseCues nCueValidities nTargetAxes nTargetAxes nSweepDirs nSweepDirs]);
    trials = vertcat(trials, trials, trials)
    nTrials = p.experimentTrials;
    %nTrials = 10;
    
    %{
     %generate random trial order
        trialOrder = randperm(size(trials,1));
        trialOrder = trialOrder(1:nTrials);
        % 1000 different permutations, try them over 1-2 sessions
    %}

    % if new subject (run = 1), generate random trial order and save
    trialOrderFile = fullfile( ...
    dataDir, 'main', sprintf('%s_trialOrder.mat', subjectID));
    if run == 1
        %trialOrder = randperm(size(trials,1));
        trialOrderSession1 = randperm(size(trials,1));
        trialOrderSession2 = randperm(size(trials,1));
        trialOrder = horzcat(trialOrderSession1, trialOrderSession2);
        existingTrialOrderFile = dir(trialOrderFile);
        if ~isempty(existingTrialOrderFile) && ~strcmp(subjectID, 'test') && ~strcmp(subjectID, 'testy')
            error('trial order file already exists!')
        else
           save(trialOrderFile, "trialOrder")
        end
    end
    % 1000 different permutations, try them over 5 sessions of 200 blocks
    load(trialOrderFile);
    trialNumberBegins = (run*nTrials) - nTrials + 1;
    trialNumberEnds = trialNumberBegins + nTrials - 1;
    trialOrder = trialOrder(trialNumberBegins:trialNumberEnds);
end

%% Staircasing
%% define trials
if p.staircasing == 1
        
    responseCues = [1 2]; %which target is responded to, AKA targets
    cueValidities = [0]; % all neutral
    % 5 logarithmically-spaced target axis frequencies in a conservative
    % "safe" range, starting with 800 and ending with 2400 Hz
    targetAxes = logspace(log10(800), log10(2400), 5);
       targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 5)
    targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 2)
    sweepDirs = {'up','down'};
    sweepTilts = targetAxes*0.016; % change to proportion? .125
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    
    % generate all condition combinations
    trials_stair = fullfact([nResponseCues nCueValidities nTargetAxes nTargetAxes nSweepDirs nSweepDirs]);
    trials_stair = vertcat(trials_stair, trials_stair, trials_stair, trials_stair, trials_stair)
    nTrials_stair = p.staircaseTrials;
    
    % generate random trial order
    trialOrder_stair = randperm(size(trials_stair,1));
    trialOrder_stair = trialOrder_stair(1:nTrials_stair);
%    Unable to perform assignment because dot indexing is not supported for variables of this type.
    currentStaircase = 1;
    % make sure that there's enough of each axis... let's try this for now
    
    nTrials = nTrials_stair;
    trialOrder = trialOrder_stair;
    trials = trials_stair;
end

if p.staircasing == 2
    % for the Psychometric Function Self-Piloting JW:
    responseCues = [1]; % cue only the one target T1
    cueValidities = [0 0]; % all neutral cues
    targetAxes = logspace(log10(800), log10(2400), 5);
        targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 5)
   targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 2)
    sweepDirs = {'up','down'};
    sweepTilts = [0.330, 0.230];
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    %% count the sweepTilts in the trial sequence
    nSweepTilts = numel(sweepTilts);
    
    trials = fullfact([nResponseCues nCueValidities nTargetAxes nSweepDirs nSweepTilts]);
    sweepTiltsPractice = vertcat(sweepTilts(1)*targetAxes, sweepTilts(2)*targetAxes);

    nTrials = length(trials);
    % Randomly assort the first half of trials with the easier sweepTilt,
    % then the second half afterward with the harder sweepTilt
    trialOrder = horzcat(randperm((size(trials,1))/2), (randperm((size(trials,1))/2) + 20));
end

% Slowed down, neutral-only trials
if p.staircasing == 3 || p.staircasing == 4
    responseCues = [1 2]; %which target is responded to, AKA targets
    cueValidities = [0]; % all neutral
    % 5 logarithmically-spaced target axis frequencies in a conservative
    % "safe" range, starting with 800 and ending with 2400 Hz
    targetAxes = logspace(log10(800), log10(2400), 5);
    targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 5)
    targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 2)
    sweepDirs = {'up','down'};
    % sweepTilts = targetAxes*0.016; % change to proportion? .125
    sweepTilts = targetAxes*0.350; %0.600;%0.300; 
    %% maybe 330? ^
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    
    %generate all condition combinations
    trials = fullfact([nResponseCues nCueValidities nTargetAxes nTargetAxes nSweepDirs nSweepDirs]);
    nTrials = p.practiceTrials;

    %trial Order
    trialOrder = randperm(size(trials,1));

    %% testing longer SOA
    if p.staircasing == 3
    p.SOADur = 1.000;
    end
end

% Practice with staircased threshold
if p.staircasing == 6 || p.staircasing == 7
    responseCues = [1 2]; %which target is responded to, AKA targets
    if p.staircasing == 6
        cueValidities = [1]; % all valid
    end
    if p.staircasing == 7
        cueValidities = [1 1 1 -1 0]; % 60 valid, 20 invalid, 20 neutral
    end
    % 5 logarithmically-spaced target axis frequencies in a conservative
    % "safe" range, starting with 800 and ending with 2400 Hz
    targetAxes = logspace(log10(800), log10(2400), 5);
    targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 5)
   targetAxes = logspace(log10(targetAxes(2)), log10(targetAxes(4)), 2)
    sweepDirs = {'up','down'};
    
    % Find the most recently saved staircase session
    staircaseDir = fullfile(dataDir, 'staircasing');
    
    staircaseFiles = dir(fullfile(staircaseDir, sprintf( ...
        '%s_staircasing_run*_ta-auditory_*.mat', subjectID)));
    
    if isempty(staircaseFiles)
        error('No staircase data file found for subject %s.', subjectID)
    end
    
    [~, newestStaircaseIdx] = max([staircaseFiles.datenum]);
    
    thresholdFile = fullfile( ...
        staircaseDir, staircaseFiles(newestStaircaseIdx).name);
    expt_staircase = load(thresholdFile, 'expt');
    subjectThresholdMean = expt_staircase.expt.threshold_avg;
    sweepTilts = targetAxes*subjectThresholdMean;
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    
    %generate all condition combinations
    trials = fullfact([nResponseCues nCueValidities nTargetAxes nTargetAxes nSweepDirs nSweepDirs]);
    nTrials = p.practiceTrials;

    %trial Order
    trialOrder = randperm(size(trials,1));
    trialOrder = trialOrder(1:nTrials);
end


if p.staircasing == 8
    % for the Psychometric Function Self-Piloting JW:
    responseCues = [1]; % cue only the one target T1
    cueValidities = [0 0 0]; % all neutral cues
    targetAxes = logspace(log10(800), log10(2400), 5);
    sweepDirs = {'up','down'};
    sweepTilts = targetAxes*p.psychometricSlope;
    
    % conditions / factors - we want them counterbalanced AKA all combinations of all
    % things, with equal amounts of each combination (pseudorandom)
    nResponseCues = numel(responseCues);
    nCueValidities = numel(cueValidities);
    nTargetAxes = numel(targetAxes);
    nSweepDirs = numel(sweepDirs);
    
    trials = fullfact([nResponseCues nCueValidities nTargetAxes nSweepDirs]);
    trials = vertcat(trials, trials, trials, trials, trials); % 30 x 5 = 150
    nTrials = length(trials);

    %trial Order
    trialOrder = randperm(size(trials,1));
end

%% Validate and preserve the original presentation schedule
if numel(trialOrder) ~= nTrials
     error(['trialOrder contains %d entries, but nTrials is %d. ' ...
        'These must match before the trial loop begins.'], ...
        numel(trialOrder), nTrials);
end

if mod(nTrials, 2) ~= 0
    error('nTrials must be even for the halfway-break logic.')
end

plannedTrialOrder = trialOrder;
   
%% Generate trials data (from rd_cupcakeAperture)
%Construct trials matrix
trials_headers = {'responseCue','cueValidity','targetAxisT1',...
    'targetAxisT2','sweepDirT1','sweepDirT2','attendCue','responseKey',...
    'responseDir','confidenceLevel','correct','rt','sweepTilt',...
    'currentStaircase','stairIdx','rtResponseCue','rtResponseAllowed',...
    'abortedTrial','originalTrial'};

abortedTrialIdx = strcmp(trials_headers,'abortedTrial');
originalTrialIdx = strcmp(trials_headers,'originalTrial');

% make sure column indices match trials headers
responseCueIdx = strcmp(trials_headers,'responseCue');
cueValidityIdx = strcmp(trials_headers,'cueValidity');
targetAxisT1Idx = strcmp(trials_headers,'targetAxisT1');
targetAxisT2Idx = strcmp(trials_headers,'targetAxisT2');
sweepDirT1Idx = strcmp(trials_headers,'sweepDirT1');
sweepDirT2Idx = strcmp(trials_headers,'sweepDirT2');
attendCueIdx = strcmp(trials_headers,'attendCue');
responseKeyIdx = strcmp(trials_headers,'responseKey');
responseDirIdx = strcmp(trials_headers,'responseDir');
% confidenceLevel:
% 1 = high confidence
% 0 = low confidence
% NaN = no valid response, such as an aborted presentation
confidenceLevelIdx = strcmp(trials_headers,'confidenceLevel');
correctIdx = strcmp(trials_headers,'correct');
rtIdx = strcmp(trials_headers,'rt');
sweepTiltIdx = strcmp(trials_headers,'sweepTilt');
currentStaircaseIdx = strcmp(trials_headers, 'currentStaircase');
stairIdxIdx = strcmp(trials_headers, 'stairIdx');
rtResponseCueIdx = strcmp(trials_headers,'rtResponseCue');
rtResponseAllowedIdx = strcmp( ...
    trials_headers, 'rtResponseAllowed');

% Each value identifies the source row in trials
originalTrialOrder = trialOrder(:);

% Canonical trial format matching trials_headers
trialTemplate = NaN(size(trials,1), numel(trials_headers));

trialTemplate(:,responseCueIdx)  = trials(:,1);
trialTemplate(:,cueValidityIdx)  = trials(:,2);
trialTemplate(:,targetAxisT1Idx) = trials(:,3);

if p.staircasing == 2
    % One-target practice:
    % column 4 = sweep direction, column 5 = tilt level
    trialTemplate(:,sweepDirT1Idx) = trials(:,4);
    trialTemplate(:,sweepTiltIdx) = sweepTilts(trials(:,5));

elseif p.staircasing == 8
    % One-target psychometric:
    % column 4 = sweep direction
    trialTemplate(:,sweepDirT1Idx) = trials(:,4);
    trialTemplate(:,sweepTiltIdx) = p.psychometricSlope;

else
    % Standard two-target trial format
    trialTemplate(:,targetAxisT2Idx) = trials(:,4);
    trialTemplate(:,sweepDirT1Idx)   = trials(:,5);
    trialTemplate(:,sweepDirT2Idx)   = trials(:,6);
end

trialTemplate(:,abortedTrialIdx) = 0;
trialTemplate(:,originalTrialIdx) = (1:size(trials,1))';

trialResults = trialTemplate(trialOrder,:);


%% Determine the instructions, based on whichExperiment
if p.staircasing == 0
    instructions = 'This is the main experiment\n\n';
elseif p.staircasing == 1
    instructions = 'This is the staircasing\n\n';
elseif p.staircasing == 2
    instructions = 'You should not be seeing this Message. Juneau messed up! Ask Juneau to restart the code.';
elseif p.staircasing == 3
    instructions = 'This is a practice session\n\nThe events are slowed down to gradually introduce you to the procedure!';
elseif p.staircasing == 4
    instructions = 'This is a practice session\n\nThe events occur in real-time to introduce you to the timing of the procedure!';
elseif p.staircasing == 5
    instructions = 'You should not be seeing this Message. Juneau messed up! Ask Juneau to restart the code.';
elseif p.staircasing == 6
    instructions = 'This is a practice session\n\nBefore the tones happen, you will have a cue\n\nIf the inner ring lights up, pay close attention to the first tone\n\nIf the outer ring lights up, pay close attention to the second tone';
elseif p.staircasing == 7
    instructions = 'This is a practice session\n\nBefore the tones happen, you will have a cue\n\nHOWEVER, the cue might not always match the requested response\n\nIf the inner ring lights up, pay close attention to the first tone\n\nIf the outer ring lights up, pay close attention to the second tone\n\nIf both rings light up, pay close attention to both tones';
elseif p.staircasing == 8
    instructions = 'This is the psychometric function. You should not be seeing this Message. Juneau messed up! Ask Juneau to restart the code.';
end

%% Show instruction screen and wait for a button press
Screen('FillRect', window, white*p.backgroundColor);
%instructionsFull = sprintf('%s\n\nThere will be two tones followed by a white ring. Your goal is to determine whether one of the tones sweeps up (press 1), or sweeps down (press 2)\n\nIf the inner white ring lights up, respond to the first tone.\n\nIf the outer white ring lights up, respond to the second tone.\n\nYou will always get feedback after each sequence of events.\n\nFeel free to ask any questions now, or press any button to continue to the task!', instructions);
%{
instructionsFull = sprintf(['%s\n\nThere will be two tones followed by a white ring. Your goal is to report whether the cued tone swept up or swept down.\n\n' ...
    'Use 1, 2, or 3 if the tone swept up.\n' ...
    'Use 8, 9, or 0 if the tone swept down.\n\n' ...
    '1 or 0 = I heard the slope.\n' ...
    '2 or 9 = I heard the sound, but not the slope.\n' ...
    '3 or 8 = I did not hear the sound.\n\n' ...
    'If the inner white ring lights up, respond to the first tone.\n\n' ...
    'If the outer white ring lights up, respond to the second tone.\n\n' ...
    'You will always get feedback after each sequence of events.\n\n' ...
    'Feel free to ask any questions now, or press any button to continue to the task!'], instructions);

instructionsFull = sprintf([ ...
    '%s\n\n' ...
    'There will be two tones.\n\n' ...
    'Before the tones, a number will appear at fixation:\n' ...
    '1 means pay attention to the first tone.\n' ...
    '2 means pay attention to the second tone.\n' ...
    '12 means pay attention to both tones.\n\n' ...
    'After the tones, a 1 or 2 will appear at fixation. ' ...
    'This tells you which tone to report.\n\n' ...
    'Use 1, 2, or 3 if the requested tone swept up.\n' ...
    'Use 8, 9, or 0 if the requested tone swept down.\n\n' ...
    '1 or 0 = I heard the slope.\n' ...
    '2 or 9 = I heard the sound, but not the slope.\n' ...
    '3 or 8 = I did not hear the sound.\n\n' ...
    'You will receive feedback after each trial.\n\n' ...
    'Feel free to ask any questions now, or press any key ' ...
    'to continue to the task!'], ...
    instructions);
%}

%% Cue-specific instructions
if strcmp(p.ringOrNum, 'ring')
    cueInstructions = [ ...
        'Before the tones:\n' ...
        'The inner ring means pay attention to the first tone.\n' ...
        'The outer ring means pay attention to the second tone.\n' ...
        'Both rings means pay attention to both tones.\n\n' ...
        'After the tones:\n' ...
        'The inner ring means report the first tone.\n' ...
        'The outer ring means report the second tone.'];

elseif strcmp(p.ringOrNum, 'number')
    cueInstructions = [ ...
        'Before the tones:\n' ...
        '1 means pay attention to the first tone.\n' ...
        '2 means pay attention to the second tone.\n' ...
        '12 means pay attention to both tones.\n\n' ...
        'After the tones:\n' ...
        '1 means report the first tone.\n' ...
        '2 means report the second tone.'];
end

instructionsFull = sprintf([ ...
    '%s\n\n' ...
    'There will be two tones. Your goal is to report whether the ' ...
    'requested tone swept up or swept down.\n\n' ...
    '%s\n\n' ...
    'Use 1 or 2 if the tone swept up.\n' ...
    'Use 9 or 0 if the tone swept down.\n\n' ...
    '1 or 0 = high confidence.\n' ...
    '2 or 9 = low confidence.\n\n' ...
    'You will always receive feedback after each trial.\n\n' ...
    'Feel free to ask any questions now, or press any key ' ...
    'to continue to the task!'], ...
    instructions, cueInstructions);
DrawFormattedText(window, instructionsFull, 'center', 'center', [1 1 1]*white);
Screen('Flip', window); %command to change what's on the screen. we're drawing on a behind (hidden) window, and at the moment we screenflip, we flip that window to the front position
KbWait(devNum);  
timeStart = GetSecs;

%% Testing DrawFixation
if strcmp(p.ringOrNum, 'ring')
    graydark  = [0.4,0.4,0.4]*white;

elseif strcmp(p.ringOrNum, 'number')
    graydark  = [0.5,0.5,0.5]*white;
end
graylight = [1, 1, 1]*white;
grayDelay = [0.4, 0.4, 0.4]*white;
    % colorFixation = [graylight; graylight; graylight];
colorFixation = [graydark; graydark; graylight];
% Dim-center version of the ordinary fixation
colorDimFixation = colorFixation;
colorDimFixation(3,:) = grayDelay;
sizeFixation = [outerRingSize; innerRingSize; fixSize]; % experiment with center fixation dot only
sizeLength = sizeFixation(3)/3;

% Number displayed inside fixation cues
% Scale "2" and "12" so their estimated illuminated areas match "1"
if strcmp(p.ringOrNum, 'number')

    baseNumberTextSize = round(fixSize * 0.70);

    numberPixelCounts = [43, 57, 100];  % Original "1", "2", "12"
    numberSizeScale = sqrt(numberPixelCounts(1) ./ numberPixelCounts);

    % Index 1 = "1", index 2 = "2", index 3 = "12"
    numberTextSizes = round(baseNumberTextSize .* numberSizeScale);


    %% TEST THE ACTUAL ILLUMINATED PIXELS AT THE NEW SIZES

    cueStrings = {'1', '2', '12'};

    measureRect = [0 0 400 200];

    measureWindow = Screen('OpenOffscreenWindow', ...
        window, graydark, measureRect);

    % Match the font and style used by the experiment window
    Screen('TextFont', measureWindow, Screen('TextFont', window));
    Screen('TextStyle', measureWindow, Screen('TextStyle', window));

    for iCue = 1:numel(cueStrings)

        % Use the newly calculated size for this cue
        Screen('TextSize', measureWindow, numberTextSizes(iCue));

        % Start with a clean background
        Screen('FillRect', measureWindow, graydark);
        backgroundImage = Screen('GetImage', measureWindow);

        % Draw the cue
        DrawFormattedText(measureWindow, cueStrings{iCue}, ...
            'center', 'center', white);

        renderedImage = Screen('GetImage', measureWindow);

        % Count pixels whose RGB value differs from the background
        litPixelMask = any( ...
            renderedImage(:, :, 1:3) ~= ...
            backgroundImage(:, :, 1:3), ...
            3);

        actualPixelCounts(iCue) = nnz(litPixelMask);

        fprintf(['Cue "%s": text size = %d, ' ...
            'illuminated glyph pixels = %d\n'], ...
            cueStrings{iCue}, ...
            numberTextSizes(iCue), ...
            actualPixelCounts(iCue));
    end

    Screen('Close', measureWindow);

end

%{
% Red filled inner circle, blue filled outer, with 20-width green strokes
drawFixation(window, cx, cy, [200; 250], [255, 0, 0; 0, 0, 255], 20, [0, 255, 0]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% Red filled inner circle, blue filled outer, with 5-width gray strokes
drawFixation(window, cx, cy, [2*fixSize; 3*fixSize], [255, 0, 0; 0, 0, 255], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation
jw_drawFixation(window, cx, cy, 3, [3*fixSize; 2*fixSize; 1*fixSize], [255, 0, 0; 0, 0, 255; 0, 255, 0], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation, smaller and tighter ring
jw_drawFixation(window, cx, cy, 3, [2.5*fixSize; 1.75*fixSize; 1*fixSize], [255, 0, 0; 0, 0, 255; 0, 255, 0], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation, fixation
jw_drawFixation(window, cx, cy, 3, [2.5*fixSize; 1.75*fixSize; 1*fixSize], [graylight; graylight; graylight], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation, neutral cue
jw_drawFixation(window, cx, cy, 3, [2.5*fixSize; 1.75*fixSize; 1*fixSize], [graydark; graydark; graydark], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation, T1
jw_drawFixation(window, cx, cy, 3, [2.5*fixSize; 1.75*fixSize; 1*fixSize], [graylight; graydark; graydark], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);

% test jw_drawFixation, T2
jw_drawFixation(window, cx, cy, 3, [2.5*fixSize; 1.75*fixSize; 1*fixSize], [graydark; graylight; graydark], 10, white*[.4, .4, .4]);
Screen('Flip', window);
WaitSecs(1);
KbWait(devNum);
%}
% create the original white fixation, with jw_drawFixation

%% Generate the nonvariable sound samples for the stimulus (ITI, SOA)
% Parameters
% p.sweepDur = 0.030; % duration of each sweep, seconds
t = 0:1/p.Fs:p.sweepDur - 1/p.Fs; % time vector
% p.rampDur = 0.005; % duration of onset/offset ramps 
rampDurSamples = 0:1/p.Fs:p.rampDur - 1/p.Fs;
rampDurSamples = length(rampDurSamples); % number of samples for onset/offset ramps
% p.SOADur = 0.250; % duration of SOA, seconds
	SOASamples = 0:1/p.Fs:p.SOADur - 1/p.Fs;
SOASamples(:) = 0; % Create a silent vector for SOA
SOASamples(2,:) = 0;
% p.rms1Lev = 100; % Norm. value for RMS of 1, arbitrary
% tLev = 60; % Target level, "dB SPL", arbitrary
% tLev = 90
% tScale = 10.^((p.tLev-p.rms1Lev)./20); % Scale factor for target level setting
% method = 'linear'; % sweep type, linear so duration of sweep before and after CF is equivalent
% method = 'logarithmic';
phi = unifrnd(0,2*pi); % starting phase of swept tone, random

%ISI Onset and Offset Samples
% ISIDur = 1.00;
ISISamples = 0:1/p.Fs:p.ISIDur - 1/p.Fs;
ISISamples(1:2,:) = 0;
% ISIOffsetDur = 0.200;
ISIOffsetSamples = 0:1/p.Fs:p.ISIOffsetDur - 1/p.Fs;
ISIOffsetSamples(1:2,:) = 0;

%% Generate the click noise (from jw_soundClick)
% "The Fourier transform of a square wave pulse is sin(x)/x"
% Source: https://www.mathworks.com/matlabcentral/answers/1612350-how-to-program-a-click-stimulus?s_tid=prof_contriblnk
% smallest it can  be on Linux and Sennheiser is 0.04644
tclick = linspace(0, p.clickDur*p.Fs, p.clickDur*p.Fs)/p.Fs;     % generate time vector (s)
xValue = 2*pi*(tclick-p.clickRampDur)*p.clickFreq;               % x value based on the Fourier transform of a square wave, sin(x)/x
s = sin(xValue)./(xValue);                              % generate the click

clickRampDurSamples = 0:1/p.Fs:p.clickRampDur - 1/p.Fs;
clickRampDurSamples = length(clickRampDurSamples); % number of samples for onset/offset ramps
s = CosineSquaredRamp(s,clickRampDurSamples);
s = s-mean(s); % Next three lines are setting the overall level
s = s./rms(s);
s = s.*p.tScale;
s = repmat(s,[2,1,1]);

clickOffsetDur = round( ...
    (0.200 - p.clickDur) * p.Fs);

%% 
%% Loops

nTrialsOriginal = nTrials;
iTrial = 1;
halfBoundary = nTrialsOriginal / 2;
halfwayBreakShown = false;
driftCorrectedByTrial = false(1,nTrials);

% Show "Fixation break!" at the beginning of the presentation
% immediately following an aborted presentation
showFixationBreakWarning = false;

while iTrial <= nTrials
if ~halfwayBreakShown && iTrial == halfBoundary + 1
    praise = jw_highpraise;

    completedTrialsAtBreak = nTrialsOriginal / 2;
    remainingTrialsAtBreak = nTrialsOriginal - completedTrialsAtBreak;

    takeABreak = sprintf([ ...
        '%s You have completed %d trials!\n\n' ...
        'You have %d trials remaining.\n\n' ...
        'Please feel free to take a short break.\n\n' ...
        'Press any key to continue.'], ...
        praise, completedTrialsAtBreak, remainingTrialsAtBreak);

    Screen('FillRect', window, white*p.backgroundColor);
    DrawFormattedText(window, takeABreak, ...
        'center', 'center', [1 1 1]*white);
    Screen('Flip', window);

    KbPressWait;
    halfwayBreakShown = true;
end
if p.staircasing == 0 || p.staircasing == 6 || p.staircasing == 7
    %pick trial index
    trialIdx = trialOrder(iTrial);

    %% pick values of all conditions on current trial
    responseCue = trials(trialIdx, responseCueIdx); % takes row from indexed ('random') order, column for respective variable 
    cueValidity = trials(trialIdx, cueValidityIdx);
    targetAxisT1 = trials(trialIdx, targetAxisT1Idx);
    targetAxisT2 = trials(trialIdx, targetAxisT2Idx);
    sweepDirT1 = trials(trialIdx, sweepDirT1Idx);
    sweepDirT2 = trials(trialIdx, sweepDirT2Idx);
    
    %% generate which cue for attending (attendCue) based on cueValidity and responseCue
    % uses 1 (attend T1), 2 (attend T2), and 3 (attend both)
    if cueValidity == 4 % if using cueValidities -1 (invalid), then cue (1 or 2) opposite of responseCue
        if responseCue == 1
            attendCue = 2;
        elseif responseCue == 2
            attendCue = 1;
        end
    elseif cueValidity == 5 % if using cueValidities 0 (neutral), then cue both (3) regardless of responseCue
        attendCue = 3;
    else % if using cueValidities 1 (valid), then cue (1 or 2) respective to responseCue
        attendCue = responseCue;
    end
    %% note to self... can I make the above a lookup table? if so, would it make it more difficult to modify in the future (e.g. having to edit a 2x5 matrix rather than 3 numbers)?
end
if p.staircasing == 1

    if currentStaircase == 1
        stairIdx = stairIdx_s1;
    elseif currentStaircase == 2
        stairIdx = stairIdx_s2;
    end
    sweepTilts = targetAxes*p.stairs(stairIdx);

    %pick trial index
    trialIdx = trialOrder(iTrial);

    % pick values of all conditions on current trial
    responseCue = trials(trialIdx, 1); % takes row from indexed ('random') order, column for respective variable 
    cueValidity = trials(trialIdx, 2);
    targetAxisT1 = trials(trialIdx, 3);
    targetAxisT2 = trials(trialIdx, 4);
    sweepDirT1 = trials(trialIdx, 5);
    sweepDirT2 = trials(trialIdx, 6);

    attendCue = 3; % all neutral for staircasing
end

if p.staircasing == 2 || p.staircasing == 8
    %pick trial index
    trialIdx = trialOrder(iTrial);
    % pick values of all conditions on current trial
    responseCue = trials(trialIdx, 1); % takes row from indexed ('random') order, column for respective variable 
    cueValidity = trials(trialIdx, 2);
    targetAxisT1 = trials(trialIdx, 3);
    sweepDirT1 = trials(trialIdx, 4);
    %% sweepTilts changes for practice!
    if p.staircasing == 2
        sweepTilts = sweepTiltsPractice(trials(trialIdx, 5),:);
    end

    attendCue = 1; % attend T1 for practice
    responseCue = 1; % respond T1 for practice
end

if p.staircasing == 3 || p.staircasing == 4
    trialIdx = trialOrder(iTrial);

    %% pick values of all conditions on current trial
    responseCue = trials(trialIdx, responseCueIdx); % takes row from indexed ('random') order, column for respective variable 
    cueValidity = trials(trialIdx, cueValidityIdx);
    targetAxisT1 = trials(trialIdx, targetAxisT1Idx);
    targetAxisT2 = trials(trialIdx, targetAxisT2Idx);
    sweepDirT1 = trials(trialIdx, sweepDirT1Idx);
    sweepDirT2 = trials(trialIdx, sweepDirT2Idx);

    attendCue = 3; % all neutral for practice
end

%%
%% Record the proportional sweep tilt used on this presentation
% sweepTilts contains absolute Hz deviations for each target axis.
% Dividing by the corresponding axis recovers the proportional tilt.
trialSweepTilt = ...
    sweepTilts(targetAxisT1) / targetAxes(targetAxisT1);
%% Define ring and number versions of the cues

% Set the normal center-fixation color first
if p.graydarkFixation
    colorFixation(3,:) = graydark;
    numberColor = graylight;
else
    numberColor = graydark;
end

% Start ring cues from ordinary fixation so the center dot never changes
colorsCue = colorFixation;
colorsResponse = colorFixation;

%% Attention precue
if attendCue == 1
    % T1: inner ring or number 1
    textCue = '1';
    colorsCue(2,:) = graylight;

elseif attendCue == 2
    % T2: outer ring or number 2
    textCue = '2';
    colorsCue(1,:) = graylight;

elseif attendCue == 3
    % Neutral: both rings or number 12
    textCue = '12';
    colorsCue(1:2,:) = repmat(graylight, 2, 1);

else
    error('Unexpected attendCue value: %d', attendCue)
end

%% Response cue
if responseCue == 1
    % Report T1: inner ring or number 1
    textResponse = '1';
    colorsResponse(2,:) = graylight;

elseif responseCue == 2
    % Report T2: outer ring or number 2
    textResponse = '2';
    colorsResponse(1,:) = graylight;

else
    error('Unexpected responseCue value: %d', responseCue)
end

%% note to self... can I make the above a lookup table? if so, would it make it more difficult to modify in the future (e.g. having to edit a 2x5 matrix rather than 3 numbers)?

%% Generate T1 and T2
    % Generate T1 and T2 
if p.staircasing ~= 2 && p.staircasing ~= 8
    Targets = zeros(2,length(t),2);
    for target = 1:2
        if target == 1
           targetAxis = targetAxisT1;
           sweepDir = sweepDirT1;
        elseif target == 2
           targetAxis = targetAxisT2;
           sweepDir = sweepDirT2;
        end
        if sweepDir == 1
            freqStart = targetAxes(targetAxis) - sweepTilts(targetAxis); % start frequency
            freqEnd = targetAxes(targetAxis) + sweepTilts(targetAxis); % end frequency
        elseif sweepDir == 2
        	freqStart = targetAxes(targetAxis) + sweepTilts(targetAxis); % start frequency
            freqEnd = targetAxes(targetAxis) - sweepTilts(targetAxis); % end frequency 
        end
        y = chirp(t,freqStart,p.sweepDur,freqEnd,p.method,phi);
        y = CosineSquaredRamp(y,rampDurSamples);
        y = y-mean(y); % Next three lines are setting the overall level
        y = y./rms(y);
        y = y.*p.tScale;
        if p.earStim
            Targets(randi([1 2]),:,target) = y;
        else
            %Targets(1,:,target) = y;
            Targets(1:2,:,target) = repmat(y,[2,1,1]);
            %Targets(2,:,target) = y;
        end
    end
end

if p.staircasing == 2 || p.staircasing == 8
    Targets = zeros(2,length(t),2);
    for target = 1%:2
        if target == 1
           targetAxis = targetAxisT1;
           sweepDir = sweepDirT1;
        %elseif target == 2
           %targetAxis = targetAxisT2;
           %sweepDir = sweepDirT2;
        end
        if sweepDir == 1
            freqStart = targetAxes(targetAxis) - sweepTilts(targetAxis); % start frequency
            freqEnd = targetAxes(targetAxis) + sweepTilts(targetAxis); % end frequency
        elseif sweepDir == 2
        	freqStart = targetAxes(targetAxis) + sweepTilts(targetAxis); % start frequency
            freqEnd = targetAxes(targetAxis) - sweepTilts(targetAxis); % end frequency 
        end
        y = chirp(t,freqStart,p.sweepDur,freqEnd,p.method,phi);
        y = CosineSquaredRamp(y,rampDurSamples);
        y = y-mean(y); % Next three lines are setting the overall level
        y = y./rms(y);
        y = y.*p.tScale;
        Targets(1,:,target) = y;
        %Targets(2,:,target) = y;
    end
end

earDist = p.earDist;
if p.distractor
    ISISamplesDist = jw_addDistractors(ISISamples, earDist);
    SOASamplesDist = jw_addDistractors(SOASamples, earDist);
    ISIOffsetSamplesDist = jw_addDistractors(ISIOffsetSamples, earDist);
    trialStim = horzcat(ISISamplesDist(:,:),Targets(:,:,1),SOASamplesDist(:,:),Targets(:,:,2),ISIOffsetSamplesDist(:,:));
else
    trialStim = horzcat(ISISamples(:,:),Targets(:,:,1),SOASamples(:,:),Targets(:,:,2),ISIOffsetSamples(:,:));
end


  %  trialStim = horzcat(ISISamples(:,:),Targets(:,:,1),SOASamples(:,:),Targets(:,:,2),ISIOffsetSamples(:,:));
    %trialStim = trialStim(1,:);
    %% Address this
    %trialStim = horzcat(s(:,:),zeros(1,clickOffsetDur),trialStim(:,:),s(:,:),zeros(1,clickOffsetDur));
    trialStim = horzcat(s(:,:),zeros(2,clickOffsetDur),trialStim(:,:),s(:,:),zeros(2,clickOffsetDur));
    %trialStim = repmat(trialStim,[2,1,1]);
    %trialStim = trialStim.';
    %% 
    PsychPortAudio('FillBuffer', pahandle, trialStim);
%% Draw fixation and wait until EyeLink is ready
jw_drawFixation(window, cx, cy, 3, ...
    sizeFixation, colorFixation, sizeLength);

% If fixation broke on the immediately preceding presentation,
% show the warning while waiting for good fixation
if p.eyetracking && showFixationBreakWarning
    Screen('TextSize', window, p.fixBreakTextSize);

    DrawFormattedText(window, 'Fixation break!', ...
        'center', cy - fixBreakTextOffsetPx, ...
        [1 1 1]*white);
end

timeFix = Screen('Flip', window);

driftCorrected = 0;

if p.eyetracking
    % This returns only after recording is active and good fixation
    % has been held. It reports whether drift correction occurred.
    driftCorrected = rd_eyeLink('trialstart', window, ...
        {el, iTrial, cx, cy, eyetrackSize});
    driftCorrectedByTrial(iTrial) = logical(driftCorrected);
    % Preserve the correspondence between EDF presentations and MAT rows,
    % including trials that were aborted and appended to the end.
    Eyelink('Message', ...
        'TRIAL_META PRESENTATION %d ORIGINAL %d TRIALIDX %d', ...
        iTrial, originalTrialOrder(iTrial), trialIdx);

    % Drift correction/setup may have replaced the display.
    % Restore normal fixation and begin the full pre-cue buffer here.
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorFixation, sizeLength);
    timeFix = Screen('Flip', window);

    % Good fixation has now been achieved. The warning was only for
    % the current presentation, so do not show it again automatically.
    showFixationBreakWarning = false;

    Eyelink('Message', 'EVENT_FIXATION_BUFFER_ONSET');

    if driftCorrected
        Eyelink('Message', 'POST_DRIFT_FIXATION_BUFFER');
    end
end

    %% generate the cue itself
%% Draw attention precue into the back buffer
if strcmp(p.ringOrNum, 'ring')
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorsCue, sizeLength);

elseif strcmp(p.ringOrNum, 'number')
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorFixation, sizeLength);

    % attendCue 1, 2, or 3 corresponds to "1", "2", or "12"
    Screen('TextSize', window, numberTextSizes(attendCue));

    DrawFormattedText(window, textCue, ...
        'center', 'center', numberColor);
end

stopThisTrial = 0;
fixation = 1;

% One flip controls onset for either cue type
timeCueOnset = Screen('Flip', window, ...
    timeFix + p.preCueFixDur - slack);
    if p.eyetracking
        Eyelink('Message', ...
            'EVENT_PRECUE_ONSET FORMAT %s CUE %d', ...
            upper(p.ringOrNum), attendCue);
    end
    %%%% sound(s, p.Fs); % sound the click!
    %sound(trialStim, p.Fs);
    %% Put this above, right after timeFix, with timeCueOnset dependent on timeTrial + 200 ms or 500 ms, <1 s
    %and/or play a dummy sound before the very first click
    %maybe when PTB is ready, make a nice boop and flips dummy screen
    timeTrial = PsychPortAudio('Start', pahandle, [], [], 1);

    if p.eyetracking
        % Marks the initial click/audio-buffer onset, not T1 onset
        Eyelink('Message', 'EVENT_AUDIO_START');
    end
    %% What about adding blank buffers? Think abt this more

    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
    if p.eyetracking
        while GetSecs < timeCueOnset + 0.200 - flipInterval && ~stopThisTrial
            WaitSecs(0.01);
            fixation = rd_eyeLink('fixcheck', window, {cx, cy, eyetrackSize});
            if ~fixation
                stopThisTrial = 1;
            end
        end
    end
    timeCueOffset = Screen('Flip', window, timeCueOnset + 0.200 - slack);
    if p.eyetracking
        Eyelink('Message', 'EVENT_PRECUE_OFFSET');
    end
    
    if p.eyetracking
        fixation = rd_eyeLink('fixcheck', window, {cx, cy, eyetrackSize});
        if ~fixation
            stopThisTrial = 1;
        end
    end
    %do the trial
    %% translating the kahuna step by step
    
    %trialStim = horzcat(Targets(:,:,1),SOASamples(:,:),Targets(:,:,2),ISIOffsetSamples(:,:));
    %trialStim = trialStim(:)*[1 1];
    %timeStim = GetSecs; % just for testing rt save data
    %timeStim = sound(trialStim, p.Fs); % how to fix this?
    %%%%sound(trialStim, p.Fs);
%% 20230919 visual targetBlips
if p.targetBlips
    %colorTargetBlips = [graydark; graydark; graylight*0.7];
    colorTargetBlips = [graydark; graydark; graydark];
    if p.graydarkFixation
        colorTargetBlips(3,:) = graylight;
    end
    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorTargetBlips, sizeLength);
    timeT1Onset = Screen('Flip', window, timeCueOffset + p.ISIDur - slack);
    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
    timeT1Offset = Screen('Flip', window, timeT1Onset + p.sweepDur - slack);
    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorTargetBlips, sizeLength);
    timeT2Onset = Screen('Flip', window, timeT1Offset + p.SOADur - slack);
    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
    timeT2Offset = Screen('Flip', window, timeT2Onset + p.sweepDur - slack);
    WaitSecs(p.ISIOffsetDur);
else
    tEndFixCheck = GetSecs + p.ISIDur+p.sweepDur+p.SOADur+p.sweepDur+p.ISIOffsetDur;
    while GetSecs < tEndFixCheck && ~stopThisTrial
        WaitSecs(0.01);
        if p.eyetracking
            fixation = rd_eyeLink('fixcheck', window, {cx, cy, eyetrackSize});
            if ~fixation
                stopThisTrial = 1;
            end
        end
    end
end

%% collect response

% Final fixation check immediately before response-cue onset
if p.eyetracking && ~stopThisTrial
    fixation = rd_eyeLink('fixcheck', window, {cx, cy, eyetrackSize});
    if ~fixation
        stopThisTrial = 1;
    end
end

% Show the response cue only if fixation has remained valid so far
if ~stopThisTrial
%% Draw response cue into the back buffer
if strcmp(p.ringOrNum, 'ring')
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorsResponse, sizeLength);

elseif strcmp(p.ringOrNum, 'number')
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorFixation, sizeLength);

    % responseCue is either 1 or 2
    Screen('TextSize', window, numberTextSizes(responseCue));

    DrawFormattedText(window, textResponse, ...
        'center', 'center', numberColor);
end

% One flip controls onset for either cue type
timeResponseOnset = Screen('Flip', window);
    %jw_drawFixation(window, cx, cy, 3, sizeFixation, colorsResponse, sizeLength);
    %timeResponseOnset = Screen('Flip', window);

    if p.eyetracking
        Eyelink('Message', ...
            'EVENT_RESPONSE_CUE_ONSET FORMAT %s CUE %d', ...
            upper(p.ringOrNum), responseCue);
    end

    % Prepare the fixation shown after the response cue disappears
    if strcmp(p.grayWhen, 'delay')
        colorAfterResponseCue = colorDimFixation;
    else
        colorAfterResponseCue = colorFixation;
    end
    
    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorAfterResponseCue, sizeLength);

    % Draw fixation alone so the number disappears at cue offset
    %jw_drawFixation(window, cx, cy, 3, ...
      %  sizeFixation, colorFixation, sizeLength);

    % Require fixation throughout the 200-ms response cue
    if p.eyetracking
        while GetSecs < timeResponseOnset + 0.200 - flipInterval && ...
                ~stopThisTrial
            WaitSecs(0.01);

            fixation = rd_eyeLink('fixcheck', window, ...
                {cx, cy, eyetrackSize});

            if ~fixation
                stopThisTrial = 1;
            end
        end
    end

    % Response-cue offset: fixation enforcement ends here
    timeDelayOnset = Screen('Flip', window, ...
        timeResponseOnset + 0.200 - slack);

    if p.eyetracking && ~stopThisTrial
        fixation = rd_eyeLink('fixcheck', window, ...
            {cx, cy, eyetrackSize});

        if ~fixation
            stopThisTrial = 1;
        end
    end

    if p.eyetracking
        Eyelink('Message', 'EVENT_RESPONSE_CUE_OFFSET');
    end
end

% Abort and requeue any trial with a fixation break
if stopThisTrial
    PsychPortAudio('Stop', pahandle);

    % The immediately following presentation should display
    % the fixation-break warning
    showFixationBreakWarning = true;

    if p.eyetracking
        Eyelink('Message', 'TRIAL_ABORT_FIXATION_BREAK');
    end

    % Preserve the presented conditions and mark the attempt as aborted
    trialResults(iTrial,attendCueIdx) = attendCue;
    
    % Store sweep tilt for every experiment mode
    trialResults(iTrial,sweepTiltIdx) = trialSweepTilt;
    
    % Staircase-specific information
    if p.staircasing == 1
        trialResults(iTrial,currentStaircaseIdx) = currentStaircase;
        trialResults(iTrial,stairIdxIdx) = stairIdx;
    end
    
    trialResults(iTrial,abortedTrialIdx) = 1;
    trialResults(iTrial,originalTrialIdx) = originalTrialOrder(iTrial);

        %{
    trialOrder(end+1) = trialOrder(iTrial);
    originalTrialOrder(end+1) = originalTrialOrder(iTrial);
    
    trialResults(end+1,:) = NaN(1,size(trialResults,2));
    trialResults(end,1:size(trials,2)) = trials(trialOrder(end),:);
    trialResults(end,abortedTrialIdx) = 0;
    trialResults(end,originalTrialIdx) = originalTrialOrder(end);
    
    nTrials = nTrials + 1;
        %}
    % Construct the retry presentation
    retryTrialOrder = trialOrder(iTrial);
    retryOriginalTrial = originalTrialOrder(iTrial);
    
    retryResult = trialTemplate(retryTrialOrder,:);
    
    if iTrial <= halfBoundary
        % Insert retry at the end of the first half, before half two
        insertIdx = halfBoundary + 1;
    
        trialOrder = [ ...
            trialOrder(1:halfBoundary), ...
            retryTrialOrder, ...
            trialOrder(insertIdx:end)];
    
        originalTrialOrder = [ ...
            originalTrialOrder(1:halfBoundary); ...
            retryOriginalTrial; ...
            originalTrialOrder(insertIdx:end)];
    
        trialResults = [ ...
            trialResults(1:halfBoundary,:); ...
            retryResult; ...
            trialResults(insertIdx:end,:)];
    
        driftCorrectedByTrial = [ ...
            driftCorrectedByTrial(1:halfBoundary), ...
            false, ...
            driftCorrectedByTrial(insertIdx:end)];
    
        halfBoundary = halfBoundary + 1;
    else
        % Second-half retries go to the end of the second half
        trialOrder(end+1) = retryTrialOrder;
        originalTrialOrder(end+1) = retryOriginalTrial;
        trialResults(end+1,:) = retryResult;
        driftCorrectedByTrial(end+1) = false;
    end
    
    nTrials = nTrials + 1;

    jw_drawFixation(window, cx, cy, 3, ...
        sizeFixation, colorFixation, sizeLength);
    Screen('Flip', window);
    WaitSecs(0.500);

    if p.eyetracking
        Eyelink('Message', 'TRIAL_END %d', iTrial);
        rd_eyeLink('stoprecording', window, []);
    end

    iTrial = iTrial + 1;
    continue
end

% Prepare the fixation shown once responses become allowed
if strcmp(p.grayWhen, 'response')
    colorResponseAllowed = colorDimFixation;
else
    colorResponseAllowed = colorFixation;
end

% No fixation requirement after response-cue offset
jw_drawFixation(window, cx, cy, 3, ...
    sizeFixation, colorResponseAllowed, sizeLength);

timeResponseAllowed = Screen('Flip', window, ...
    timeDelayOnset + p.responseDelayDur - 0.200 - slack);
if p.eyetracking
    Eyelink('Message', 'EVENT_RESPONSE_ALLOWED');
end
KbReleaseWait(devNum);

allowedKeys = p.allowedResponseKeys;

validResponse = 0;
while ~validResponse
    [secs, keyCode] = KbWait(devNum);
    responseKey = find(keyCode);

    if numel(responseKey) == 1 && ismember(responseKey, allowedKeys)
        validResponse = 1;
    else
        KbReleaseWait(devNum); % ignore invalid/multiple keys and wait again
    end
end

rt = secs - timeTrial;
rtResponseCue = secs - timeResponseOnset;
rtResponseAllowed = secs - timeResponseAllowed;

    %% Response and store
%trialsHeaders = {'RT','ResponseKey'};
%trialIdx = 1;
%trials(trialIdx, 1) = rt;
%trials(trialIdx, 2) = responseKey;

% responseDir:
% 1 = up
% 2 = down
%
% confidenceLevel:
% 1 = high
% 0 = low

if ismember(responseKey, [p.keyOne p.keyTwo])
    responseDir = 1; % up

elseif ismember(responseKey, [p.keyNine p.keyZero])
    responseDir = 2; % down

else
    % This should be impossible because response collection only
    % accepts p.allowedResponseKeys
    error('Unexpected accepted response key: %d', responseKey)
end

if ismember(responseKey, [p.keyOne p.keyZero])
    confidenceLevel = 1;
    confidenceText = 'HIGH';
else
    confidenceLevel = 0;
    confidenceText = 'LOW';
end

if responseCue == 1
   responseDirCorrect = sweepDirT1;
elseif responseCue == 2
   responseDirCorrect = sweepDirT2;
end

if responseDir == responseDirCorrect
   colorFeedback = [0,1,0]*white;
   %correct = 'correct'
   correct = 1;
else
   colorFeedback = [1,0,0]*white; 
   %correct = 'incorrect' % need to account for failure to answer as
   %well!
   correct = 0;
end

if p.eyetracking
    Eyelink('Message', ...
        ['RESPONSE PRESENTATION %d KEY %d DIR %d ' ...
         'CONFIDENCE %s CORRECT %d'], ...
        iTrial, responseKey, responseDir, ...
        confidenceText, correct);
end
%colorsFeedback = [colorsResponse(1,:); colorsResponse(2,:); colorFeedback];
colorsFeedback = [graydark; graydark; colorFeedback];

% Store trial info
%trials(iTrial,iti2Idx) = iti; % the actual ITI, including extra delay
%ITI code from cupcake, not sure if i need this just yet
trialResults(iTrial,attendCueIdx) = attendCue;
% If people press 2 keys, we would still want a record of that
% cell array to save all responseKey
expt.allResponseKeys{iTrial} = responseKey;
%% look upward and make sure this functions well, esp. with correctness / feedback
% note - do not overwrite variables!
if numel(responseKey) > 1
    trialResults(iTrial,responseKeyIdx) = NaN;
else
    trialResults(iTrial,responseKeyIdx) = responseKey;
end
%% Think about making responseKey a cell array
trialResults(iTrial,responseDirIdx) = responseDir;
trialResults(iTrial,correctIdx) = correct;
trialResults(iTrial,rtIdx) = rt;
trialResults(iTrial,confidenceLevelIdx) = ...
    confidenceLevel;
trialResults(iTrial,rtResponseCueIdx) = rtResponseCue;
trialResults(iTrial,rtResponseAllowedIdx) = ...
    rtResponseAllowed;

% Store the proportional sweep tilt for every experiment mode
trialResults(iTrial,sweepTiltIdx) = trialSweepTilt;

% Store staircase-specific information only during staircasing
if p.staircasing == 1
    trialResults(iTrial,currentStaircaseIdx) = currentStaircase;
    trialResults(iTrial,stairIdxIdx) = stairIdx;
end

% If staircasing, updateStaircase and getReversalValues
if p.staircasing == 1 && currentStaircase == 1
    [stairIdx, lastFewAcc_s1] = updateStaircase(p.stairs, stairIdx, lastFewAcc_s1, correct);
    stairIdx_s1 = stairIdx;
    stairValues(iTrial) = stairIdx;
    completedStairTrials_s1 = completedStairTrials_s1 + 1;
    stairValues_s1(completedStairTrials_s1) = stairIdx;
    reversalValues_s1 = getReversalValues(stairValues_s1);
    % Show the current threshold estimate
    if numel(reversalValues_s1)>=5 %9 %5
        % Average the last 5 reversals to get threshold
        threshold_s1 = mean(p.stairs(reversalValues_s1(end-4:end)));
        fprintf('Stair 1 threshold estimate = %f\n', threshold_s1)
    else
        threshold_s1 = [];
    end
    % Swap to the other staircase for the upcoming trial
    currentStaircase=2;
elseif p.staircasing == 1 && currentStaircase == 2
    [stairIdx, lastFewAcc_s2] = updateStaircase(p.stairs, stairIdx, lastFewAcc_s2, correct);
    stairIdx_s2 = stairIdx;
    stairValues(iTrial) = stairIdx;
    completedStairTrials_s2 = completedStairTrials_s2 + 1;
    stairValues_s2(completedStairTrials_s2) = stairIdx;
    reversalValues_s2 = getReversalValues(stairValues_s2);
    % Show the current threshold estimate
    if numel(reversalValues_s2)>=5 %10 %5
        % Average the last 5 reversals to get threshold
        threshold_s2 = mean(p.stairs(reversalValues_s2(end-4:end)));
        fprintf('Stair 2 threshold estimate = %f\n', threshold_s2)
    else
        threshold_s2 = [];
    end
    % Swap to the other staircase for the upcoming trial
    currentStaircase=1;
end

jw_drawFixation(window, cx, cy, 3, sizeFixation, colorsFeedback, sizeLength);
% drawFixation(window, cx, cy, fixSize, colorFeedback);
Screen('Flip', window);
WaitSecs(0.500);

if p.eyetracking
    Eyelink('Message', 'TRIAL_END %d', iTrial);
    rd_eyeLink('stoprecording', window, []);
end

iTrial = iTrial + 1;
end

%% Additional store expt info
%% Session and design metadata
expt.sessionTime = sessionTime;
expt.sessionTimeString = datestr(sessionTime, 'yyyymmdd_HHMMSS');

expt.expType = expType;
expt.expName = expName;

expt.trials = trials;
expt.targetAxes = targetAxes;
expt.sweepDirs = sweepDirs;
expt.responseCues = responseCues;
expt.cueValidities = cueValidities;

% sweepTilt is stored as a proportion of the target-axis frequency
expt.sweepTiltDefinition = ...
    'Proportional frequency deviation from the target axis';

% Planned order versus actual presentation order after retries
expt.plannedTrialOrder = plannedTrialOrder;
expt.finalPresentationOrder = trialOrder;

expt.nTrialsOriginal = nTrialsOriginal;
expt.nPresentations = nTrials;

expt.dataFile = dataFile;

if p.eyetracking
    expt.eyeDataFile = eyeDataFile;
end

%% Store expt info
% expt.subjectID = subjectID;
% expt.run = run;
% expt.p = p;
% expt.timing = timing;
expt.subjectID = subjectID;
expt.run = run;
expt.p = p;
expt.whenSaved = datestr(now);
expt.trials_headers = trials_headers;
expt.trialResults = trialResults;
if p.staircasing == 1
    expt.threshold_s1 = threshold_s1;
    expt.threshold_s2 = threshold_s2;
    threshold_avg = mean([threshold_s1, threshold_s2]);
    expt.threshold_avg = threshold_avg;
    expt.stairs = p.stairs;
end
%% Print accuracy
completedTrials = trialResults(:,abortedTrialIdx)==0 & ~isnan(trialResults(:,correctIdx));
correctProportion = mean(trialResults(completedTrials,correctIdx));
correctPercent = correctProportion*100;
expt.correctProportion = correctProportion;
expt.correctPercent = correctPercent;

% Save what kind of run this was
expt.whichExperiment = p.staircasing;

% expt.trialsPresented = trialsPresented;
% expt.staircase = staircase;
% expt.whenSaved = datestr(now);

expt.originalTrialOrder = originalTrialOrder;
expt.driftCorrectedByTrial = driftCorrectedByTrial;

if p.eyetracking
    expt.eyeFile = eyeFile;
    expt.eyeDataDir = p.eyeDataDir;
end

%% Analyze and save data

saveData = 1;

if saveData
    save(dataFile, 'expt');
end

% Transfer the EDF after the behavioral data are safely saved
if p.eyetracking
    rd_eyeLink('eyestop', window, {eyeFile, p.eyeDataDir});

    expt.eyeDataTransferred = ...
        exist(eyeDataFile, 'file') == 2;

    if ~expt.eyeDataTransferred
        warning('EDF transfer could not be verified: %s', ...
            eyeDataFile);
    end

    % Update the MAT file with the transfer result
    save(dataFile, 'expt');
end

%% Add a feedback screen! Detailing accuracy
praise = jw_highpraise;
correctPercentString = num2str(correctPercent);
feedback = sprintf('%s Your task accuracy was %s%s!', praise, correctPercentString, '%');
Screen('FillRect', window, white*p.backgroundColor);
DrawFormattedText(window, feedback, 'center', 'center', [1 1 1]*white);
Screen('Flip', window); %command to change what's on the screen. we're drawing on a behind (hidden) window, and at the moment we screenflip, we flip that window to the front position


%% Close the program safely
WaitSecs(2);

% Wait for a press and then wait until the key is fully released.
% This prevents held keys from spilling into the MATLAB command window.
KbPressWait(devNum);
KbReleaseWait(devNum);

%% Shut down shared audio safely

% Mute first to reduce the chance of a loud shutdown transient.
try
    PsychPortAudio('Volume', pahandle, 0);
    WaitSecs(0.050);
catch ME
    warning('Could not mute PsychPortAudio before closing: %s', ...
        ME.message);
end

%{
% Snd was connected to the shared PsychPortAudio handle.
% Detach it before closing that handle.
try
    Snd('Close');
catch ME
    warning('Could not close Snd cleanly: %s', ME.message);
end
%}

% Only call Stop if playback is actually still active.
try
    paStatus = PsychPortAudio('GetStatus', pahandle);

    if paStatus.Active
        % Wait for the current playback buffer to finish naturally.
        PsychPortAudio('Stop', pahandle, 1);
    end
catch ME
    warning('Could not stop PsychPortAudio cleanly: %s', ...
        ME.message);
end

% Now it is safe to close the underlying audio handle.
try
    PsychPortAudio('Close', pahandle);
catch ME
    warning('Could not close PsychPortAudio handle: %s', ...
        ME.message);

    % Last-resort closure of all PsychPortAudio handles.
    PsychPortAudio('Close');
end

% Close the visual display last.
sca;