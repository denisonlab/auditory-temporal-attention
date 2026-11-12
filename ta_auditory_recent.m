%% Safety setup
clear;
commandwindow;

%% PTB setup
% Check we are running PTB-3
AssertOpenGL;

% Skip screen tests - ONLY for demo, not for real experiments
%Screen('Preference', 'SkipSyncTests', 1);
Screen('Preference', 'SkipSyncTests', 0);

%% Basic info
% Load parameters for this experiment
p = ta_auditory_params;

% Name the subject
%subjectID = 'PilotJW_VisualBlips'; %'E2_S0002_Stair3'; %'E2_S0070'; %'E2_S0070_Stair2'; %'E2_S0096_Stair2'; % 'S0007Newsession'; %'S0052Newsession';
%subjectID='test_mle';
subjectID='2024JWTest';
run = 3; %7; %1; %6; %8; %2; %6;
%% ^^^^^ for collaborating colleagues -- all you have to do is increase run by one and run the code! - JW
dataDir = 'data';
% Check for existing data file, while creating variable dataFile
if p.staircasing == 0
    expName = 'ta-auditory';
    %dataFile = sprintf('%s/%s/%s_run%02d_%s_%s.mat', dataDir, subjectID, subjectID, run, expName, datestr(now, 'yyyymmdd'));
    dataFile = sprintf('%s/%s_run%02d_%s_%s.mat', dataDir, subjectID, run, expName, datestr(now, 'yyyymmdd'));
elseif p.staircasing == 1
    expName = 'ta-auditory-staircase';
    dataFile = sprintf('%s/%s_%s.mat', dataDir, subjectID, expName);
elseif p.staircasing == 8
    expName = 'ta-auditory-psychometric';
    dataFile = sprintf('%s/%s_%s_%s.mat', dataDir, subjectID, p.psychometricSlope, expName);
else%if %p.staircasing == 2
    expName = 'ta-auditory-practice';
    dataFile = sprintf('%s/%s_practice%02d_%s_%s.mat', dataDir, subjectID, run, expName, datestr(now, 'yyyymmdd'));
end

existingDataFile = dir(dataFile);
if ~isempty(existingDataFile) && ~strcmp(subjectID, 'test') && ~strcmp(subjectID, 'testy')
    error('data file already exists!')
end


%% figure this out!

    expt{run}.data={};
    expt{run}.participant = subjectID;
    expt{run}.datetime=datetime('now');
    expt{run}.timings={};
    
    if p.staircasing==1
        expt{run}.staircasing.staircase1=[];
        expt{run}.staircasing.staircase2=[];
    end
%}

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

%% Keyboard
% Check all "devices" (keyboards, mice) for response input
devNum = -1;

%% Calculate stimulus dimensions (px) and position
pixelsPerDegree = ang2pix(1, p.screenWidthCm, screenWidthPx, p.viewDistCm);

fixSize = p.fixSize*pixelsPerDegree;

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
    thresholdFile = sprintf('%s/%s_%s.mat', dataDir, subjectID, 'ta-auditory-staircase');
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
    trialOrderFile = sprintf('%s/%s_trialOrder.mat', dataDir, subjectID);
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
    
    thresholdFile = sprintf('%s/%s_%s.mat', dataDir, subjectID, 'ta-auditory-staircase');
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

%% Generate trials data (from rd_cupcakeAperture)
%Construct trials matrix
trials_headers = {'responseCue','cueValidity','targetAxisT1',...
    'targetAxisT2','sweepDirT1','sweepDirT2','attendCue','responseKey',...
    'responseDir','correct','rt','sweepTilt','currentStaircase','stairIdx'};

% Preallocate the trialsResults, sorting the randomized trials by iTrial
% order
trialResults = trials(trialOrder,:);

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
correctIdx = strcmp(trials_headers,'correct');
rtIdx = strcmp(trials_headers,'rt');
sweepTiltIdx = strcmp(trials_headers,'sweepTilt');
currentStaircaseIdx = strcmp(trials_headers, 'currentStaircase');
stairIdxIdx = strcmp(trials_headers, 'stairIdx');

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
instructionsFull = sprintf('%s\n\nThere will be two tones followed by a white ring. Your goal is to determine whether one of the tones sweeps up (press 1), or sweeps down (press 2)\n\nIf the inner white ring lights up, respond to the first tone.\n\nIf the outer white ring lights up, respond to the second tone.\n\nYou will always get feedback after each sequence of events.\n\nFeel free to ask any questions now, or press any button to continue to the task!', instructions);
DrawFormattedText(window, instructionsFull, 'center', 'center', [1 1 1]*white);
Screen('Flip', window); %command to change what's on the screen. we're drawing on a behind (hidden) window, and at the moment we screenflip, we flip that window to the front position
KbWait(devNum);  
timeStart = GetSecs;

%% Testing DrawFixation

graydark  = [0.4,0.4,0.4]*white;
graylight = [1, 1, 1]*white;
    % colorFixation = [graylight; graylight; graylight];
colorFixation = [graydark; graydark; graylight];
sizeFixation = [2.25*fixSize; 1.5*fixSize; 0.5*fixSize]; % experiment with center fixation dot only
sizeLength = sizeFixation(3)/3;
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

clickOffsetDur = round((0.200-p.clickDur)*44100);


%% 
%% Loops

for iTrial = 1:nTrials
if iTrial == ((nTrials)/2) + 1
    praise = jw_highpraise;

    takeABreak = sprintf('%s You have completed %s trials! Please feel free to take a break before you finish the last %s trials.', praise, iTrial, iTrial);
    Screen('FillRect', window, white*p.backgroundColor);
    DrawFormattedText(window, takeABreak, 'center', 'center', [1 1 1]*white);
    Screen('Flip', window);
    WaitSecs(2)

    takeABreak = sprintf('%s You have completed %s trials! Please feel free to take a short break before you finish the last %s trials.\n\n Press any key to continue', praise, iTrial, iTrial);
    Screen('FillRect', window, white*p.backgroundColor);
    DrawFormattedText(window, takeABreak, 'center', 'center', [1 1 1]*white);
    Screen('Flip', window);

    KbPressWait;
else
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

    %% generate the color of the cue based on attendCue
    if attendCue == 1 % cyan for T1
        colorCue = [0,1,1]*white;
        textCue = "T1";
        colorsCue = [graydark; graylight; graylight];
    elseif attendCue == 2 % yellow for T2
        colorCue = [1,1,0]*white;
        textCue = "T2";
        colorsCue = [graylight; graydark; graylight];
    elseif attendCue == 3 % magenta for both
        % colorCue = [1,0,1]*white;
        colorCue = [0.25,0.25,0.25]*white; % let's try a graylight neutral
        textCue = "Both";
        colorsCue = [graylight; graylight; graylight];
    end

    %% generate the color of the response cue based on responseCue
    if responseCue == 1
        colorResponse = [0,1,1]*white;
        textResponse = "T1";
        colorsResponse = [graydark; graylight; graylight];
    elseif responseCue == 2
        colorResponse = [1,1,0]*white;
        textResponse = "T2";
        colorsResponse = [graylight; graydark; graylight];
    end

    %% 20230919 visuaBlips graydarkFixation
    if p.graydarkFixation
        colorFixation(3,:) = graydark;
        colorsCue(3,:) = graydark;
        colorsResponse(3,:) = graydark;
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
%% draw the fixation
jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
% drawFixation(window, cx, cy, fixSize, p.fixColor*white);
timeFix = Screen('Flip', window);

    %% generate the cue itself
    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorsCue, sizeLength);
    % drawFixation(window, cx, cy, fixSize, colorCue);
    %DrawFormattedText(window, sprintf('%s',textCue), 'center', 'center', colorCue);
    timeCueOnset = Screen('Flip', window, timeFix + 1 - slack);
    %%%% sound(s, p.Fs); % sound the click!
    %sound(trialStim, p.Fs);
    %% Put this above, right after timeFix, with timeCueOnset dependent on timeTrial + 200 ms or 500 ms, <1 s
    %and/or play a dummy sound before the very first click
    %maybe when PTB is ready, make a nice boop and flips dummy screen
    timeTrial = PsychPortAudio('Start', pahandle, [], [], 1);
    %% What about adding blank buffers? Think abt this more

    jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
    % drawFixation(window, cx, cy, fixSize, p.fixColor*white);
    timeCueOffset = Screen('Flip', window, timeCueOnset + 0.200 - slack);
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
    WaitSecs(p.ISIDur+p.sweepDur+p.SOADur+p.sweepDur+p.ISIOffsetDur);
end

    %% collect response

jw_drawFixation(window, cx, cy, 3, sizeFixation, colorsResponse, sizeLength);
% drawFixation(window, cx, cy, fixSize, colorResponse);
%DrawFormattedText(window, sprintf('%s',textResponse), 'center', 'center', colorResponse);
timeResponseOnset = Screen('Flip', window, timeCueOffset - slack);
%% Fix this ^^^
%%%%sound(s, p.Fs); % sound the click!
jw_drawFixation(window, cx, cy, 3, sizeFixation, colorFixation, sizeLength);
% drawFixation(window, cx, cy, fixSize, p.fixColor*white);
timeResponseOffset = Screen('Flip', window, timeResponseOnset + 0.200 - slack);
[secs, keyCode] = KbWait(devNum);
rt = secs - timeTrial;
responseKey = find(keyCode);

    %% Response and store
%trialsHeaders = {'RT','ResponseKey'};
%trialIdx = 1;
%trials(trialIdx, 1) = rt;
%trials(trialIdx, 2) = responseKey;

% up key 1 = 11 or 49; down key 2 = 12 or 50
if responseKey == p.keyOne
   responseDir = 1;
elseif responseKey == p.keyTwo
   responseDir = 2;
else
   responseDir = 3;
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
%colorsFeedback = [colorsResponse(1,:); colorsResponse(2,:); colorFeedback];
colorsFeedback = [graydark; graydark; colorFeedback];

% Store trial info
%trials(iTrial,iti2Idx) = iti; % the actual ITI, including extra delay
%ITI code from cupcake, not sure if i need this just yet
trialResults(iTrial,attendCueIdx) = attendCue;
% If people press 2 keys, we would still want a record of that
% cell array to save all responseKey
expt = {};
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
if p.staircasing == 1
    trialResults(iTrial,sweepTiltIdx) = p.stairs(stairIdx);
    trialResults(iTrial, currentStaircaseIdx) = currentStaircase;
    trialResults(iTrial, stairIdxIdx) = stairIdx;
end

% If staircasing, updateStaircase and getReversalValues
if p.staircasing == 1 && currentStaircase == 1
    [stairIdx, lastFewAcc_s1] = updateStaircase(p.stairs, stairIdx, lastFewAcc_s1, correct);
    stairIdx_s1 = stairIdx;
    stairValues(iTrial) = stairIdx;
    stairValues_s1(idivide(int64(iTrial),int64(2),'ceil')) = stairIdx;
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
    stairValues_s2(idivide(int64(iTrial),int64(2),'ceil')) = stairIdx;
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
end
%% Print accuracy
correctProportion = mean(trialResults(1:nTrials,correctIdx));
correctPercent = correctProportion*100;
expt.correctProportion = correctProportion;
expt.correctPercent = correctPercent;

% Save what kind of run this was
expt.whichExperiment = p.staircasing;

% expt.trialsPresented = trialsPresented;
% expt.staircase = staircase;
% expt.whenSaved = datestr(now);

%% Analyze and save data

saveData = 1;

if saveData
   save(dataFile, 'expt')
end

%% Add a feedback screen! Detailing accuracy
praise = jw_highpraise;
correctPercentString = num2str(correctPercent);
feedback = sprintf('%s Your task accuracy was %s%s!', praise, correctPercentString, '%');
Screen('FillRect', window, white*p.backgroundColor);
DrawFormattedText(window, feedback, 'center', 'center', [1 1 1]*white);
Screen('Flip', window); %command to change what's on the screen. we're drawing on a behind (hidden) window, and at the moment we screenflip, we flip that window to the front position


%% Close the program by pressing a button
WaitSecs(2);
KbPressWait;
sca;
PsychPortAudio('Stop');
PsychPortAudio('Close');