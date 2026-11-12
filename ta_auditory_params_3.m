function p = ta_auditory_params_3

% Things to look out for:
% 1. PsychPortAudio Devices -- artificial deadline function
% 2. Screen resolution
% 3. p.OS, p.keyOne, p.keyTwo -- is there a way to get the keyboard numbers
% based on the name
% KbName('1!') to mitigate imaginary numpad -- artificial deadline function
% 4. p.staircasing; determines what part of the experiment we are running
% fix this too as well, and make them strings

% Screen
%p.screenWidthCm = 20.9; %20;do not define here
p.viewDistCm = 57; %75; %57; %75; %57; %75; %57; %75;%57; %75; % for monitor
%p.viewDistCm = 57; % for laptop

% ^^ adjust the above for linux monitor. 75 cm

%{
% Initialize Windows vs Linux
p.OS = 2; % 1 for Linux, 2 for Windows
if p.OS == 1 % Linux
    p.keyOne = 11;
    p.keyTwo = 12;
elseif p.OS == 2 % Windows
    p.keyOne = 49;
    p.keyTwo = 50;
end
%}
%% Identify the correct key numbers
KbName('UnifyKeyNames');

%% Identify the four valid response keys
p.keyOne = KbName('1!');
p.keyTwo = KbName('2@');
p.keyNine = KbName('9(');
p.keyZero = KbName('0)');

% Only these four keys are accepted as responses
p.allowedResponseKeys = ...
    [p.keyOne p.keyTwo p.keyNine p.keyZero];

% Fixation
%p.fixSize = 0.5; % degrees visual angle
p.outerRingSize = 2.7344;
p.innerRingSize = 1.8230;
p.fixSize = 0.6077;
p.fixColor = 1; % white

% Images
p.backgroundColor = 0.5; % gray
p.imPos = [0 0];
p.imDur = 1; % s
p.gratingSize = 4; % degrees visual angle
p.gratingSF = 1; % cycles per degree
p.gaborSD = 0.5; % about 4 SDs will be visible at full contrast

% Sounds
p.Fs = 44100; % samples per second
p.tLev = 80; %70; %80; % Target level, "dB SPL", arbitrary, usually at 80

p.rms1Lev = 100; % Norm. value for RMS of 1, arbitrary
p.tScale = 10.^((p.tLev-p.rms1Lev)./20); % Scale factor for target level setting
p.sweepDur = 0.030; % duration of each sweep in seconds
p.rampDur = 0.005; % duration of each onset/offset ramp in seconds
p.SOA = 0.250; % onset-to-onset SOA, seconds
p.SOADur = p.SOA - p.sweepDur; % silence between T1 offset and T2 onset
% method = 'linear'; % sweep type, linear so duration of sweep before and after CF is equivalent
p.method = 'logarithmic';
p.ISIDur = 0.800; %1.000;
p.ISIOffsetDur = 0.470; %0.500;
p.responseDelayDur = 0.600; % gray-dot delay before response cue, seconds
% Total time from response-cue onset until responses are allowed.
% The number is visible for the first 0.200 seconds.
p.clickDur = 0.0005;           % duration of each click in seconds
p.clickRampDur = p.clickDur/2;  % duration before the click peak in seconds
p.clickFreq = 8000;           % highest frequency of click peak in Hertz

% Titration / Staircasing
% Have these named as strings and call
%{
0 = 'main'
'staircase'
'showcase'
'slow-neutral-easy'
'fast-neutral-easy'
'allvalid'
'fullpractice'
%}

p.eyetracking = 1; % 1 = use EyeLink fixation monitoring; 0 = no eye tracking
p.eyeDataDir = 'eyedata';
p.eyetrackSize = 3; % allowable fixation diameter in dva. turns into pixel radius
p.preCueFixDur = 1.000; % fixation buffer after EyeLink is ready
% Fixation-break warning shown when an aborted trial is presented again
p.fixBreakTextSize = 28;
p.fixBreakTextOffset = 1.5; % degrees above fixation

% Visual cue style: 'ring' or 'number'
p.ringOrNum = 'number';

% If you are testing quickly, use 3 4 or 1 - JW

% When should the fixation center dim?
% 'none'     = never dim
% 'delay'    = dim while responses are prohibited
% 'response' = dim once responses are allowed
p.grayWhen = 'response';



p.staircasing = 0; % 0 for main experiment, 1 for staircasing session, 2/3/4/6/7 for practice
% p.staircasing = 2; showcase 40, one-target trials
% for 3; slowed down, all-neutral trials with easy tilts 
% for 4; full-speed, all-neutral trials with easy tilts
% for 6; full-speed, all-valid trials with staircased threshholded tilts
% for 7; full-speed, varying-validity trials with staircased threshholded
% for 8; psychometric function
% tilts
% p.stairs = [0.005 0.030 0.055 0.080 0.105 0.130 0.155 0.180 0.205 0.230 0.255 0.280 0.305 0.330 0.355 0.380 0.405 0.430 0.455 0.480]; %0.505 0.530 0.555 0.580];%0.605 0.630 0.655 0.680];
% Expt 1
% p.stairs = [0.080 0.130 0.180 0.230 0.280 0.330 0.380 0.430 0.480 0.530];
% Expt 2
%p.stairs = [0.030 0.080 0.130 0.180 0.230 0.280 0.330 0.380 0.430];
% Still slightly too easy for me, might need to lower everything:
%p.stairs = [0.010 0.060 0.110 0.160 0.210 0.260 0.310 0.360 0.410 0.460];
% Adding finer steps
% For S70
% for s70 p.stairs = [0.160 0.185 0.210 0.235 0.260 0.285 0.310 0.335 0.360 0.385 0.410 0.435 0.460 0.485 0.510 0.535 0.560];
% p.stairs = [0.005 0.0075 0.010 0.0225 0.035 0.0475 0.060 0.085 0.110 0.135 0.160 0.185 0.210 0.235 0.260 0.285 0.310 0.335 0.360 0.385 0.410 0.435 0.460];
% for S02
p.stairs = [0.005 0.0075 0.010 0.0225 0.035 0.0475 0.060 0.085 0.110 0.135 0.160 0.185 0.210 0.235 0.260];
% Go for even spacing, since I am already in log/proportion scales
p.staircaseTrials = 160; %130;%150; % total for both staircases.. must be divisible by 2

% Experimental
p.experimentTrials = 160; %5;

% Practice
p.practiceTrials = 32; %usually 20 or 32

p.psychometricSlope = 0.016;

% TA-Auditory along with jw_soundClick
% Specify and choose the audio device
% Make sure the name is specific enough to identify one device!
p.deviceName = 'Scarlett'; %'Scarlett'; %'Analog'; %'Speaker'; %'Scarlett'; %Scarlett';%'Scarlett'; %'Headphones';
% 'Headphones' for Juneau's AirPods Pro on his computer
% 'Scarlett' for FocusRite Audio Interface
% 'Crestron' for Lab Meeting Display
% 'Speaker' for Laptop Speakers
% 'Analog' for EEG Room -> Linux -> Speakers
PsychPortAudioDevices = PsychPortAudio('GetDevices');
PsychPortAudioDeviceIndex = [PsychPortAudioDevices.DeviceIndex];
PsychPortAudioDeviceNames = {PsychPortAudioDevices.DeviceName};
DeviceNameIndex = find(contains(PsychPortAudioDeviceNames,p.deviceName));
% Careful, this takes the FIRST deviceNum that qualifies from
% DeviceNameIndex
p.deviceNum = PsychPortAudioDeviceIndex(DeviceNameIndex(:,1));


% Variations on the task
p.earStim = 0;
p.distractor = 0;
p.earDist = 0;
p.targetBlips = 0;
p.graydarkFixation = 0;
