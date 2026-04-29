%Edit the sourcedata files for fmriprep. Mainly, this is editing the /fmap
%.json files, as specified below. 

%Also, to keep the format of the .json file, an online source function, 
%saveAsJSON.m, was used for this: (modification by Liu, A.Y.C. 2020; based 
%on Kirsch, L. 2015). 
%https://au.mathworks.com/matlabcentral/fileexchange/77284-structure-and-object-to-json

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 10/01/21

%Define fmriprep directory, so that it used:
FmriprepDir = input('Please enter fmriprep directory:', 's');
%e.g., FmriprepDir = '/data/USERS/LENORE/fmriprep_directory/';

%go into fmriprep sourcedata directory
cd([FmriprepDir 'sourcedata/']);

%choose participants
participants = uipickfiles;

for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    
    %edit all .json files from the fmap folder - need to modify the 'IntendedFor' section
    cd ([PAR_NAME '/fmap/']);
    %first mag file
    jsonMag1 = fileread([PAR_NAME '_magnitude1.json']);
    jsonDataMag1 = jsondecode(jsonMag1);
    jsonDataMag1(1).IntendedFor = ['func/', PAR_NAME '_task-rest_run-01_bold.nii'];
    jsonDataMag1(1).ImageType = '[ORIGINAL, PRIMARY, M, ND]';
    saveAsJSON(jsonDataMag1, [PAR_NAME '_magnitude1.json']);
    
    %second mag file
    jsonMag2 = fileread([PAR_NAME '_magnitude2.json']);
    jsonDataMag2 = jsondecode(jsonMag2);
    jsonDataMag2(1).IntendedFor = ['func/' PAR_NAME '_task-rest_run-01_bold.nii'];
    jsonDataMag2(1).ImageType = '[ORIGINAL, PRIMARY, M, ND]';
    saveAsJSON(jsonDataMag2, [PAR_NAME '_magnitude2.json']);
   
    %phase diff file
    jsonPhaseDiff = fileread([PAR_NAME '_phasediff.json']);
    jsonDataPhaseDiff = jsondecode(jsonPhaseDiff);
    jsonDataPhaseDiff(1).IntendedFor = ['func/' PAR_NAME '_task-rest_run-01_bold.nii'];
    jsonDataPhaseDiff(1).ImageType = '[ORIGINAL, PRIMARY, P, ND]';
    %Switch around echo 1 + echo 2 times in the _phasediff.json files in fmap
    %folder. The echo 2 time should be greater than the echo 1 time.
    Echo1 = jsonDataPhaseDiff(1).EchoTime1;
    Echo2 = jsonDataPhaseDiff(1).EchoTime2;
    jsonDataPhaseDiff(1).EchoTime1 = Echo2;
    jsonDataPhaseDiff(1).EchoTime2 = Echo1;
    saveAsJSON(jsonDataPhaseDiff, [PAR_NAME '_phasediff.json']);
    
    %go into fmriprep sourcedata directory
    cd([FmriprepDir 'sourcedata/']);   
end
