%single ICA MELODIC per each subject (for denoising). This is done after
%fmriprep, hence the options to omit the preprocessing steps. 

clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%Define fmriprep directory, so that it may be used to get preprocessed data 
%to run ICA:
FmriprepDir = input('Please enter fmriprep directory:', 's');
%e.g., FmriprepDir = '/data/USERS/LENORE/fmriprep_directory/';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g, ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/';

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(FmriprepDir));
addpath(genpath(ScriptDirectory));

%go into fmriprep derivatives directory
cd([FmriprepDir 'derivatives/fmriprep/']);

%choose participanst to run single-session ICA
ICA_participants = uipickfiles;

for i = 1:length(ICA_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(ICA_participants{1,i});

    %go into each participant's folder
    cd ([PAR_NAME '/func/']); 
    
    %Run single-session MELODIC and place output into participant directory - some options available below. 

    %data formatted to a standard template (e.g., MNI) 
    unix(['melodic -i ' PAR_NAME '_task-rest_run-1_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz -m ' PAR_NAME '_task-rest_run-1_space-MNI152NLin2009cAsym_desc-brain_mask.nii.gz --nobet -o filtered_func_data.ica']);

    %participant specific
    %unix(['melodic -i ' PAR_NAME '_task-rest_run-1_desc-preproc_bold.nii.gz -m /SOFTWARE/fsl/data/standard/MNI152_T1_1mm_brain_mask.nii.gz --nobet -o SingleSubICA.ica']);
   
    %go back into fmriprep derivatives directory
    cd([FmriprepDir 'derivatives/fmriprep/']); 
end
