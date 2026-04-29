%Modify files from running fmriprep into FSL's MELODIC format in order for
%FSL's FIX to identify and process the files. 

%Reference from the Neurostars forum post: https://neurostars.org/t/ica-fix-wth-fmriprep/3638/9 
% and FSL forum post (for verification from Ludovica G. (creator of FIX)): https://www.jiscmail.ac.uk/cgi-bin/webadmin?A2=FSL;6e71614b.1911

clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%choose which fMRI file name you want to use to process (e.g.
%_task-rest_run-1_space-MNI152Lin2009cAsym_desc-preproc_bold.nii.gz')
fMRI_filename = input('Please enter the preprocessed fMRI file name you wish to use:', 's');
%e.g., fMRI_filename = '_task-rest_run-1_space-MNI152NLin2009cAsym_';
T1w_filename = input('Please enter the preprocessed T1w file name you wish to use:', 's');
%e.g., T1w_filename = '_space-MNI152NLin2009cAsym_desc-preproc_T1w.nii.gz';

%Define fmriprep directory:
FmriprepDir = input('Please enter fmriprep directory:', 's');
%e.g., FmriprepDir = '/data/USERS/LENORE/fmriprep_directory/';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/';

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(FmriprepDir));
addpath(genpath(ScriptDirectory));

%go into fmriprep derivatives directory
cd([FmriprepDir 'derivatives/fmriprep/']);

%choose participants to run FIX on
FIX_participants = uipickfiles;

%copy the necessary preprocessed files from fmriprep into a new folder (named for_FIX.ica)
for i = 1:length(FIX_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(FIX_participants{1,i});

    %go into each participant's folder
    cd ([PAR_NAME '/func/']); 
    
    %create a 'FIX folder' (for_FIX.ica) for each participant
    mkdir for_FIX.ica;
    
    %create the subdirectories within the FIX folder
    mkdir for_FIX.ica/reg
    mkdir for_FIX.ica/mc
    
    %place the filtered_func_data.ica folder and contents into the FIX folder
    movefile filtered_func_data.ica for_FIX.ica

    %make a copy, rename, and move preprocessed files into the FIX folder 
    %For preprocessed 4D data
    copyfile([PAR_NAME, fMRI_filename, 'desc-preproc_bold.nii.gz'], ['filtered_func_data.nii.gz']);
    movefile('filtered_func_data.nii.gz', 'for_FIX.ica');
    
    %For the motion parameters data created by mcflirt - run python code
    unix(['python ' ScriptDirectory 'OnlineSourceCode/fmriprep2hcp_mvmt_regressors.py ' PAR_NAME '_task-rest_run-1_desc-confounds_timeseries.tsv prefiltered_func_data_mcf.par']);
    movefile('prefiltered_func_data_mcf.par', 'for_FIX.ica/mc');

    %For brain mask
    copyfile([PAR_NAME, fMRI_filename, 'desc-brain_mask.nii.gz'], ['mask.nii.gz']);
    movefile('mask.nii.gz', 'for_FIX.ica');
    
    %For mean functional data - calculate temporal mean of your 4D fMRI data
    unix(['fslmaths ' PAR_NAME, fMRI_filename, 'desc-preproc_bold.nii.gz -Tmean mean_func.nii.gz']); 
    movefile('mean_func.nii.gz', 'for_FIX.ica');

    %create example func data - this is the 3D reference vol used for
    %motion correction
    copyfile([PAR_NAME, fMRI_filename, 'boldref.nii.gz'], ['example_func.nii.gz']);
    movefile('example_func.nii.gz', 'for_FIX.ica/reg');
    
    %For T1w (the brain-extracted structural) from anat directory
    copyfile([FmriprepDir 'derivatives/fmriprep/' PAR_NAME '/anat/' PAR_NAME, T1w_filename], ['highres.nii.gz']);
    movefile('highres.nii.gz', 'for_FIX.ica/reg');
    
    %For the FLIRT transform matrix, from structural to functional space -
    %create an identity matrix, as suggested by: https://github.com/nipreps/fmriprep/issues/1765
    id_mat = [1, 0, 0, 0; 0, 1, 0, 0; 0, 0, 1, 0; 0, 0, 0, 1];
    save('highres2example_func.mat', 'id_mat');
    movefile('highres2example_func.mat', 'for_FIX.ica/reg');
    
    %go back into fmriprep derivatives directory
    cd([FmriprepDir 'derivatives/fmriprep/']);
end 
