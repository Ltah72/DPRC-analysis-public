%Run the FIX (FSL's ICA classifier) on select participants. First run it to 
%create the weighted file. Then, run it on the rest of the participant 
%files. 


clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%Define fmriprep directory:
FmriprepDir = input('Please enter fmriprep directory:', 's');
%e.g., FmriprepDir = '/data/USERS/LENORE/fmriprep_directory/';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/';

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(FmriprepDir));
addpath(genpath(ScriptDirectory));

%specify directory where hand label noise text files are
HandLabelsICA_dir = input('Please enter directory that holds your manually classified ICA labels:', 's');
%e.g., HandLabelsICA_dir = '/data/USERS/LENORE/MELODIC/hand_labels_text_files/';

%go into fmriprep derivatives directory
cd([FmriprepDir 'derivatives/fmriprep/']);

%choose participants to train FIX on
FIX_train_PAR = uipickfiles;

%copy the necessary preprocessed files from fmriprep into a new folder (named for_FIX.ica)
for i = 1:length(FIX_train_PAR)
    
    [upper_path, PAR_NAME, ~] = fileparts(FIX_train_PAR{1,i});

    %copy, rename, & move hand label noise text files into each of the participant's FIX files.
    copyfile([HandLabelsICA_dir, 'labels_' PAR_NAME, '.txt'], ['hand_labels_noise.txt']);
    movefile(['hand_labels_noise.txt'], [FmriprepDir 'derivatives/fmriprep/' PAR_NAME '/func/for_FIX.ica']);
end 


%run FIX - the output weighted file will be in the fix training files
%directory. Note that you need to list out your participants and their 
%specific directories here. (INCOMPLETE)
unix(['/SOFTWARE/fix/fix -t /SOFTWARE/fix/training_files/TrainingDPRC.RData -l ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0001F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0019F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0020F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0057F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0069F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-CDPRC0034F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-CDPRC0036F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-CDPRC0041F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-DDPRC0012F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-DDPRC0013F0/func/for_FIX.ica ' FmriprepDir, 'derivatives/fmriprep/sub-DDPRC0022F0/func/for_FIX.ica']);
unix(['/SOFTWARE/fix/fix -t /SOFTWARE/fix/training_files/TrainingDPRC.RData -l ' FmriprepDir, 'derivatives/fmriprep/sub-ADPRC0001F0/func/for_FIX.ica']);

%unix(['/SOFTWARE/fix/fix -t TrainingDPRC.RData -l /data/USERS/LENORE/MELODIC/sub-ADPRC0001F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-ADPRC0019F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-ADPRC0020F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-ADPRC0057F0 /data/USERS/LENORE/MELODIC/sub-ADPRC0069F0 /data/USERS/LENORE/MELODIC/sub-CDPRC0034F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-CDPRC0036F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-CDPRC0041F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-DDPRC0012F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-DDPRC0013F0/SingleSubICA.ica /data/USERS/LENORE/MELODIC/sub-DDPRC0022F0/SingleSubICA.ica']);


%once you have your trained weighted file, you will now run FIX to classify
%the rest of your data. 


