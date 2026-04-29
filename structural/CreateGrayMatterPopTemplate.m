%Create a grey matter template based on study cohort. This will be done
%mostly via FSLVBM
%(https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FSLVBM/UserGuide). This template
%will be used for resting-state ICA. 

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 01/03/21

clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%define/add pathways
GreyMatPopTempDir = input('Please enter grey matter directory:', 's');
%e.g., GreyMatPopTempDir = '/data/USERS/LENORE/fslvbm_template/';

%Define your sourcedata directory:
sourcedataDirectory = input('Please enter sourcedata directory:', 's');
%e.g., sourcedataDir = (['/data/sourcedata/' period]);

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/';

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(GreyMatPopTempDir));
addpath(genpath(ScriptDirectory));

%go into sourcedata file
cd ([sourcedataDir]);

%1. Place all of your T1w data into your FSL-VBM directory. Choose an equal
%amount of participants per of group for this.  
GM_pop_participants = uipickfiles;

for i = 1:length(GM_pop_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(GM_pop_participants{1,i});
    copyfile ([sourcedataDir, '/' PAR_NAME '/anat/' PAR_NAME '_T1w.nii'], [GreyMatPopTempDir]); 

end

%go into your FSL-VBM directory
cd ([GreyMatterPopDir]);

%2. Extract brain information using BET. 
unix('fslvbm_1_bet -b -B -f 0.3'); 
%I found that this setting best fit the DPRC data. -B in bet2 to try and
%redice image bias and neck voxels, while keeping the threshold (-f 0.3) a
%little bit more liberal than its default setting (-f 0.5). 

%3. Create your template
unix('fslvbm_2_template -n');
%This is a template based on non-linear registration (-n option). 

