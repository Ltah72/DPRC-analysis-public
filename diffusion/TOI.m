%This is the tract of interest (TOI) pipeline (e.g. superior longitudinal
%fasciculus (SLF)) which will select certain TOIs using manually generated 
%ROI masks and data from the FBA pipeline. Unlike the other pipelines, this 
%pipeline will require more manual user input (e.g. going into the viewing 
%gui, mrview, with the tract(s) that you select. Also, unlike my other 
%scripted pipelines, there is no 'formal manual' for this from mrtrix, 
%and many of the steps are referenced by the mrtrix community forum and 
%logic of steps/analysis - see in my created manual for more details.

% Steps:
% 1. Compute track density image (TDI)
% 2-3. Options: (1)Generate seedpoints (see below first script) (2)Create 
% mask for the track of interest (TOI)
% 4. Manually include/exclude fibres on your TOI if needed
% 5. Create a fixel mask with your TOI
% 6. Threshold the TOI mask
% 7. Compute fixel-based metrics against TOI mask per each participant 

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 04/08/20

clc;
clear all;
close all;


%define/add pathways
startdir = input('Please enter data directory:', 's');
%e.g., startdir = '/data/USERS/LENORE';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/diffusion';

%should be the same groupname from what the user analysed in the FBA script.  
groupname = input('Which pre-processed group / study do you want to continue to analyse?: ', 's');

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%Create TOI folder
TOI = input('Please name a tract of interest (TOI) that you want to analyse: ', 's');

mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/' TOI]);

%go into group folder
cd([startdir '/derivatives/' period, '/diff_data/', groupname]);

%define variables
participants = dir(fullfile('preprocessed_dwi', '*.mif'));

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(startdir));
addpath(genpath(ScriptDirectory));

%copy needed files into the TOI directory
copyfile ([startdir '/derivatives/' period, '/diff_data/' groupname '/wmfod_template.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname '/' TOI]);
copyfile ([startdir '/derivatives/' period, '/diff_data/' groupname '/tracks_2_million_sift.tck'], [startdir,'/derivatives/' period, '/diff_data/', groupname '/' TOI]);

%go into TOI directory
cd ([startdir,'/derivatives/' period, '/diff_data/', groupname, '/' TOI]);


%-------------------------------------------------------------------------%
%Step 1: Create a track map (TDI) in order to see the tracts a bit better.

unix(['tckmap -template wmfod_template.mif -dec tracks_2_million_sift.tck tdi_image.mif']);


%-------------------------------------------------------------------------%
%Step 2: Create a mask of your TOI - manually select ROI(s) in mrview for 
%this. Use TDI image and other anatomical references as guides to do this.

%%%%% go into mrview and create a manual mask for your ROI/TOI %%%%%

% Once you created your ROI mask, move it into the current processing
%directory.

mask = input('Please provide the full name of the TOI mask (e.g. mask_corpus_callosum.mif): ', 's');


%-------------------------------------------------------------------------%
%Step 3: Extract TOI from the previously generated whole-brain SIFT 
%tractography file
unix(['tckedit tracks_2_million_sift.tck -include ' mask ' ' TOI '_sift.tck']);
%e.g. of full command:
%unix(['tckedit tracks_2_million_sift.tck -include mask_corpus_callosum.mif CorpusCallosum_sift.tck']);


%-------------------------------------------------------------------------%
%Step 4: Manual inclusion/exclusion of fibres (if needed)
%%%%% go into mrview for this if needed %%%%%, 
% Make your edits to the line below %

%unix(['tckedit ' TOI '_sift.tck -exclude -1.9,-21.5,-40.1,30 -exclude 10.3,-14.8,3.9,15 -exclude -12.8,-18.3,3.2,15 ' TOI '_sift_edit.tck -force']);

%e.g. of full command:
%unix(['tckedit CorpusCallosum_sift.tck -exclude -1.9,-21.5,-40.1,30 -exclude 10.3,-14.8,3.9,15 -exclude -12.8,-18.3,3.2,15 CorpusCallosum_sift_edit.tck']);


%-------------------------------------------------------------------------%
%Step 5: Create fixel mask with your TOI - make a fixel-wise TDI
ManualEdits = input('After viewing your TOI, did you make additional edits? (y or n): ', 's');

%with edits
if ManualEdits == 'y'
    unix(['tck2fixel ' TOI '_sift_edit.tck ' startdir,'/derivatives/' period, '/diff_data/', groupname '/fixel_directory/fixel_mask output_' TOI '_TOI_fixel_directory ' TOI '_fixel_mask.mif']);
%no edits done    
elseif ManualEdits == 'n'
    unix(['tck2fixel ' TOI '_sift.tck ' startdir,'/derivatives/' period, '/diff_data/', groupname '/fixel_directory/fixel_mask output_' TOI '_TOI_fixel_directory ' TOI '_fixel_mask.mif']);
end

%e.g. of full command:
%unix(['tck2fixel CorpusCallosum_sift_edit.tck ' startdir,'/derivatives/diff_data/', groupname '/fixel_directory/fixel_mask output_CorpusCallosum_TOI_fixel_directory CorpusCallosum_fixel_mask.mif']);


%-------------------------------------------------------------------------%
%Step 6: Threshold the TOI mask
unix(['mrthreshold -abs 0.95 output_' TOI '_TOI_fixel_directory/' TOI '_fixel_mask.mif output_' TOI '_TOI_fixel_directory/' TOI '_fixel_mask_thr.mif']);


%-------------------------------------------------------------------------%
%Step 7: Compute fixel-based metrics (FD, FC, FDC) with the TOI fixel mask per each participant.
CreateTOIFBAMetricFiles(participants, TOI, startdir, groupname, period);







%%%% Test SLF script below: %%%%

%SLF tract parameters

%left SLF
unix(['tckgen wmfod_template.mif -select 10k -maxlen 250 -minlen 10 -angle 45 -power 1.0 -cutoff 0.15 -seed_sphere 32.62,30.05,26.56,3 -seed_sphere 38.13,-30.24,24.24,2 -exclude 16.59,17.93,27.23,2 -exclude 46.95,-21.89,-18.17,5 -exclude 36.59,-31.79,-2.25,1 -exclude 39.12,-26.26,-4.86,2 -exclude 46.03,7.805,-7.66,2 -exclude 49.34,-35.52,-14.41,2 -exclude 50.96,-34.06,-13.5,2 -exclude 42.1,-37,-13.57,2 -exclude 38.3,-39.5,-3.83,2 -exclude 39.75,-41.43,-15.13,1 -exclude 36.41,-45.01,-1.24,1 -exclude 43.92,-34.85,-9.75,1 -exclude 17.36,15.72,28.83,1 -exclude 14.9,18.27,24.22,1 -exclude 29.44,-12.79,17.54,1 -exclude 25.8,10.52,12.03,1 -exclude 41.77,-38.25,-11.79,1 -exclude 39.1,-39.28,-1.16,1 -exclude 41.09,-41.09,-10.73,1 SLF_track_L.tck -force']);

%right SLF
unix(['tckgen wmfod_template.mif -select 10k -maxlen 250 -minlen 10 -angle 45 -power 1.0 -cutoff 0.15 -seed_sphere -32.07,-31.95,27.1,2 -seed_sphere -29.58,-29.02,32.48,1 -exclude -20.6,-26.88,30.97,2 -exclude -28.37,-25.79,18.15,2 -exclude -45.73,-38.22,-16.67,3 -exclude -39.33,-38.52,-6.52,2 -exclude -15.1,-16.68,34.53,2 -exclude -42.81,-36.38,-15.77,2 -exclude -21.52,-25.73,32.96,1 -exclude -46.22,-38.92,-13.32,2 -exclude -37.78,-42.51,-4.69,2 -exclude -29.12,-22.63,23.17,1 -exclude -29.56,-29.87,21.41,1 -exclude -30.43,-26.8,21.34,1 -exclude -23.05,-24.8,33.88,1 -exclude -41.68,-40.76,-11.95,2 -exclude -46.46,-33.85,-14.63,1 -exclude -20.87,-14.17,32.98,1 -exclude -30,-26.52,22.69,1 -exclude -36.92,-45.25,-0.20,1 -exclude -44.28,-38.71,-14.34,1 -exclude -40.8,-41.74,-14.69,1 -exclude -39.51,-43.72,-9.23,1 -exclude -39.51,-45.39,-10.57,1 -exclude -39.51,-44.91,-8.12,1 -exclude -39.3,-47.31,-14.82,1 -exclude -30,-24.42,23.55,1 -exclude -49.07,-39.52,-14.89,2 -exclude -29.29,-25.85,24.52,1 -exclude -31.06,-24.19,24.01,1 -exclude -23.29,-18.45,31.67,1 -exclude -40.55,-42.32,-9.28,1 -exclude 37.21,-47.15,-2.85,1 -exclude -44.69,-38.38,-11.58,1 -exclude -38.55,-49.75,-12.89,2 -exclude -35.49,-48.78,-2.35,1 SLF_track_R.tck -force']);


%merge the left and right tracks as one track file. 
unix(['tckedit SLF_track_L.tck SLF_track_R.tck SLF_track_whole.tck']);

%create fixel mask with your track file. 
unix(['tck2fixel SLF_track_whole.tck fixel_mask output_SLF_fixel_mask SLF_fixel_mask.mif'])

%threshold your mask (whole)
unix(['mrthreshold -abs 0.5 output_SLF_fixel_mask/SLF_fixel_mask.mif output_SLF_fixel_mask/SLF_fixel_mask_thr.mif -force']);



%%%Left SLF %%%
%create fixel mask with your track file (left SLF). 
unix(['tck2fixel SLF_track_L.tck fixel_mask output_SLF_track_L_fixel_mask SLF_L_fixel_mask.mif'])
%threshold your mask (left SLF)
unix(['mrthreshold -abs 0.5 output_SLF_track_L_fixel_mask/SLF_L_fixel_mask.mif output_SLF_track_L_fixel_mask/SLF_L_fixel_mask_thr.mif']);

%%%Right SLF %%%
%create fixel mask with your track file (left SLF). 
unix(['tck2fixel SLF_track_R.tck fixel_mask output_SLF_track_R_fixel_mask SLF_R_fixel_mask.mif'])
%threshold your mask (right SLF)
unix(['mrthreshold -abs 0.5 output_SLF_track_R_fixel_mask/SLF_R_fixel_mask.mif output_SLF_track_R_fixel_mask/SLF_R_fixel_mask_thr.mif']);


%Compute fixel-based metrics (FD, FC, FDC) with the TOI fixel mask per each participant.
CreateTOIFBAMetricFiles(participants, TOI, startdir, groupname);




















