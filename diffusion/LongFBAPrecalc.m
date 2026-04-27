%For Longitudinal fixel-based analysis (FBA), specifically for looking at
%mixed-design effects (e.g. 5 x 2 ANOVA). Group x timepoint effects, which
%allows for interactions. This script will pre-calculate the rate of change 
%of the images over time. Do this optional step after having run through
%the CSD and most of the FBA pipline (specifically, after step 6 of
%smoothing your participant fixels). 

%Longitudinal study design advice (use mrcalc and pre-calculate individual difference-over-time images): 
%https://community.mrtrix.org/t/longitudinal-fixel-based-analysis/910/2 
%https://community.mrtrix.org/t/replicating-longitudinal-fixel-based-analysis-approach/2071/15  
%https://community.mrtrix.org/t/2x2-mixed-design/4072/3 


%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 29/10/21

clc;
clear all;
close all;

%define/add pathways
derivdir = input('Please enter derivatives directory:', 's');
%e.g., derivdir = '/data/USERS/LENORE/derivatives';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/diffusion';

%should be the same groupname from what the user analysed in the CSD script.
groupname = input('Which pre-processed group / study do you want to continue to analyse (e.g cross-sectional, longitudinal)?: ', 's');

%choose time period
period = input('Which time period do you want to analyse (e.g. F0, F2, TH, all, etc)?: ', 's');

%make directories to hold subtracted fixel files in template folder
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/fd_smooth_longit/']);
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/log_fc_smooth_longit/']);
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/fdc_smooth_longit/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(derivdir));
addpath(genpath(ScriptDirectory));

%go into template folder
cd([derivdir '/groups/' period, '/diff_data/' groupname, '/template/']);

%choose participants
participants = uipickfiles;

for i = 1:2:length(participants) %choose every other participant - this is because of how the participant files are ordered as a list in our dataset. 
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:13);

    %perform subtraction method. Note that we are dividing by 730 days (for
    %accounting for the 2 year difference applied to each participant). 
    unix(['mrcalc fd_smooth/' PAR_NAME 'F2_fd.mif fd_smooth/' PAR_NAME 'F0_fd.mif -subtract 730 -div fd_smooth_longit/' PAR_NAME '_long-rate_fd.mif']);
    unix(['mrcalc log_fc_smooth/' PAR_NAME 'F2_log_fc.mif log_fc_smooth/' PAR_NAME 'F0_log_fc.mif -subtract 730 -div log_fc_smooth_longit/' PAR_NAME '_long-rate_log_fc.mif']); 
    unix(['mrcalc fdc_smooth/' PAR_NAME 'F2_fdc.mif fdc_smooth/' PAR_NAME 'F0_fdc.mif -subtract 730 -div fdc_smooth_longit/' PAR_NAME '_long-rate_fdc.mif']); 

end 

%go back into group folder
cd([derivdir '/groups/' period, '/diff_data/' groupname]);

%create text files of a participant list to run statistics
%for fd list
fid1 = fopen('files_fd_longit.txt', 'w');
files1 = dir('template/fd_smooth_longit/*DPRC*');
fprintf(fid1, '%s\n', files1.name);
fclose(fid1);

%for log_fc list
fid2 = fopen('files_log_fc_longit.txt', 'w');
files2 = dir('template/log_fc_smooth_longit/*DPRC*');
fprintf(fid2, '%s\n', files2.name);
fclose(fid2);

%for fdc list
fid3 = fopen('files_fdc_longit.txt', 'w');
files3 = dir('template/fdc_smooth_longit/*DPRC*');
fprintf(fid3, '%s\n', files3.name);
fclose(fid3);


%Run statistical tests for each FBA metric
unix(['fixelcfestats template/fd_smooth_longit/ files_fd_longit.txt stats_matrices/design_matrix_group_diff.txt stats_matrices/contrast_matrix_group_diff.txt template/matrix/ stats_fd_longit/']);
unix(['fixelcfestats template/log_fc_smooth_longit/ files_log_fc_longit.txt stats_matrices/design_matrix_group_diff.txt stats_matrices/contrast_matrix_group_diff.txt template/matrix/ stats_log_fc_longitx/']);
unix(['fixelcfestats template/fdc_smooth_longit/ files_fdc_longit.txt stats_matrices/design_matrix_group_diff.txt stats_matrices/contrast_matrix_group_diff.txt template/matrix/ stats_fdc_longit/']);

