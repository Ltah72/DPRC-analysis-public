%Create white matter hyperintensity lesion masks using the lesion
%prediction algorithm (LPA) in the lssion segmentation tool (LST) in SPM12.
%Inputs are the FLAIR image (required) and the T1w image (optional) as the
%referene image. You can read more about it below, with the citation. 

%https://www.applied-statistics.de/lst.html
%Schmidt, P., & Wink, L. (2019). LST : A lesion segmentation tool for SPM. 

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 17/07/21


%function format: Vf2 is the FLAIR image, Vref is the reference image (T1w
%anatomical image).
%ps_LST_lpa(Vf2, Vref, html)


clc;
clear all;
close all;


%define/add pathways

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

sourcedatadir = input('Please enter source data directory:', 's');
%e.g., sourcedatadir = ['/data/sourcedata/', period '/'];

startdir = input('Please enter data directory:', 's');
%e.g., startdir = '/data/USERS/LENORE';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/diffusion';

output_WMH_dir = input('Please enter your output WMH directory:', 's');
%e.g., output_WMH_dir = [startdir '/WMH_lesion_masks'];

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(startdir));
addpath(genpath(ScriptDirectory));

%add path to the spm and LST directories
addpath(genpath('/SOFTWARE/MATLAB/spm12/'));
addpath(genpath('/SOFTWARE/MATLAB/spm12/toolbox/LST'));

%go into your sourcedata file 
cd(sourcedatadir);

%define participants
participants = uipickfiles;

%Run LPA on your participants (using the FLAIR + T1w image as inputs) 
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
    
    %define your inputs (FLAIR and reference image)     
    inputFLAIR = [sourcedatadir, PAR_NAME, '/anat/' PAR_NAME '_FLAIR.nii'];
    RefT1w = [sourcedatadir, PAR_NAME, '/anat/' PAR_NAME '_T1w.nii'];
    
    %run LPA function
    ps_LST_lpa(inputFLAIR, RefT1w)

    %go into the folder with the output files
    cd ([sourcedatadir, PAR_NAME '/anat']);
    
    %move output files into the WMH output folder
    movefile(['LST_lpa_mr' PAR_NAME '_FLAIR'], [output_WMH_dir]); 
    movefile(['LST_lpa_mr' PAR_NAME '_FLAIR.mat'], [output_WMH_dir]); 
    movefile(['mr' PAR_NAME '_FLAIR.nii'], [output_WMH_dir]); 
    movefile(['ples_lpa_mr' PAR_NAME '_FLAIR.nii'], [output_WMH_dir]);
    movefile(['report_LST_lpa_mr' PAR_NAME '_FLAIR.html'], [output_WMH_dir]); 
    
    %go back to sourcedatadir
    cd(sourcedatadir);

end 
