%Conduct track-weighted functional connectivity (TW-FC) - combine a
%structural dwi data with a functional resting-state fMRI data, with a
%specific network (e.g. Frontoparietal Network (FPN). Based on these
%papers: 
%Calamante, F., Masterton, R.A.J., Tournierm J.D., Smith, R.E., Willats,
%L., Raffelt, D., & Connelly, A. (2013). Track-weighted functional
%connectivity (TW-FC): A tool for characterizing the structural-functional
%connections in the brain. 
%Calamante, F., Smith, R.E., Liang, X., Zalesky, A., & Connelly, A. (2017).
%Track-weighted dynamic functional connectivity (TW-dFC): a new method to
%study time-resolved functional connectivity. 

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 19/09/2021

clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%ask user for what kind of group/study analysis they will conduct (e.g.
%cross-sectional, F0s, longitudinal, F0 vs. F2, etc.):
groupname = input('Please name the group/study analysis: ', 's');

%Choose directory where the diffusion data is (same level as sourcedata
%folder).
derivdir = input('Please enter derivatives directory:', 's');
%shortcut for debugging purposes:
%e.g., derivdir = '/data/USERS/LENORE/derivatives';

%Define script directory, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc';

%Define connectome directory, where most of the pre-required files lie.
ConnectomeDir = input('Please enter connectome directory:', 's');
%e.g., ConnectomeDir = ([derivdir, '/groups/' period '/diff_data/' groupname '/connectome/']);

%Define fmriprep derivatives directory.
FmriprepDerDir = input('Please enter fmriprep derivatives directory:', 's');
%e.g., FmriprepDerDir = ([derivdir, '/fmriprepped_data/derivatives/fmriprep/']);

%Define nuisance regressor (NR) removed derivatives directory.
NRDir = input('Please enter func denoised derivatives directory:', 's');
%e.g., NRDir = ([derivdir '/fMRI_denoised/']);

%If using RSN online maps, define directory: 
RSN-onlineDir = input('Please enter your downloaded RSN directory:', 's');
%e.g., RSN_onlineDir = ('/SOFTWARE/online_brain_templates/HCP_TFM-RSN/');

%Create TW-FC directory for holding output data
mkdir([derivdir,'/groups/' period '/diff_data/', groupname, '/TW-FC/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(derivdir));
addpath(genpath(ScriptDirectory));

%go into your sourcedata folder, where all participant files will be
%there.
cd(['/data/sourcedata/' period]);

%choose participants to run TW-dFC
TWFC_participants = uipickfiles;

%go back into your derivatives folder.
cd([derivdir '/groups/' period '/diff_data/', groupname, '/TW-FC/']);

%copy any necessary files over to the TW-FC directory:
%copy the wmfod template file
copyfile ([derivdir '/groups/' period, '/diff_data/' groupname, '/template/wmfod_template.mif'], [derivdir,'/groups/' period, '/diff_data/', groupname, '/TW-FC/']);


for i = 1:length(TWFC_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(TWFC_participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
    
    %1. Co-register t1w dwi image through fsl FLIRT
    %a) Linear registration with 6 dof and the transformation matrix output
    unix(['flirt -in ', FmriprepDerDir, PAR_NAME, '/anat/' PAR_NAME, '_space-MNI152NLin6Asym_desc-preproc_T1w.nii.gz -ref ' ConnectomeDir, 'ref_b0_', PAR_NAME, '.nii -dof 6 -out t1_flirt_' PAR_NAME, '.nii -omat transform_flirt_t12dwi_' PAR_NAME '.mat']);
    %b) Apply the linear transformation to the t1 and t2 image
    unix(['transformconvert transform_flirt_t12dwi_' PAR_NAME '.mat ' FmriprepDerDir, PAR_NAME, '/anat/' PAR_NAME, '_space-MNI152NLin6Asym_desc-preproc_T1w.nii.gz ' ConnectomeDir, 'ref_b0_', PAR_NAME, '.nii flirt_import transform_mrtrix_t12dwi_' PAR_NAME '.txt -force']);
    unix(['mrtransform ' FmriprepDerDir, PAR_NAME, '/anat/' PAR_NAME, '_space-MNI152NLin6Asym_desc-preproc_T1w.nii.gz -linear transform_mrtrix_t12dwi_' PAR_NAME '.txt ' PAR_NAME, '_T1coreg-MNI.nii -force']);
    %c) Convert the co-registered T1w-dwi nifti to .mif file
    unix(['mrconvert ' PAR_NAME, '_T1coreg-MNI.nii ' PAR_NAME '_T1coreg-MNI.mif -force']);
    
    %2. Co-register fMRI-dwi file
    %a) Create transformation matrix
    unix(['flirt -in ' FmriprepDerDir, PAR_NAME '/func/' PAR_NAME '_task-rest_run-1_space-MNI152NLin6Asym_boldref.nii.gz -ref ' ConnectomeDir 'ref_b0_' PAR_NAME '.nii -out flirt_bold_' PAR_NAME '.nii -omat bold2b0_' PAR_NAME '.mat']);
    %b) Generate coregistered image (fmri main bold sequence coregistered to B0):
    unix(['flirt -in ' NRDir, PAR_NAME '_task-rest_run-1_space-MNI152NLin6Asym_desc-smoothAROMAnonaggr_bold_NR.nii.gz -applyxfm -init bold2b0_' PAR_NAME '.mat -paddingsize 0.0 -interp trilinear -ref ' ConnectomeDir 'ref_b0_' PAR_NAME '.nii -out bold_coreg-MNI_' PAR_NAME '.nii.gz']);
    %unix(['flirt -in ' FmriprepDerDir, PAR_NAME '/func/' PAR_NAME '_task-rest_run-1_space-MNI152NLin6Asym_desc-smoothAROMAnonaggr_bold.nii.gz -applyxfm -init bold2b0_' PAR_NAME '.mat -paddingsize 0.0 -interp trilinear -ref ' ConnectomeDir 'ref_b0_' PAR_NAME '.nii -out bold_coreg-MNI_' PAR_NAME '.nii.gz']);
    %c) Convert the co-registered BOLD-dwi nifti to .mif file
    unix(['mrconvert bold_coreg-MNI_' PAR_NAME '.nii.gz bold_coreg-MNI_' PAR_NAME '.mif -force']);
    
    %3. Warp individual track files to group template
    %create template to subject warp (use non-linear transformation)
    %co-register subject and template
    unix(['mrregister ' ConnectomeDir 'wmfod_norm_' PAR_NAME '.mif wmfod_template.mif -nl_warp_full warp_full_' PAR_NAME '.mif -force']);
    %create template to subject warp
    unix(['warpconvert warp_full_' PAR_NAME '.mif warpfull2deformation w_t2s_' PAR_NAME '.mif -from 2 -template ' ConnectomeDir 'wmfod_norm_' PAR_NAME '.mif -force']);
    % warp subject tck to template space
    unix(['tcktransform ' ConnectomeDir 'sift_1M_' PAR_NAME '.tck w_t2s_' PAR_NAME '.mif sift_1M_subject_at_template_' PAR_NAME '.tck -force']);
    
    
    %4. %---------------------TW-FC with specific network (FPN)---------------------%
    %Option 1: %%%%%% Using group ICA %%%%%%%
    %convert group ICA output to .mif format
    unix(['mrconvert ' derivdir '/fMRI_groupICA/groupICALongitudinal/melodic_IC.nii.gz melodic_IC.mif']);
    %extract map from group ICA (IC 05 - left FPN)
    unix(['mrconvert melodic_IC.mif -coord 3 5 FC_map-IC05.mif']);
    
    %Initial TWI generation:
    unix(['tckmap sift_1M_subject_at_template_' PAR_NAME '.tck temp_' PAR_NAME '.mif -template ' PAR_NAME '_T1coreg-MNI.mif -contrast scalar_map -image FC_map-IC05.mif -stat_vox mean -stat_tck sum']);
    %Deriving the mask (voxels with at least 5 streamlines with non-zero TW
    %values):
    unix(['tckmap sift_1M_subject_at_template_' PAR_NAME '.tck - -template temp_' PAR_NAME '.mif -contrast scalar_map_count -image FC_map-IC05.mif | mrcalc - 5 -ge mask_' PAR_NAME '.mif -datatype bit']);
    %Apply the mask:
    unix(['mrcalc temp_' PAR_NAME '.mif mask_' PAR_NAME '.mif -mult TWFC_study-cohort-ICA' PAR_NAME '.mif']);
    
    %Option 2: %%%%%% Using RSN template mask %%%%%%%
    %Using the HCP RSN (https://www.fmrib.ox.ac.uk/datasets/TFMs/)
    %convert group RSN maps output to .mif format
    unix(['mrconvert ' RSN_onlineDir 'sICA22_RSN21.nii sICA22_RSN21.mif']);
    %extract map from group RSN (IC 13 - right FPN, IC 14 - left FPN)
    unix(['mrconvert sICA22_RSN21.mif -coord 3 13 rFPN_FCmap-IC13.mif']);
    unix(['mrconvert sICA22_RSN21.mif -coord 3 14 lFPN_FCmap-IC14.mif']);
    %combine the left & right FPN into one map:
    unix(['mrcalc rFPN_FCmap-IC13.mif lFPN_FCmap-IC14.mif -max FPN_FCmap_RSN.mif']);
   
    %Initial TWI generation:
    unix(['tckmap sift_1M_subject_at_template_' PAR_NAME '.tck temp_' PAR_NAME '.mif -template ' PAR_NAME '_T1coreg-MNI.mif -contrast scalar_map -image FPN_FCmap_RSN.mif -stat_vox mean -stat_tck sum']);
    %Deriving the mask (voxels with at least 5 streamlines with non-zero TW
    %values):
    unix(['tckmap sift_1M_subject_at_template_' PAR_NAME '.tck - -template temp_' PAR_NAME '.mif -contrast scalar_map_count -image FPN_FCmap_RSN.mif | mrcalc - 5 -ge mask_' PAR_NAME '.mif -datatype bit']);
    %Apply the mask:
    unix(['mrcalc temp_' PAR_NAME '.mif mask_' PAR_NAME '.mif -mult TWFC_onlineRSN_' PAR_NAME '.mif']);
   
    
    %5. %-------------------TW-dFC without specific network (whole-brain dynamic)---------------------%
    
    %a) Create the track-weighted dynamic functional connectivity (TW-dFC) per each individual
    unix(['tckdfc -dynamic rectangle 81 sift_1M_subject_at_template_' PAR_NAME '.tck -template ' PAR_NAME '_T1coreg-MNI.mif bold_coreg-MNI_' PAR_NAME '.mif tdfc_dynamic_' PAR_NAME '.mif']);
    %b) Create summary image per each individual - for 95% confidence interval
    %calculate average image
    unix(['mrmath tdfc_dynamic_' PAR_NAME '.mif mean tdfc_average_' PAR_NAME '.mif -axis 3']);
    %calculate STD (voxels across volumes)
    unix(['mrmath tdfc_dynamic_' PAR_NAME '.mif std tdfc_std_' PAR_NAME '.mif -axis 3']);
    %divide STD image by square root of n to get the SE
    unix(['mrcalc tdfc_std_' PAR_NAME '.mif 490 -sqrt -divide tdfc_SE_' PAR_NAME '.mif']);
    %multiply +1.96 (95% CI) to the SE image (upper margin of error)
    unix(['mrcalc tdfc_SE_' PAR_NAME '.mif 1.96 -mult tdfc_upper_ME_' PAR_NAME '.mif']);
    %add the mean to the upper margin of error to get the upper bound
    unix(['mrcalc tdfc_average_' PAR_NAME '.mif tdfc_upper_ME_' PAR_NAME '.mif -add tdfc_upper_bound_' PAR_NAME '.mif']);
    
    %do the same steps, but this time for the lower bound (-1.96)
    %multiply -1.96 (95% CI) to the SE image (lower margin of error)
    unix(['mrcalc tdfc_SE_' PAR_NAME '.mif 1.96 -neg -mult tdfc_lower_ME_' PAR_NAME '.mif']); %works
    %add the mean to the lower margin of error to get the lower bound
    unix(['mrcalc tdfc_average_' PAR_NAME '.mif tdfc_lower_ME_' PAR_NAME '.mif -add tdfc_lower_bound_' PAR_NAME '.mif']);
    %subtract upper bound - lower bound to get the CI 95%
    unix(['mrcalc tdfc_upper_bound_' PAR_NAME '.mif tdfc_lower_bound_' PAR_NAME '.mif -subtract tdfc_CI95_' PAR_NAME '.mif']);
    
end

%6. Run permutations on data for group comparisons: 

%for TW-FC specific network:
unix(['mrclusterstats TWFCInput.txt stats/mrclusterstats/design_matrix_CvAD.txt stats/mrclusterstats/contrast_matrix_CvAD.txt template2_regrid_tdfc_mask.mif stats/mrclusterstats/output_CvAD']);

%for TW-FC without a specific network:
unix(['mrclusterstats TWFCInput.txt stats/mrclusterstats/design_matrix_CvAD.txt stats/mrclusterstats/contrast_matrix_CvAD.txt template2_regrid_tdfc_mask.mif stats/mrclusterstats/output_CvAD']);








%Calculate group mean TW-dFC template from the normalised individual TW-dFC maps
%change zeros into NaNs
unix(['mrgrid tdfc_dynamic_' PAR_NAME '.mif crop -mask upsampled_mask_mod_' PAR_NAME, '.mif -fill nan cropped2_tdfc_dynamic_low-res-2.1_win81_' PAR_NAME '.mif']); % ~ 1.02 GB -- half the size of its uncropped version!

%4D image
unix(['mrmath tdfc_dynamic*.mif mean average_tdfc_dynamic.mif']);
%3D image
unix(['mrmath tdfc_CI95*.mif mean average_tdfc_CI95.mif']);

unix(['mrcalc tdfc_CI95*.mif -add 2 -div average_tdfc_CI95_mrcalc.mif']); %same as mrmath's mean

unix(['mrmath tdfc_CI95*.mif sum sum_tdfc_CI95.mif']); %add









