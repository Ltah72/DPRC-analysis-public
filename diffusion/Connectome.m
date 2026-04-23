%This script will create a connectome to represent the number of
%streamlines connecting different parts of the brain. Preprocessing is done 
%via fmriprep (this should be done as the first step of this pipeline. This 
%script also assumes that the user has ran the previous pipelines 
%(preprocessing and constrained spherical deconvolution (CSD)), and it will 
%use that processed data for this pipeline. The tractography inputs for 
%this will be with using anatomically constrained tractography (ACT) and 
%spherical-deconvolution informed filtering (SIFT). The brain will be 
%parcellated into different regions or nodes using an atlas, as specified 
%in the code below and in the manual. 

%See MRtrix documentation: 
%https://mrtrix.readthedocs.io/en/latest/quantitative_structural_connectivity/structural_connectome.html
%Also, the B.A.T.M.A.N. tutorial is also good: https://osf.io/fkyht/overview

% Steps: 
% 1. Preprocess anatomical images (T1w and T2 FLAIR images) 
%   a. Gibbs ringing correction (mrdegibbs)
%   b. Run fmriprep to preprocess anat + func images
% 2. Create a 5tt image for ACT
%   a. Co-registration t1w and t2 FLAIR to dwi image
%   b. Generate a '4tt image'
%   c. Edit in a pathological image to create a '5ttimage'
%   d. Create a 'seed' boundary between the white and grey matter for
%   streamline generation
% 2. Create streamlines for tractography with ACT
% 3. Refine streamlines with SIFT
% 4. Parcellate brain regions/nodes with an atlas (e.g. FreeSurfer)
% 5. Create the connectome
%   Option 1: Create an 84 x 84 connectome (simple)
%       a. Convert the labels to MRtrix format
%       b. Co-register the parcellation to the grey matter boundary
%       c. Create the whole-brain connectome
%   Option 2: Use HCP MMP 1.0 atlas - 379 x 379 connectome
%       a. Map annotation file of HCP MMP atlas from fsaverage to your subject
%       b. Map HCP MMP annotations onto volumetric image and add
%       subcortical segmentation
%       c. Replace random integers of hcpmmp1 file with integers that start
%       at and increase by 1
%       d. Register the ordered atlas-based volumetric parcellation to
%       diffusion space
%       e. Create the whole-brain connectome
% 6. Select a specific tract between regions from the connectome
% 7. Perform group-wise statistics of the connectome using non-parametric 
% permutation testing
%       a. On whole-brain connectome (all nodes)
%       b. On nodes / connections of interest
% 8. Options for connectome visualisation
%       a. Make the connectome appear more ‘anatomical’
%       b.	Check over connectome files

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz or Ltahara2@gmail.com
%Date: 20/12/20


clc;
clear all;
close all;

%define/add pathways
derivdir = input('Please enter derivatives directory:', 's');

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');

%Define fmriprep directory, so that it may be used:
FmriprepDirectory = input('Please enter fmriprep directory:', 's');

%should be the same groupname from what the user analysed in the previous scripts (e.g. CSD.m).  
groupname = input('Which pre-processed group / study do you want to continue to analyse?: ', 's');

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%Define your sourcedata directory:
sourcedataDir = input('Please enter sourcedata directory:', 's');
%e.g., sourcedataDir = (['/data/sourcedata/' period]);

%Define preprocessed dwi directory (e.g. CSD, FBA) for preprocessed dwi
%files:
DWIPreprocDir = ([derivdir, '/groups/' period, '/diff_data/' groupname, '/IN/']); 

%Define connectome directory, where your output files will go to.
ConnectomeDir = input('Please enter connectome directory:', 's');
%e.g., ConnectomeDir = ([derivdir, '/groups/' period '/diff_data/' groupname '/connectome/']);

%create a connectome folder to place all of the data and analysis in
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/connectome/']);

%make directory to hold your matrices (e.g. design and contrast)for statistical tests - for connectome analysis. 
%You need to manually create and store the files in here.
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/connectome/stats_matrices/']);

%make directories to hold the statistical tests for the whole and interested connectome networks
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/connectome/outputWhole_connectome_stats/']);
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/connectome/outputFPN_stats/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(derivdir));
addpath(genpath(ScriptDirectory));

%go into the dwi participant analysed folder + choose participants
cd(DWIPreprocDir);

%define variables
msgfig = 'Choose participants for analysis (all should be included from this directory.)';
uiwait(msgbox(msgfig));
participants = uipickfiles;
datafile = '_acq_data_dwi';

%create stat matrices for analysis - can automate this or can be manual
warningMessage = 'Automaticity of study matrices for running stats have only been customised to this script''s creator (e.g 5 groups - HC, SCD, aMCI, mMCI, AD). If you have your own group design, then you will have to manually create your matrices.';
uiwait(warndlg(warningMessage));
UserAutomate = input('Do you want to automate your matrice creation? y or n: ', 's');
if UserAutomate == 'y'
    excelFile = input('Please name the excel file you wish to use: ', 's');
    fileLocation = input(['Where is this file, ' excelFile  ', located? Name the directory: '], 's');
    CreateStudyMatrices(excelFile, fileLocation, derivdir, groupname);
elseif UserAutomate == 'n'
    UserMatrices = input('Have you created your own matrices? y or n: ', 's');
    if UserMatrices == 'y'
        disp('Make sure you have already placed your matrix files into the stats_matrices folder.');
    elseif UserMatrices == 'n'
        error('Pipeline stopped; please first create you matrix files and place into the stats_matrices folder.');
    end
end

%create 5ttImageCheck text file w/ header line and put into quality control (qc)
cd([derivdir '/groups/' period, '/diff_data/', groupname, '/qc/']);
fid4 = fopen('5ttImageCheck.txt', 'w');
if (fid4 == -1)
    disp('Error in creating the text file.')
else
    fprintf(fid4, '%s     %s', 'Participant', '5ttImage_status');
    fclose(fid4);
end

%go into the connectome folder
cd(ConnectomeDir);


for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i}); 
 
    %copy fmriprep FreeSurfer's recon-all output into FreeSurfer's $SUBJECTS directory
    unix(['cp -r ' derivdir, '/fmriprepped_data/derivatives/freesurfer/', PAR_NAME, '/ $SUBJECTS_DIR']);

    %---------------------------------------------------------------------%
    %Step 1: Apply Gibbs ringing and Bias field correction on anat images
    
    %Acquire fmriprepped, Gibbs-corrected T1w anat image from fmriprep directory
    unix(['mrconvert ' FmriprepDir, 'derivatives/fmriprep/' PAR_NAME, '/anat/' PAR_NAME, '_desc-preproc_T1w.nii.gz ' PAR_NAME, '_T1w.nii']);
    
    %Have to apply bias field correction on Gibbs-corrected T2 FLAIR using BET and ANTs - N4 algorithm
    %get brain mask from FLAIR
    unix(['bet ' FmriprepDir, 'sourcedata/' PAR_NAME, '/anat/' PAR_NAME, '_FLAIR.nii bet_' PAR_NAME, '_FLAIR.nii -m -f 0.2']);
    %bias field correct directly with ANTs programme
    unix(['N4BiasFieldCorrection -d 3 -x bet_' PAR_NAME, '_FLAIR_mask.nii.gz -i ' FmriprepDir, 'sourcedata/' PAR_NAME, '/anat/' PAR_NAME, '_FLAIR.nii -o ' PAR_NAME '_bfc_FLAIR.nii']);
    
    %---------------------------------------------------------------------%
    %Step 2: Create a 5tt image for ACT
    %a) co-registration of t1w and t2 FLAIR to dwi image through fsl FLIRT
    %convert to nii format to run fsl's FLIRT - use the best B0 volume,
    %which will be the first vol in the dwi sequence (vol 0).
    unix(['mrconvert -coord 3 0 ', DWIPreprocDir, 'preprocessed_dwi/' PAR_NAME, datafile,'.mif ref_b0_', PAR_NAME, '.nii']);
    
    %linear registration with 6 dof and the transformation matrix output
    unix(['flirt -in ', PAR_NAME, '_T1w.nii -ref ref_b0_', PAR_NAME, '.nii -dof 6 -out t1_flirt_' PAR_NAME, '.nii -omat transform_flirt_t12dwi_' PAR_NAME '.mat']);
    unix(['flirt -in ', PAR_NAME, '_bfc_FLAIR.nii -ref ref_b0_', PAR_NAME, '.nii -dof 6 -out t2_flirt_' PAR_NAME, '.nii -omat transform_flirt_t22dwi_' PAR_NAME '.mat']);
    
    %apply the linear transformation to the t1 and t2 image
    unix(['transformconvert transform_flirt_t12dwi_' PAR_NAME '.mat ' PAR_NAME, '_T1w.nii ref_b0_', PAR_NAME, '.nii flirt_import transform_mrtrix_t12dwi_' PAR_NAME '.txt']);
    unix(['transformconvert transform_flirt_t22dwi_' PAR_NAME '.mat ' PAR_NAME, '_bfc_FLAIR.nii ref_b0_', PAR_NAME, '.nii flirt_import transform_mrtrix_t22dwi_' PAR_NAME '.txt']);
    unix(['mrtransform ' PAR_NAME, '_T1w.nii -linear transform_mrtrix_t12dwi_' PAR_NAME '.txt ' PAR_NAME, '_T1coreg.nii']);
    unix(['mrtransform ' PAR_NAME, '_bfc_FLAIR.nii -linear transform_mrtrix_t12dwi_' PAR_NAME '.txt -template ' PAR_NAME, '_T1coreg.nii ' PAR_NAME, '_T2coreg.nii']);
    
    %convert to .mif (mrtrix) format
    unix(['mrconvert ' PAR_NAME, '_T1coreg.nii ' PAR_NAME '_T1coreg.mif']);
    unix(['mrconvert ' PAR_NAME, '_T2coreg.nii ' PAR_NAME '_T2coreg.mif']);
    
    %b) generate 5tt image with upsampled brain mask, t1, and t2 FLAIR (first format will be 4tt, and then 5tt after adding WMH image - see step below).
    unix(['5ttgen fsl -mask ' DWIPreprocDir '/brain_mask/' PAR_NAME, datafile, '.mif -t2 ' PAR_NAME, '_T2coreg.mif ' PAR_NAME '_T1coreg.mif 4ttimage_' PAR_NAME '.mif']);

    %c) edit in the pathological tissue (WMH) to the 5tt image:
    %convert WMH lesion mask to .mif file
    unix(['mrconvert ' derivdir '/WMH_lesion_masks/ples_lpa_mr', PAR_NAME, '_FLAIR.nii ples_lpa_mr', PAR_NAME, '_FLAIR.mif']);
    
    %reslice the WMH lesion mask to match the 4tt image
    unix(['mrtransform ples_lpa_mr' PAR_NAME, '_FLAIR.mif -template 4ttimage_' PAR_NAME, '.mif ' PAR_NAME, '_WMH_mask_transformed.mif']);
    
    %add the image into the 5tt image
    unix(['5ttedit -path ', PAR_NAME, '_WMH_mask_transformed.mif 4ttimage_' PAR_NAME, '.mif 5ttimage_' PAR_NAME '.mif']);
    
    %check that your 5ttimage conforms to MRtrix's expected format
    FiveTTImageCheck(PAR_NAME, derivdir, period, groupname);

    %d) Create a 'seed' boundary between the white and gray matter for
    %streamline generation
    unix(['5tt2gmwmi 5ttimage_' PAR_NAME '.mif gmwmSeed_mask_' PAR_NAME '.mif']);
   
    %---------------------------------------------------------------------%
    %Step 3: Generate streamline tracks with -act
    unix(['tckgen -act 5ttimage_' PAR_NAME '.mif -backtrack -seed_gmwmi gmwmSeed_mask_' PAR_NAME '.mif -maxlength 250 -cutoff 0.06 -select 10000000 ' DWIPreprocDir, 'wmfod_norm_' PAR_NAME '.mif tracks_10M_' PAR_NAME '.tck']);
    
    %OPTIONAL - if you want to view the tracks, make a smaller version of them:
    %unix(['tckedit tracks_10M_' PAR_NAME '.tck -number 200k smallerTracks_200k_' PAR_NAME '.tck']);
    
    %---------------------------------------------------------------------%
    %Step 4: Refine streamlines with tcksift2
    % Note that this will NOT reduce streamlines to 1 mil, but will optimise the streamlines to a cross-sectional area multiplier to match the whole-brain tractogram to the fixel-wise fibre densities (e.g., tcksift2 algorithm)
    unix(['tcksift2 -act 5ttimage_' PAR_NAME '.mif -out_mu sift_mu_' PAR_NAME '.txt -out_coeffs sift_coeffs_' PAR_NAME '.txt tracks_10M_' PAR_NAME '.tck ' DWIPreprocDir, 'wmfod_norm_' PAR_NAME '.mif sift_1M_' PAR_NAME '.txt']);
    
    %OPTIONAL: tcksift2 with -min_factor option (to set threshold of streamline weights to 0.3)
    %unix(['tcksift2 -act 5ttimage_' PAR_NAME '.mif -min_factor 0.3 -out_mu sift_mu_thr_' PAR_NAME '.txt -out_coeffs sift_coeffs_thr_' PAR_NAME '.txt tracks_10M_' PAR_NAME '.tck ' DWIPreprocDir, 'wmfod_norm_' PAR_NAME '.mif sift_1M_thr_' PAR_NAME '.txt']);
    
    %Also, generate the .tck 1M SIFT file, used for connectome visualisation later on 
    unix(['tcksift -act 5ttimage_' PAR_NAME '.mif -term_number 1000000 tracks_10M_' PAR_NAME '.tck ' DWIPreprocDir 'wmfod_norm_' PAR_NAME '.mif sift_1M_' PAR_NAME '.tck']);

%     %OPTIONAL CHECK - Compare the tracks with and without SIFT with 10k tracks (for
%     %visualation only)
%     unix(['tckedit tracks_10M_' PAR_NAME '.tck -number 10k SuperSmallTracks_10k_' PAR_NAME '.tck']);
%     unix(['tckedit sift_1M_' PAR_NAME '.tck -number 10k SuperSmallTracks_10k_' PAR_NAME '_sift.tck']);

    %---------------------------------------------------------------------%
    %Step 5: Create the connectome
    %Option 1: Create a 'simple' connectome, using MRtrix default atlas - Desikan-Killiany (84 x 84)
    %a) Convert labels to mrtrix format
    %unix(['labelconvert ' PAR_NAME '/mri/aparc+aseg.mgz $FREESURFER_HOME/FreeSurferColorLUT.txt /SOFTWARE/anaconda3/share/mrtrix3/labelconvert/fs_default.txt ' PAR_NAME '_parcels.mif']);
    
    %b) Co-register the parcellation to the grey matter boundary
    %unix(['mrtransform ' PAR_NAME '_parcels.mif -interp nearest -linear transform_mrtrix_t12dwi.txt -inverse -datatype uint32 ' PAR_NAME '_parcels_coreg.mif']);
    
    %c) Create whole-brain connectome
    %unix(['tck2connectome -symmetric -zero_diagonal -scale_invnodevol -tck_weights_in sift_1M_' PAR_NAME '.txt tracks_10M_' PAR_NAME '.tck ' PAR_NAME '_parcels_coreg.mif ' PAR_NAME '_parcels_coreg.csv -out_assignment assignments_' PAR_NAME '_parcels_coreg.csv']);
    
%     %FOR MATLAB: visualise the connectome matrix in Matlab, with code below:
%     connectome = importdata([PAR_NAME '_parcels_coreg.csv']);
%     imagesc(connectome)


    %Option 2: use HCP MMP1 atlas (379 x 379):
    %a) map the annotation file of the HCP MMP 1.0 atlas from fsaverage to
    %your subject. Remember to do that for both hemispheres. 
    %unix(['mri_surf2surf --srcsubject fsaverage --trgsubject ' PAR_NAME ' --hemi lh --sval-annot $SUBJECTS_DIR/fsaverage/label/lh.hcpmmp1.annot --tval $SUBJECTS_DIR/' PAR_NAME '/label/lh.hcpmmp1.annot']);
    %unix(['mri_surf2surf --srcsubject fsaverage --trgsubject ' PAR_NAME ' --hemi rh --sval-annot $SUBJECTS_DIR/fsaverage/label/rh.hcpmmp1.annot --tval $SUBJECTS_DIR/' PAR_NAME '/label/rh.hcpmmp1.annot']);
    
    %b) map the HCP MMP 1.0 annotations onto the volumetric image and add
    %(FreeSurfer-specific) subcortical segmentation. Convert the resulting
    %file to .mif format (use datatype uint32, which is liked best by
    %MRtrix). 
    %unix(['mri_aparc2aseg --old-ribbon --s ' PAR_NAME ' --annot hcpmmp1 --o ' PAR_NAME '_hcpmmp1.mgz']);
    %unix(['mrconvert -datatype uint32 ' PAR_NAME '_hcpmmp1.mgz ' PAR_NAME '_hcpmmp1.mif -force']);
    
    %c) Replace the random integers of the hcpmmp1.mif file with integers that
    %start at 1 and increase by 1. 
    %unix(['labelconvert ' PAR_NAME '_hcpmmp1.mif /SOFTWARE/anaconda3/share/mrtrix3/labelconvert/hcpmmp1_original.txt /SOFTWARE/anaconda3/share/mrtrix3/labelconvert/hcpmmp1_ordered.txt ' PAR_NAME '_hcpmmp1_parcels_nocoreg.mif -force']);
   
    %d) Register the ordered atlas-based volumetric parcellation to diffusion
    %space. 
    %unix(['mrtransform ' PAR_NAME '_hcpmmp1_parcels_nocoreg.mif -linear transform_mrtrix_t12dwi_' PAR_NAME '.txt -inverse -datatype uint32 ' PAR_NAME '_hcpmmp1_parcels_coreg.mif -force']);
    
    %e) Create the whole-brain connectome: 
    %unix(['tck2connectome -symmetric -zero_diagonal -scale_invnodevol sift_1M_' PAR_NAME '.tck ' PAR_NAME '_hcpmmp1_parcels_coreg.mif hcpmmp1_' PAR_NAME '.csv -out_assignment assignments_hcpmmp1_' PAR_NAME '.csv -force']);
    
    %try with weights (but will give you too 'bright' of a connectome image):
    %unix(['tck2connectome -symmetric -zero_diagonal -scale_invnodevol -tck_weights_in sift_1M_' PAR_NAME '.txt tracks_10M_' PAR_NAME '.tck ' PAR_NAME '_hcpmmp1_parcels_coreg.mif hcpmmp1w_' PAR_NAME '.csv -out_assignment assignments_hcpmmp1w_' PAR_NAME '.csv -force']);

end 


%OPTIONAL - selecting a connectome of interest, e.g., the frontal parietal network from the HCP atlas
%for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i}); 
    %---------------------------------------------------------------------%
    %Step 6: Select a specfic tract between specific regions from the connectome
    
    %for the whole FPN (complete) 
    %unix(['connectome2tck -nodes 73,253,67,247,97,277,98,278,26,206,70,250,71,251,87,267,68,248,83,263,85,265,84,264,86,266,40,220,41,221,55,235,44,224,43,223,36,216,39,219,37,217,48,228,95,275,49,229,117,297,50,230,47,227,42,222,45,225,46,226,29,209,143,323,151,331,150,330,149,329,148,328,116,296,147,327,146,326,145,325,144,324 -files single -exclusive sift_1M_' PAR_NAME '.tck assignments_hcpmmp1_' PAR_NAME '.csv ' PAR_NAME '_whole-FPN']);
    
    %for the whole FPN (complete) with weights: 
    %unix(['connectome2tck -nodes 73,253,67,247,97,277,98,278,26,206,70,250,71,251,87,267,68,248,83,263,85,265,84,264,86,266,40,220,41,221,55,235,44,224,43,223,36,216,39,219,37,217,48,228,95,275,49,229,117,297,50,230,47,227,42,222,45,225,46,226,29,209,143,323,151,331,150,330,149,329,148,328,116,296,147,327,146,326,145,325,144,324 -files single -exclusive tracks_10M_' PAR_NAME '.tck -tck_weights_in sift_1M_' PAR_NAME '.txt assignments_hcpmmp1w_' PAR_NAME '.csv ' PAR_NAME '_whole-FPN_w']);    
    
%end 


    %---------------------------------------------------------------------%
    %Step 7: Perform group-wise statistics of the connectome using
    %non-parametric permuation testing
    
    %write participants to file
    InputConnectomeFileList(participants);
    
    %run connectome stats
    unix(['connectomestats ConnectomeInput.txt tfnbs stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt stats_results/ConnectomeWhole_stats/outputWhole_connectome_']);
   
    %create connectome template to visualise group differences. There is a function here, but it is formatted for HCP atlas. Can modify the function and/or make your own template.  
    %CreateConnectomeTemplate(participants);
    
    %OPTIONAL - connectome of interest permuatation analysis. For a subnetwork, like the FPN, use vectorstats: 
    %unix(['vectorstats -strong FPN_small-node_Input.txt stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt stats_results/FPN_stats/strong/smallNode/small-node_FPN_stats-']);
    %unix(['vectorstats -strong FPN_big-node_Input.txt stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt stats_results/FPN_stats/strong/bigNode/big-node_FPN_stats-']);
   
