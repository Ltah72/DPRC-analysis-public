%Preprocessing script for DPRC diffusion data. Preprocessing is based upon
%MRtrix recommended pre-processing steps:
%   https://mrtrix.readthedocs.io/en/0.3.16/workflows/DWI_preprocessing_for_quantitative_analysis.html
%   https://mrtrix.readthedocs.io/en/latest/fixel_based_analysis/mt_fibre_density_cross-section.html
%And in working with UKB data:
%   Maximov & Westlye (2019).Towards an optimised processing pipeline for diffusion magnetic resonance imaging data: Effects of artefact corrections on diffusion metrics and their age associations in UK Biobank.
%   Alfaro-Almagro et al., (2018). Image processing and Quality Control for the first 10,000 brain imaging datasets from UK Biobank

%All preprocessing steps are deployed through the MRtrix3 programme.
%Utilised neuroimaging programs are FSL, MRtrix3, and ANTs are for
%these steps. Assumes BIDS formatting and organisation. This script calls
%upon 6 preprocessing steps (as functions) which are, in order:

%Steps:                                         Methodology - programme, algorithm/model, author

%0. 'Pre-steps'                                 (organise files/directories, define variaiables, etc.)
%1. Noise correction                            (denoising -- MP-PCA, Veraart et al., 2016)
%2. Gibbs ringing correction                    (local sub-voxel shift, Kellner et al., 2016)
% OPTIONAL - edit/reorganising files            (DPRC specific dwi volumes edit for UKB protocol)
%3. Field distortion                            (TOPUP -- FSL, Andersson et al., 2003; Smith, 2004)
%  a)Best B0 pair selection                     (BestB0 – in-house function, Alfaro-Almagro, et al., 2018)
%4  Estimate a brain mask                       (BET -- FSL, Smith, 2002)
%5. Eddy current distortions                    (Eddy -- FSL, Andersson & Sotiropoulos, 2016)
%  a) Run eddy quality control                  (eddy_quad -- FSL, Bastiani et al., 2019)
%6. Bias field correction                       (ANTs -- N4BiasFieldCorrection, Tustison et al., 2010)
%  (run twice)                                  (dwi2mask)
%o  Perform group motion (eddy) qc              (eddy_squad -- FSL, Bastiani et al., 2019) 

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 30/06/20

clc;
clear all;
close all;

%Choose directory where the diffusion data is (same level as sourcedata
%folder).
startdir = input('Please enter data directory:', 's');

%Define script directory, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');

%go to the directory where your scripts are.
cd(ScriptDirectory);

%ask user for what kind of group/study analysis they will conduct (e.g. 
%cross-sectional, F0s, longitudinal, F0 vs. F2, etc.):
groupname = input('Please name the group/study analysis: ', 's');

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%ask user if they want to do slice-to-volume motion correction
%(mp_order), with eddy CUDA/GPU requirement?
SlicetoVol_Corr = input('Do you want to perform slice-to-vol motion correction (mp_order)? *Requires eddy cuda and GPU. y or n: ', 's');

%make directories
mkdir([startdir,'/derivatives/' period '/diff_data/', groupname, '/preprocessed_dwi/']);
mkdir([startdir,'/derivatives/' period '/diff_data/', groupname, '/brain_mask/']);
mkdir([startdir,'/derivatives/' period '/diff_data/dwiqc/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(startdir));
addpath(genpath(ScriptDirectory));

%define dwi datafile
datafile = '_acq_data_dwi';

%create the qc text files in the dwiqc directory
QC_CreateFiles(startdir, period);

%go into your sourcedata folder, where all participant files will be
%there.
cd(['/data/sourcedata/' period]);

%choose participants you want to include in the group.
msgfig = 'Please choose participants for analysis.';
uiwait(msgbox(msgfig));
participants = uipickfiles;

%Preprocessing starts here:
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    full_path = ([upper_path '/' PAR_NAME '/']);
    source_dwi = (['/data/sourcedata/', period, '/' PAR_NAME, '/dwi/']);
    
    %create derivatives folder per each participant
    mkdir([startdir,'/derivatives/' period, '/diff_data/', PAR_NAME, '/dwi/']);
    
    %copy data from source folder into participants derivatives folder
    copyfile ([source_dwi, '*'], [startdir,'/derivatives/' period, '/diff_data/', PAR_NAME, '/dwi/']);
    
    %move into participants folder
    cd([startdir '/derivatives/' period, '/diff_data/' PAR_NAME, '/dwi/']);
    
    %count number of BDs that the participant has.
    BDs = dir([PAR_NAME '*BD*.nii']);
    
    %combine all volumes together (main, BUs, BDs) for denoising and Gibbs
    %ringing correction.
    if length(BDs) == 1
        NumBDs = 1;
        unix(['mrcat ' PAR_NAME, datafile, '.nii ', PAR_NAME, '_acq_BU_dwi.nii ', PAR_NAME, '_acq_BD1_dwi.nii ', 'combined_', PAR_NAME, datafile, '.nii']); %106 volumes total
    elseif length(BDs) == 2
        NumBDs = 2;
        unix(['mrcat ' PAR_NAME, datafile, '.nii ', PAR_NAME, '_acq_BU_dwi.nii ', PAR_NAME, '_acq_BD1_dwi.nii ', PAR_NAME, '_acq_BD2_dwi.nii ' 'combined_', PAR_NAME, datafile, '.nii']); %107 volumes total
    elseif length(BDs) == 3
        NumBDs = 3;
        unix(['mrcat ' PAR_NAME, datafile, '.nii ', PAR_NAME, '_acq_BU_dwi.nii ', PAR_NAME, '_acq_BD1_dwi.nii ', PAR_NAME, '_acq_BD2_dwi.nii ', PAR_NAME, '_acq_BD3_dwi.nii ', 'combined_', PAR_NAME, datafile, '.nii']); %108 volumes total
    end
    
    %convert nifti to to .mif format for mrtrix3 processing
    unix(['mrconvert combined_', PAR_NAME, datafile,'.nii combined_', PAR_NAME, datafile,'.mif']);
    
    %---------------------------------------------------------------------%
    %Step 1: Denoising
    unix(['dwidenoise combined_', PAR_NAME, datafile,'.mif d', PAR_NAME, datafile,'.mif -noise noise_' PAR_NAME, datafile,'.mif']);
    
    %create a copy in NIFTI format, in case you want to examine in fsleyes
    unix(['mrconvert d', PAR_NAME, datafile,'.mif d', PAR_NAME, datafile, '.nii']);
    unix(['mrconvert noise_', PAR_NAME, datafile,'.mif noise_', PAR_NAME, datafile, '.nii']);
    
    %eyeball your residuals as part of quality control; compare raw data
    %with the denoised data. If denoising did a good job, there should be
    %little or no anatomy in the residual maps or the diffusion weighted
    %images (this does not apply to the intial B0).
    unix(['mrcalc combined_', PAR_NAME, datafile,'.mif d', PAR_NAME, datafile '.mif -subtract res_' PAR_NAME, datafile,'.mif']);
    
    %create a copy in NIFTI format
    unix(['mrconvert res_', PAR_NAME, datafile,'.mif res_', PAR_NAME, datafile, '.nii']);
    
    %---------------------------------------------------------------------%
    %Step 2: Gibbs ringing
    unix(['mrdegibbs d' PAR_NAME, datafile,'.mif gd' PAR_NAME, datafile,'.mif -axes 0,1']);
    
    %create a copy in NIFTI format
    unix(['mrconvert gd', PAR_NAME, datafile,'.mif gd', PAR_NAME, datafile, '.nii']);
    
    %---------------------------------------------------------------------%
    %OPTIONAL - intermediate steps: edit gradient files & organising
    %This is if you need to edit/reorganise your dwi volumes - note that this had to be done specifically in this fashion below for DPRC data to follow UKB protocol 
    
    %edit gradient text files (bval & bvec files). We need to add on the
    %last B0 file (vol 106) to the dataset.
    LastB0AddOn(PAR_NAME, datafile);
    
    %separate data into main dwi data and p-a files.
    unix(['mrconvert -coord 3 0:105 gd' PAR_NAME, datafile,'.mif cgd' PAR_NAME, datafile,'.mif']);
    
    if length(BDs) == 1
        unix(['mrconvert -coord 3 106 gd' PAR_NAME, datafile,'.mif PA_' PAR_NAME, datafile,'.mif']);
    elseif length(BDs) == 2
        unix(['mrconvert -coord 3 106:107 gd' PAR_NAME, datafile,'.mif PA_' PAR_NAME, datafile,'.mif']);
    elseif length(BDs) == 3
        unix(['mrconvert -coord 3 106:108 gd' PAR_NAME, datafile,'.mif PA_' PAR_NAME, datafile,'.mif']);
    end
    
    %match up gradient files (bvals, bvecs) with main dwi dataset.
    unix(['mrconvert -fslgrad ', PAR_NAME, datafile,'.bvec ' PAR_NAME, datafile,'.bval cgd' PAR_NAME, datafile,'.mif bcgd', PAR_NAME, datafile,'.mif']);
    
    %create a copy in NIFTI format
    unix(['mrconvert bcgd', PAR_NAME, datafile,'.mif bcgd', PAR_NAME, datafile, '.nii']);
    
    %---------------------------------------------------------------------%
    %Step 3-5: Field distortion (topup, eddy)
    
    %extract all B0s from the dataset
    unix(['dwiextract -bzero bcgd' PAR_NAME, datafile,'.mif AP_' PAR_NAME, datafile,'.mif']);
    
    %place a-p and p-a images into one file
    unix(['mrcat AP_' PAR_NAME, datafile,'.mif ' 'PA_' PAR_NAME, datafile,'.mif ' 'allB0s_' PAR_NAME, datafile,'.mif']);
    
    %a) choose best pair using the 'BestB0' function
    BU_used = BestB0(PAR_NAME, datafile, NumBDs, startdir, period);
    
    %---------------------------------------------------------------------%
    %Step 4: Estimate brain mask
    %estimate brain mask with the chosen best B0 pair with fsl's BET
    unix(['fslmaths TUB0s_' PAR_NAME, datafile, '.nii -Tmean combinedTUB0s_' PAR_NAME, datafile, '.nii']);
    unix(['bet combinedTUB0s_' PAR_NAME, datafile, '.nii bet_' PAR_NAME, datafile, '.nii -m -f 0.2']);
    
    %make a copy (must convert) of the brain mask as a .nii file
    unix(['mrconvert bet_', PAR_NAME, datafile,'_mask.nii.gz brain_mask_', PAR_NAME, datafile, '.nii']);
    %create a copy in mif format
    unix(['mrconvert brain_mask_', PAR_NAME, datafile,'.nii brain_mask_', PAR_NAME, datafile, '.mif']);
    
    %---------------------------------------------------------------------%
    %Step 5: Run eddy
    %run topup, eddy (w/ -repol and option w/ gradient change and mp_order)
    RunEddy(BU_used, SlicetoVol_Corr, PAR_NAME, datafile, ScriptDirectory);
    
    %create a copy in NIFTI format
    unix(['mrconvert ebbcgd', PAR_NAME, datafile,'.mif ebbcgd', PAR_NAME, datafile, '.nii']); %eddy corrected data    
    
    %a) Perform eddy quality control (qc) per each participant
    
    %Run 'eddy_quad' on participant for quality check as well. 
    RunEddyQuad(startdir, ScriptDirectory, PAR_NAME, datafile, period);
    %add participants eddy qc data onto the collated text file.
    eddyqc_ToText(PAR_NAME, startdir, period);
    
    %---------------------------------------------------------------------%
    %Step 6: Bias field correction (B0s)
    
    %bias field correction with ants
    unix(['dwibiascorrect ants ebbcgd' PAR_NAME, datafile, '.mif febbcgd' PAR_NAME, datafile, '.mif']);
    
    %run bias field correction with ants again for improved mask and bias field
    %corrected image
    unix(['dwibiascorrect ants febbcgd' PAR_NAME, datafile, '.mif f2ebbcgd' PAR_NAME, datafile, '.mif']);

    %create a copy in NIFTI format
    unix(['mrconvert f2ebbcgd', PAR_NAME, datafile,'.mif f2ebbcgd', PAR_NAME, datafile, '.nii']);
    
    %copy dwi file over and rename
    copyfile(['f2ebbcgd', PAR_NAME, datafile,'.mif'], [PAR_NAME, datafile,'.mif']);
    movefile([PAR_NAME, datafile, '.mif'], [startdir, '/derivatives/' period, '/diff_data/', groupname, '/preprocessed_dwi']);
    
    %copy brain mask (generated from fsl's BET) file over
    copyfile(['brain_mask_', PAR_NAME, datafile,'.mif'], [PAR_NAME, datafile,'.mif']);
    movefile([PAR_NAME, datafile, '.mif'], [startdir, '/derivatives/' period, '/diff_data/', groupname, '/brain_mask']);
      
end

%o Run 'eddy_squad' to conduct group quality control on eddy
RunEddySquad(participants, startdir, period);
