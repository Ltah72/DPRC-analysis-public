%Create a fractional anisotropy (FA) template equivalent of the white 
%matter fibre orientation density (FOD) population template. Guidelines 
%from: https://community.mrtrix.org/t/fa-template-from-fod-template/4991/4

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 11/10/21

%define/add pathways
derivdir = input('Please enter derivatives directory:', 's');
%e.g., derivdir = '/data/USERS/LENORE/derivatives';

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc';

%should be the same groupname from what the user analysed in the preproccessing script.
groupname = input('Which pre-processed group / study do you want to continue to analyse, e.g, cross-sectional, longitudinal, etc?: ', 's');

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, TH, all, etc?: ', 's');

%Create FA template directory
mkdir([derivdir,'/groups/' period '/diff_data/' groupname '/IN/template/FA/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(derivdir));
addpath(genpath(ScriptDirectory));

%define dwi datafile
datafile = '_acq_data_dwi';

%go back into participant derivatives folder
cd([derivdir '/groups/' period, '/diff_data/' groupname, '/IN/preprocessed_dwi/']);

%choose participants for analysis (do not include excluded participants). It has been advised from the MRtrix guideline that around 40 participants is a good number.  
msgfig = 'Choose participants for FA population template (~40). You should have an even number of participants to represent each group.';
uiwait(msgbox(msgfig));
participants = uipickfiles;

%go back into group folder
cd([derivdir '/groups/' period, '/diff_data/', groupname]);


%-------------------------------------------------------------------------%
%Step 1: Convert subject data to FA images
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
   
    %generate a tensor image from the preprocessed dwi
    unix(['dwi2tensor -mask IN/brain_mask/', PAR_NAME, datafile, '.mif IN/preprocessed_dwi/', PAR_NAME, datafile, '.mif template/FA/dt_' PAR_NAME '.mif']);
    %generate FA image from tensor image
    unix(['tensor2metric template/FA/dt_' PAR_NAME '.mif -fa template/FA/fa_' PAR_NAME '.mif']);
end

%-------------------------------------------------------------------------%
%Step 2: Create wmfod population template with the warp directory
unix(['population_template IN/pop_temp_wmFOD template/FA/wmfod_template.mif IN/pop_temp_gmFOD template/FA/gmfod_template.mif IN/pop_temp_csfFOD template/FA/csffod_template.mif -mask_dir IN/pop_temp_upsampled_masks -voxel_size 1.25 -warp_dir template/FA/warps']);

%-------------------------------------------------------------------------%
%Step 3: Warp FA images to template FOD
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
   
    unix(['mrtransform template/FA/fa_' PAR_NAME '.mif -template template/FA/wmfod_template.mif -warp_full template/FA/warps/fod_norm_' PAR_NAME '.mif template/FA/FA_warped_' PAR_NAME '.mif']);
end

%-------------------------------------------------------------------------%
%Step 4: compute the average of the transformed images
unix(['mrmath template/FA/*FA_warped* mean template/FA/FA_template.mif']);

%-------------------------------------------------------------------------%
%Step 5: convert to nifti format
unix(['mrconvert template/FA/FA_template.mif template/FA/FA_template.nii']);

