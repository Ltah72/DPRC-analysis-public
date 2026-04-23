%Constrained spherical deconvolution (CSD) pipeline will conduct CSD to
%estimate response functions for fibre orientation distributions (FOD).

%Steps are followed through on the MRtrix manual here:
%https://mrtrix.readthedocs.io/en/latest/fixel_based_analysis/mt_fibre_density_cross-section.html


% Steps:
% 1. Generate response function estimates using multi-tissue CSD
% 2. Compute a group average response function of each tissue
% 3. Create FOD images (wm, gm, csf)
%   a.Upsample dwi image
%   b.Upsample brain mask images
%   c.FOD estimation (multi-tissue SD)
% 4. Joint bias field correction and intensity normalistion
% 5. Generate FOD population template
% 6. Register all subjects to FOD template
%   a.Register to FOD template
%   b.Warp mask to template
%   c.Compute intersection of masks in template space


%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz or Ltahara2@gmail.com
%Date: 30/06/20


clc;
clear all;
close all;

%define/add pathways
startdir = input('Please enter data directory:', 's');

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');

%go to the directory where your scripts are.
cd(ScriptDirectory);

%should be the same groupname from what the user analysed in the preproccessing script.
groupname = input('Which pre-processed group / study do you want to continue to analyse?: ', 's');

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%go to group folder
cd([startdir '/derivatives/' period '/diff_data/', groupname]);

%make a parent directory to hold all inputs used for CSD and FBA
mkdir([startdir,'/derivatives/diff_data/', groupname, '/IN/']);
%make subdirectories to hold the participants' data (response functions, fod
%images, brain masks)
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/responseFunctionWM/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/responseFunctionGM/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/responseFunctionCSF/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/wmFODimages/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/gmFODimages/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/csfFODimages/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_wmFOD']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_gmFOD/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_csfFOD/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_upsampled_masks/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/upsampled_masks/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/warpedMasks/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/qc/']);
mkdir([startdir,'/derivatives/' period, '/diff_data/', groupname, '/template/']);
%move the preprocessed data (dwi and masks) into the parent directory
movefile([startdir,'/derivatives/' period, '/diff_data/', groupname, '/preprocessed_dwi'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN']);
movefile([startdir,'/derivatives/' period, '/diff_data/', groupname, '/brain_mask'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(startdir));
addpath(genpath(ScriptDirectory));

%define dwi datafile
datafile = '_acq_data_dwi';

%choose which participants will comprise the population template (~40 total):
%message user
msgfig = 'Choose participants for population template (~40). You should have an even number of participants to represent each group.';
uiwait(msgbox(msgfig));
cd([startdir '/derivatives/' period, '/diff_data/', groupname, '/IN/preprocessed_dwi/']);
pop_rep_pts = uipickfiles;
cd([startdir '/derivatives/' period, '/diff_data/' groupname, '/template']);
fid4 = fopen('pop_temp_pts.txt', 'a+');
if (fid4 == -1)
    disp('Error in opening the text file.')
else
    for i = 1:length(pop_rep_pts)
        [upper_path, POP_PAR, ~] = fileparts(pop_rep_pts{1,i});
        POP_PAR = POP_PAR(1:15);
        fprintf(fid4, '%s', POP_PAR);
        fprintf(fid4, '\n');
    end
    fclose(fid4);
end

%go back into participant derivatives folder
cd([startdir '/derivatives/' period, '/diff_data/' groupname, '/IN/preprocessed_dwi/']);

%choose participants for analysis (do not include excluded participants). 
msgfig = 'Choose participants for analysis (do not include excluded participants, or participants you are not interested in (e.g. naMCI))';
uiwait(msgbox(msgfig));
participants = uipickfiles;

%go back into group folder
cd([startdir '/derivatives/' period, '/diff_data/', groupname]);

%CSD steps begin below:

%-------------------------------------------------------------------------%
%Step 1: Generate response function estimates using multi-tissue CSD
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
    %generate a response function per each subject - use the dhollander algorithm
    unix(['dwi2response dhollander -mask IN/brain_mask/', PAR_NAME, datafile, '.mif IN/preprocessed_dwi/', PAR_NAME, datafile, '.mif IN/responseFunctionWM/' PAR_NAME, '_out_sfwm IN/responseFunctionGM/' PAR_NAME '_out_gm IN/responseFunctionCSF/' PAR_NAME '_out_csf -voxels qc/' PAR_NAME '_voxels.mif']);
    
end

%-------------------------------------------------------------------------%
%Step 2: Compute a group average response function of each tissue
unix(['responsemean ' startdir, '/derivatives/' period, '/diff_data/' groupname, '/IN/responseFunctionWM/* group_average_responseWM.txt']);
unix(['responsemean ' startdir, '/derivatives/' period, '/diff_data/' groupname, '/IN/responseFunctionGM/* group_average_responseGM.txt']);
unix(['responsemean ' startdir, '/derivatives/' period, '/diff_data/' groupname, '/IN/responseFunctionCSF/* group_average_responseCSF.txt']);

%-------------------------------------------------------------------------%
%Step 3: Create FOD images (wm, gm, csf fod images)

for j = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,j});
    PAR_NAME = PAR_NAME(1:15);
    
    %a) upsample DW images
    %regrid option (Gaussian smoothing)
    unix(['mrgrid -voxel 1.25 IN/preprocessed_dwi/' PAR_NAME, datafile, '.mif regrid IN/upsampled_dwi_' PAR_NAME '.mif']);
    %convert to NIFTI format
    unix(['mrconvert IN/upsampled_dwi_' PAR_NAME, '.mif IN/upsampled_dwi_' PAR_NAME, '.nii']);

    %b) compute an upsampled brain mask image from the dwi image (use BET again)
    %get reference image (b0)
    unix(['mrconvert -coord 3 0 IN/upsampled_dwi_' PAR_NAME '.mif IN/ref_b0_' PAR_NAME '.mif']);
    %convert to NIFTI format
    unix(['mrconvert IN/ref_b0_' PAR_NAME, '.mif IN/ref_b0_' PAR_NAME, '.nii']);
    unix(['bet IN/ref_b0_' PAR_NAME, '.nii IN/upsampled_mask_' PAR_NAME, '.nii -m -f 0.2']);
    unix(['mrconvert IN/upsampled_mask_' PAR_NAME, '_mask.nii.gz IN/upsampled_mask_' PAR_NAME '.mif']);
    
    %move, copy, and rename each of the upsampled brain mask into a folder
    %movefile(['upsampled_mask_' PAR_NAME '.mif'], [startdir,'/derivatives/diff_data/', groupname, '/IN/']);
    copyfile(['IN/upsampled_mask_' PAR_NAME '.mif'], ['IN/upsampled_masks/PRE_' PAR_NAME '.mif']);
    
    %c) fibre orientation distribution estimation (using group response
    %estimates for each tissue and the upsampled images)
    unix(['dwi2fod msmt_csd IN/upsampled_dwi_' PAR_NAME '.mif group_average_responseWM.txt IN/wmfod_' PAR_NAME '.mif group_average_responseGM.txt IN/gmfod_' PAR_NAME '.mif group_average_responseCSF.txt IN/csffod_' PAR_NAME '.mif -mask IN/upsampled_mask_' PAR_NAME '.mif']);
    
    %view the FODs in a single image with all 3 tissue types:
    unix(['mrconvert -coord 3 0 IN/wmfod_' PAR_NAME '.mif - | mrcat IN/csffod_' PAR_NAME '.mif IN/gmfod_' PAR_NAME '.mif - qc/vf_' PAR_NAME '.mif']);
    %You can then view this file in mrview with the odf file overlayed
    
    %---------------------------------------------------------------------%
    %Step 4: Joint bias field correction and intensity normalisation
    unix(['mtnormalise IN/wmfod_' PAR_NAME '.mif IN/wmfod_norm_' PAR_NAME '.mif IN/gmfod_' PAR_NAME '.mif IN/gmfod_norm_' PAR_NAME '.mif IN/csffod_' PAR_NAME '.mif IN/csffod_norm_' PAR_NAME '.mif -mask IN/upsampled_mask_' PAR_NAME '.mif']);
    
    %copy and rename each of the FOD images (e.g. wmfod_PARNAME.mif) into a separate folder
    copyfile (['IN/wmfod_norm_' PAR_NAME '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/wmFODimages/']);
    movefile(['IN/wmFODimages/wmfod_norm_' PAR_NAME '.mif'], ['IN/wmFODimages/PRE_' PAR_NAME '.mif']);
    copyfile (['IN/gmfod_norm_' PAR_NAME '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/gmFODimages/']);
    movefile(['IN/gmFODimages/gmfod_norm_' PAR_NAME '.mif'], ['IN/gmFODimages/PRE_' PAR_NAME '.mif']);
    copyfile (['IN/csffod_norm_' PAR_NAME '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/csfFODimages/']);
    movefile(['IN/csfFODimages/csffod_norm_' PAR_NAME '.mif'], ['IN/csfFODimages/PRE_' PAR_NAME '.mif']);
    
end


%-------------------------------------------------------------------------%
%Step 5: Generate a study-specific unbiased FOD template - this step is
%computationally expensive and takes a long time to process. Will pick out
%the participants that you chose at the beginning.
for i = 1:length(pop_rep_pts)
    [upper_path, POP_PAR, ~] = fileparts(pop_rep_pts{1,i});
    POP_PAR = POP_PAR(1:15);
    copyfile (['IN/wmfod_norm_' POP_PAR '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_wmFOD/']);
    copyfile (['IN/gmfod_norm_' POP_PAR '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_gmFOD/']);
    copyfile (['IN/csffod_norm_' POP_PAR '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_csfFOD/']);
    copyfile (['IN/upsampled_mask_' POP_PAR, '.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/pop_temp_upsampled_masks/']);
end

unix(['population_template IN/pop_temp_wmFOD template/wmfod_template.mif IN/pop_temp_gmFOD template/gmfod_template.mif IN/pop_temp_csfFOD template/csffod_template.mif -mask_dir IN/pop_temp_upsampled_masks -voxel_size 1.25']);

%-------------------------------------------------------------------------%
%Step 6: Register all subjects to FOD template
for k = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,k});
    PAR_NAME = PAR_NAME(1:15);
    
    %register all subject FOD images to the FOD template
    unix(['mrregister IN/wmfod_norm_' PAR_NAME '.mif -mask1 IN/upsampled_mask_' PAR_NAME '.mif template/wmfod_template.mif -nl_warp IN/' PAR_NAME '_subject2template_warp.mif IN/' PAR_NAME '_template2subject_warp.mif']);
    
    %warp all subject masks into template space
    unix(['mrtransform IN/upsampled_mask_' PAR_NAME '.mif -warp IN/' PAR_NAME '_subject2template_warp.mif -interp nearest -datatype bit IN/' PAR_NAME '_dwi_mask_in_template_space.mif']);
    
    %place a copy of output warped mask (e.g. PAR_NAME_dwi_mask_in_template_space.mif) into a folder
    copyfile (['IN/' PAR_NAME '_dwi_mask_in_template_space.mif'], [startdir,'/derivatives/' period, '/diff_data/', groupname, '/IN/warpedMasks/']);
    
end

%Compute the intersection of all subject masks in template space
unix(['mrmath ' startdir, '/derivatives/' period, '/diff_data/' groupname, '/IN/warpedMasks/* min template/template_mask.mif -datatype bit']);




