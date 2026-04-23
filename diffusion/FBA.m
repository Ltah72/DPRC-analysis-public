%Fixel-based analysis (FBA) pipeline, which is sequential to running the
%CSD pipeline. The FBA pipeline will compute the diffusion fibre metrics,
%which are fibre density (FD), fibre cross-section (FC), and fibre density
%and cross-section (FDC). Statistical analysis will be performed on these
%metrics, and whole-brain fibre tractography will be conducted.

%MRtrix manual: https://mrtrix.readthedocs.io/en/latest/fixel_based_analysis/mt_fibre_density_cross-section.html

% Steps:
% 1. Compute a white matter template analysis fixel mask
% 2. Estimate participants’ fixels and diffusion fibre metric
%   a.Warp FOD images to template space
%   b.Segment FOD images to estimate fixels and their FD metric
%   c.Reorient fixels
%   d.Assign subject fixels to template fixels (compute FD)
%   e.Compute FC metric
%   f.Compute FDC metric
% 3. Perform whole-brain fibre tractography on the FOD template
% 4. Reduce biases in tractogram densities (using SIFT)
% 5. Generate fixel-fixel connectivity matrix
% 6. Smooth fixel data using fixel-fixel connectivity
% 7. Perform statistical analysis of FD, FC, and FDC
% 8. Visualise results (with mrview)
% 9. Display results with streamlines
%   a.Reduce number of streamlines to 200k
%   b.Create .tsf file (map fixels to streamlines)
%   c.Visualise .tsf in mrview
% 10. FBA post-statistical inference
%   a.Calculate whole-brain FBA metrics per each participant and put onto a
%   text file
%   b.Express the effect size relative to controls

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz or Ltahara2@gmail.com
%Date: 30/06/20

clc;
clear all;
close all;

%define/add pathways
derivdir = input('Please enter derivatives directory:', 's');

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');

%should be the same groupname from what the user analysed in the CSD script.
groupname = input('Which pre-processed group / study do you want to continue to analyse (e.g cross-sectional, longitudinal)?: ', 's');

%choose time period
period = input('Which time period do you want to analyse (e.g. F0, F2, TH, all, etc)?: ', 's');

%make directories for FBA data
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/log_fc_data/']);
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/fd_data/']);
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/template/fdc_data/']);

%make directory to hold your matrices (e.g. design and contrast)for
%statistical tests - for FBA analysis. You need to manually create and
%store the files in here.
mkdir([derivdir,'/groups/' period, '/diff_data/', groupname, '/stats_matrices/']);

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(derivdir));
addpath(genpath(ScriptDirectory));

%go into participant derivatives folder
cd([derivdir '/groups/' period, '/diff_data/' groupname, '/IN/preprocessed_dwi']);

%choose participants for analysis (do not include excluded participants).
msgfig = 'Choose participants for analysis (do not include excluded participants)';
uiwait(msgbox(msgfig));
participants = uipickfiles;

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
%go back into group folder
cd([derivdir '/groups/' period, '/diff_data/', groupname]);


%FBA steps begin below:
%-------------------------------------------------------------------------%
%Step 1: Compute a white matter template analysis fixel mask
unix(['fod2fixel -mask template/template_mask.mif -fmls_peak_value 0.06 template/wmfod_template.mif template/fixel_mask']);

%-------------------------------------------------------------------------%
%Step 2: Estimate participants’ fixels and FBA metrics
for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    PAR_NAME = PAR_NAME(1:15);
    
    %a) Warp each participant's FOD images to template space.
    unix(['mrtransform IN/wmfod_norm_' PAR_NAME '.mif -warp IN/' PAR_NAME '_subject2template_warp.mif -reorient_fod no IN/' PAR_NAME '_fod_in_template_space_NOT_REORIENTED.mif -force']);
    
    %b) segment FOD images to estimate fixels and their fibre density (FD)
    mkdir(['IN/' PAR_NAME]);
    unix(['fod2fixel -mask template/template_mask.mif IN/' PAR_NAME '_fod_in_template_space_NOT_REORIENTED.mif IN/' PAR_NAME '/fixel_in_template_space_NOT_REORIENTED -afd fd.mif']);
    
    %c) reorient fixel orientations
    unix(['fixelreorient IN/' PAR_NAME '/fixel_in_template_space_NOT_REORIENTED IN/' PAR_NAME '_subject2template_warp.mif IN/' PAR_NAME '/fixel_in_template_space']);
    
    %d) assign subject fixels to template fixels (compute FD)
    mkdir(['template/' PAR_NAME '/fd']);
    unix(['fixelcorrespondence IN/' PAR_NAME '/fixel_in_template_space/fd.mif template/fixel_mask template/' PAR_NAME '/fd fd.mif']);
    
    %e) compute fibre cross-section (FC) metric
    mkdir(['template/' PAR_NAME '/fc']);
    unix(['warp2metric IN/' PAR_NAME '_subject2template_warp.mif -fc template/fixel_mask template/' PAR_NAME '/fc fc.mif']);
    
    %for group analysis FC - take the log of FC
    mkdir(['template/' PAR_NAME '/log_fc']);
    copyfile (['template/' PAR_NAME '/fc/index.mif'], ['template/' PAR_NAME '/log_fc']);
    copyfile (['template/' PAR_NAME '/fc/directions.mif'], ['template/' PAR_NAME '/log_fc']);
    unix(['mrcalc template/' PAR_NAME '/fc/fc.mif -log template/' PAR_NAME '/log_fc/log_fc.mif']);
    
    %f) Compute a combined measure of fibre density and cross-section (FDC)
    mkdir(['template/' PAR_NAME '/fdc']);
    copyfile (['template/' PAR_NAME '/fc/index.mif'], ['template/' PAR_NAME '/fdc']);
    copyfile (['template/' PAR_NAME '/fc/directions.mif'], ['template/' PAR_NAME '/fdc']);
    unix(['mrcalc template/' PAR_NAME '/fd/fd.mif template/' PAR_NAME '/fc/fc.mif -mult template/' PAR_NAME '/fdc/fdc.mif']);
    
    %copy and rename each participant file into the 3 metric files (fd, fc, fdc) into separate folders
    copyfile (['template/' PAR_NAME '/fd/fd.mif'], ['template/fd_data/' PAR_NAME '_fd.mif']);
    copyfile (['template/' PAR_NAME '/log_fc/log_fc.mif'], ['template/log_fc_data/' PAR_NAME '_log_fc.mif']);
    copyfile (['template/' PAR_NAME '/fdc/fdc.mif'], ['template/fdc_data/' PAR_NAME '_fdc.mif']);
    
end

%copy 1 index and 1 directions file of each metric (fd, fc, and fdc) into
%the 3 metric files (any 1 participant file is okay to use for this).
copyfile (['template/' PAR_NAME '/fd/index.mif'], ['template/fd_data/']);
copyfile (['template/' PAR_NAME '/fd/directions.mif'], ['template/fd_data/']);
copyfile (['template/' PAR_NAME '/log_fc/index.mif'], ['template/log_fc_data/']);
copyfile (['template/' PAR_NAME '/log_fc/directions.mif'], ['template/log_fc_data/']);
copyfile (['template/' PAR_NAME '/fdc/index.mif'], ['template/fdc_data/']);
copyfile (['template/' PAR_NAME '/fdc/directions.mif'], ['template/fdc_data/']);

%-------------------------------------------------------------------------%
%Step 3: Perform whole-brain fibre tractography on the FOD template -- this
%step is computationally expensive and takes a long time to process
cd template;
unix(['tckgen -angle 22.5 -maxlen 250 -minlen 10 -power 1.0 wmfod_template.mif -seed_image template_mask.mif -mask template_mask.mif -select 20000000 -cutoff 0.06 tracks_20_million.tck']);
%-------------------------------------------------------------------------%
%Step 4:Reduce biases in tractogram densities (using SIFT)
unix(['tcksift tracks_20_million.tck wmfod_template.mif tracks_2_million_sift.tck -term_number 2000000']);

%-------------------------------------------------------------------------%
%Step 5: Generate fixel-fixel connectivity matrix - redo
unix(['fixelconnectivity fixel_mask/ tracks_2_million_sift.tck matrix/']);

%-------------------------------------------------------------------------%
%Step 6: Smooth fixel data using fixel-fixel connectivity
unix(['fixelfilter fd_data smooth fd_smooth -matrix matrix/']);
unix(['fixelfilter log_fc_data smooth log_fc_smooth -matrix matrix/']);
unix(['fixelfilter fdc_data smooth fdc_smooth -matrix matrix/']);


%-------------------------------------------------------------------------%
%Step 7: Perform statistical analysis of FD, FC, and FDC -- this step is
%computationally expensive and takes a long time to process
cd ../;

%create text files of a participant list to run statistics
CreateParticipantFixelList;

%use the design matrix (e.g. design_matrix.txt) that corresponds to the participant list (participant
%groups - i.e. status)

%create contrast matrix to specify the tests and/or co-variates that you
%will include in your analysis (e.g. contrast_matrix.txt)

%Run statistical tests for each fibre metric (e.g., FD, FC, FDC) - see examples below.
k = 0;
while true
unix(['fixelcfestats -strong template/fd_smooth/ files_fd.txt stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt template/matrix/ stats_fd/']);
unix(['fixelcfestats -strong template/log_fc_smooth/ files_log_fc.txt stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt template/matrix/ stats_log_fc/']);
unix(['fixelcfestats -strong template/fdc_smooth/ files_fdc.txt stats_matrices/design_matrix.txt stats_matrices/contrast_matrix.txt template/matrix/ stats_fdc/']);
k = k+1;
disp(k)
times_run = k;
end 




%%%%% Below, you will need to manually edit these commands specific to your data. See examples below.  

%-------------------------------------------------------------------------%
%Step 8: Visualise results
%To view the results load the population FOD template image in mrview, and
%overlay the fixel images using the vector plot tool. Note that p-value
%images are saved as (1 - p-value). Therefore to visualise all results at a
%threshold of p < 0.05, within the mrview fixel plot tool, apply a lower
%threshold at a value of 0.95.



%-------------------------------------------------------------------------%
%Step 9: Display results with streamlines
%cropping streamlines from the template-derived whole-brain tractogram to include streamline points that correspond to significant fixels

% a) Reduce number of streamlines to 200,000
unix(['tckedit template/tracks_2_million_sift.tck -num 200000 template/tracks_200k_sift.tck']);

%b) map fixel values to streamline points and save them in a 'track scalar file' (.tsf)
unix(['fixel2tsf stats_fd/fwe_1mpvalue.mif template/tracks_200k_sift.tck stats_fd/fd_WholeBrainfwe_pvalue.tsf']);
unix(['fixel2tsf stats_log_fc/fwe_1mpvalue.mif template/tracks_200k_sift.tck stats_log_fc/fc_WholeBrainfwe_pvalue.tsf']);
unix(['fixel2tsf stats_fdc/fwe_1mpvalue.mif template/tracks_200k_sift.tck stats_fdc/fdc_WholeBrainfwe_pvalue.tsf']);

%c) visualise track scalar files using the tractogram tool in MRview. First
%load the streamlines (tracks_200k_sift.tck). Then to dynamically threshold
%(remove) streamline point by p-value select the “Thresholds” dropdown and
%select “Separate Scalar file” and set to 0.95.

%d) Smooth the .tsf file
unix(['tsfsmooth -stdev 4 stats_fd/fd_WholeBrainfwe_pvalue.tsf stats_fd/smoothed_fd_WholeBrainfwe_pvalue.tsf']);
unix(['tsfsmooth -stdev 4 stats_fc/fc_WholeBrainfwe_pvalue.tsf stats_fc/smoothed_fc_WholeBrainfwe_pvalue.tsf']);
unix(['tsfsmooth -stdev 4 stats_fdc/fdc_WholeBrainfwe_pvalue.tsf stats_fdc/smoothed_fdc_WholeBrainfwe_pvalue.tsf']);


%-------------------------------------------------------------------------%
%Step 10: FBA post-statistical inference

%a) Calculate whole-brain FBA metrics per each participant and put onto a text file.
%CreateWholeBrainFBAMetricFiles(participants);
%use this files as inputs into a statistical programme in order to
%calculate the statistics of the group differences


%b) Expressing the effect size relative to controls
%for fd
unix(['mrcalc stats_fd/abs_effect.mif stats_fd/beta0.mif  -div 100 -mult stats_fd/percentage_effect.mif']);
%for fdc
unix(['mrcalc stats_fdc/abs_effect.mif stats_fdc/beta0.mif  -div 100 -mult stats_fdc/percentage_effect.mif']);
%for fc
unix(['mrcalc 1 1 stats_log_fc/abs_effect.mif -exp -div -sub stats_log_fc/percentage_effect.mif']);





