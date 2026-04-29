%Run Fmriprep (version 20.2.1) via Docker on subjects. For the anatomical 
%files, this includes: bias field correction, skull-stripping, brain 
%surface reconstruction (recon-all,FreeSurfer), brain mask estimation (ANTs 
%+ FreeSurfer), spatial normalisation (antsRegistration, ANTs), brain 
%tissue segmentation (FAST, FSL). For the functional files, this includes:
%slice time correction (3dtshift, AFNI), motion correction (mcflirt, FSL),
%coregistration to T1w with 6 dof (bbregister, FreeSurfer), and motion
%correction transformations (antsApplyTransforms, ANTs). 

%If it hasn't been done already, you may need to edit the files for
%fmriprep (e.g. .json files), which I have created a script for this,
%called ModifyFilesForFmriprep.m.

%Esteban, O., Markiewicz, C. J., Blair, R. W., Moodie, C. A., Isik, A. I., Erramuzpe, A., … Gorgolewski, K. J. (2019). fMRIPrep: a robust preprocessing pipeline for functional MRI.
%https://fmriprep.org/en/stable/

%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 10/01/21

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%if using Docker, define your home directory: 
fmriprep_dock_dir = input('Please enter your home directory where fmriprep-docker is stored:', 's');
%e.g., fmriprep_dock_dir = '/home/ubuntu/.local/bin/./fmriprep-docker'

%Define your sourcedata directory:
sourcedataDir = input('Please enter sourcedata directory:', 's');
%e.g,. sourcedataDir = (['/data/sourcedata/' period]);

%Define fmriprep directory, so that it may be used:
FmriprepDir = input('Please enter fmriprep directory:', 's');
%e.g, FmriprepDir = '/data/USERS/LENORE/fmriprep_directory/';

%go into fmriprep sourcedata directory
cd([FmriprepDir 'sourcedata/']);

%specify participants
participants = uipickfiles;

for i = 1:length(participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(participants{1,i});
    
    %---------------------------------------------------------------------%
    %Step 1: Preprocess anatomical images (T1w and T2 FLAIR images)
    
    %a) Gibbs ringing correction (mrdegibbs) on all anat files (T1w, T2w
    %blade, + T2w FLAIR). Use the '-force' option to overwrite the raw file in the sourcedata. 
    %Do note that every time you re-do fmriprep, this script will copy the 
    %raw, uncleaned /anat sourcedata to work with, with the participants you chose. 
    copyfile ([sourcedataDir, '/' PAR_NAME, '/anat/*'], [FmriprepDir, '/sourcedata/' PAR_NAME, '/anat']);
    %Gibbs ringing
    cd ([PAR_NAME '/anat']);
    unix(['mrdegibbs ' PAR_NAME '_T1w.nii ' PAR_NAME '_T1w.nii -force']);
    unix(['mrdegibbs ' PAR_NAME '_T2w.nii ' PAR_NAME '_T2w.nii -force']);
    unix(['mrdegibbs ' PAR_NAME '_FLAIR.nii ' PAR_NAME '_FLAIR.nii -force']);
    
    cd([FmriprepDir 'sourcedata/']);
    
    %b) Run fmriprep to preprocess anat + func images with different options: 
    
    %using a custom template (e.g., DPRC template)
    unix(['sudo ' fmriprep_dock_dir, ' ' FmriprepDir 'sourcedata ' FmriprepDir 'derivatives participant --participant-label ' PAR_NAME ' --output-spaces /SOFTWARE/fmriprep/templateflow/tpl-DPRCcustom --skull-strip-t1w force --fs-license-file /SOFTWARE/freesurfer/license.txt']);  
    
    %using a native space (participant func)
    %unix(['sudo ' fmriprep_dock_dir, ' ' FmriprepDir 'sourcedata ' FmriprepDir 'derivatives participant --participant-label ' PAR_NAME ' --output-spaces func --skull-strip-t1w force --fs-license-file /SOFTWARE/freesurfer/license.txt']); 

    %using a standard template
    %unix(['sudo ' fmriprep_dock_dir, ' ' FmriprepDir 'sourcedata ' FmriprepDir 'derivatives participant --participant-label ' PAR_NAME ' --output-spaces MNI152NLin2009cAsym --skull-strip-t1w force --fs-license-file /SOFTWARE/freesurfer/license.txt']);

end
