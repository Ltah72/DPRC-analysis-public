%Create population template based on your study cohort, using FSL's FLIRT
%tool. 

%Steps are based upon this fslwiki page: http://web.mit.edu/fsl_v5.0.10/fsl/doc/wiki/FLIRT(2f)FAQ.html#How_do_I_make_my_own_study-specific_template_image_with_FLIRT.3F


clc;
clear all;
close all;

%choose time period
period = input('Which time period do you want to analyse, e.g. F0, F2, all, etc?: ', 's');

%define/add pathways
StudyTempDir = input('Please enter study template directory:', 's');
%e.g., StudyTempDir = '/data/USERS/LENORE/study_template/';

%Define your sourcedata directory:
sourcedataDirectory = input('Please enter sourcedata directory:', 's');
%e.g., sourcedataDir = (['/data/sourcedata/' period]);

%Script directory is defined, so that it can be added to path below:
ScriptDirectory = input('Please enter script directory:', 's');
%e.g., ScriptDirectory = '/data/USERS/LENORE/scripts/dprc/';

%Add your script and all necessary files (e.g. data, functions) to path
addpath(genpath(StudyTempDir));
addpath(genpath(ScriptDirectory));

%go into sourcedata file
cd (sourcedataDir);

%Choose template participants (leave out a reference image)
study_participants = uipickfiles;

%Choose a reference image from among the set & place into directory
RefFile = uipickfiles;
RefName = RefFile{1};
RefPAR = RefName(21:35); %modify if needed
copyfile ([sourcedataDir, '/' RefPAR '/anat/' RefPAR '_T1w.nii'], [StudyTempDir]); 

%go into study template dir
cd (StudyTempDir);

%place the rest of image files into directory
for i = 1:length(study_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(study_participants{1,i});
    copyfile ([sourcedataDir, '/' PAR_NAME '/anat/' PAR_NAME '_T1w.nii'], [StudyTempDir]); 
    
    %Register each image in the set to the reference image, using FLIRT, and 
    %saving the output images
    unix(['flirt -in ' PAR_NAME '_T1w -ref ' RefPAR '_T1w -dof 12 -out imout' num2str(i) ' -omat ' PAR_NAME '_to_imref.mat']); 
end

%Average the output images together using fslmaths (for 100 participants)
unix(['fslmaths imout1 -add imout2 -add imout3 -add imout4 -add imout5 -add imout6 -add imout7 -add imout8 -add imout9 -add imout10'...'
    ' -add imout11 -add imout12 -add imout13 -add imout14 -add imout15 -add imout16 -add imout17 -add imout18 -add imout19 -add imout20'...' 
    ' -add imout21 -add imout22 -add imout23 -add imout24 -add imout25 -add imout26 -add imout27 -add imout28 -add imout29 -add imout30'...' 
    ' -add imout31 -add imout32 -add imout33 -add imout34 -add imout35 -add imout36 -add imout37 -add imout38 -add imout39 -add imout40'...' 
    ' -add imout41 -add imout42 -add imout43 -add imout44 -add imout45 -add imout46 -add imout47 -add imout48 -add imout49 -add imout50'...' 
    ' -add imout51 -add imout52 -add imout53 -add imout54 -add imout55 -add imout56 -add imout57 -add imout58 -add imout59 -add imout60'...' 
    ' -add imout61 -add imout62 -add imout63 -add imout64 -add imout65 -add imout66 -add imout67 -add imout68 -add imout69 -add imout70'...' 
    ' -add imout71 -add imout72 -add imout73 -add imout74 -add imout75 -add imout76 -add imout77 -add imout78 -add imout79 -add imout80'...' 
    ' -add imout81 -add imout82 -add imout83 -add imout84 -add imout85 -add imout86 -add imout87 -add imout88 -add imout89 -add imout90'...' 
    ' -add imout91 -add imout92 -add imout93 -add imout94 -add imout95 -add imout96 -add imout97 -add imout98 -add imout99 -add imout100'...' 
    ' -div 100 avg_im -odt float']);

%Repeat process and iterate your images, but this time using your average
%created image as the reference image
for i = 1:length(study_participants)
    
    [upper_path, PAR_NAME, ~] = fileparts(study_participants{1,i});
    
    %Register each image in the set to the new reference image (the average 
    %image you created above), using FLIRT, and saving the output images
    unix(['flirt -in ' PAR_NAME '_T1w -ref avg_im -dof 12 -out imout-iter_' num2str(i) ' -omat ' PAR_NAME '_to_imref-iter.mat']);   
end

%Average the output images together using fslmaths (for 100 participants)
unix(['fslmaths imout-iter_1 -add imout-iter_2 -add imout-iter_3 -add imout-iter_4 -add imout-iter_5 -add imout-iter_6 -add imout-iter_7 -add imout-iter_8 -add imout-iter_9 -add imout-iter_10'...'
    ' -add imout-iter_11 -add imout-iter_12 -add imout-iter_13 -add imout-iter_14 -add imout-iter_15 -add imout-iter_16 -add imout-iter_17 -add imout-iter_18 -add imout-iter_19 -add imout-iter_20'...' 
    ' -add imout-iter_21 -add imout-iter_22 -add imout-iter_23 -add imout-iter_24 -add imout-iter_25 -add imout-iter_26 -add imout-iter_27 -add imout-iter_28 -add imout-iter_29 -add imout-iter_30'...' 
    ' -add imout-iter_31 -add imout-iter_32 -add imout-iter_33 -add imout-iter_34 -add imout-iter_35 -add imout-iter_36 -add imout-iter_37 -add imout-iter_38 -add imout-iter_39 -add imout-iter_40'...' 
    ' -add imout-iter_41 -add imout-iter_42 -add imout-iter_43 -add imout-iter_44 -add imout-iter_45 -add imout-iter_46 -add imout-iter_47 -add imout-iter_48 -add imout-iter_49 -add imout-iter_50'...' 
    ' -add imout-iter_51 -add imout-iter_52 -add imout-iter_53 -add imout-iter_54 -add imout-iter_55 -add imout-iter_56 -add imout-iter_57 -add imout-iter_58 -add imout-iter_59 -add imout-iter_60'...' 
    ' -add imout-iter_61 -add imout-iter_62 -add imout-iter_63 -add imout-iter_64 -add imout-iter_65 -add imout-iter_66 -add imout-iter_67 -add imout-iter_68 -add imout-iter_69 -add imout-iter_70'...' 
    ' -add imout-iter_71 -add imout-iter_72 -add imout-iter_73 -add imout-iter_74 -add imout-iter_75 -add imout-iter_76 -add imout-iter_77 -add imout-iter_78 -add imout-iter_79 -add imout-iter_80'...' 
    ' -add imout-iter_81 -add imout-iter_82 -add imout-iter_83 -add imout-iter_84 -add imout-iter_85 -add imout-iter_86 -add imout-iter_87 -add imout-iter_88 -add imout-iter_89 -add imout-iter_90'...' 
    ' -add imout-iter_91 -add imout-iter_92 -add imout-iter_93 -add imout-iter_94 -add imout-iter_95 -add imout-iter_96 -add imout-iter_97 -add imout-iter_98 -add imout-iter_99 -add imout-iter_100'...' 
    ' -div 100 avg_im_iter -odt float']);

