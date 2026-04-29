# -*- coding: utf-8 -*-
"""
Created on Wed Aug 18 16:39:30 2021

@author: ltah262
"""
#Run *Denoiser* as a tool to remove the nuisance regressors from fMRI data. This
#tool will remove the specified noise regressors from the .tsv file, which was
#generated from fmriprep. 

#Denoiser: https://github.com/arielletambini/denoiser
    
#fmriprep: https://fmriprep.org/en/stable/

#fmriprep does minimal preprocessing on fMRI BOLD data, and so in order to 
#preprocess your data, you can use the .tsv file to denoise the data more. This 
#should be run on your data after running fmriprep. 

#This script was created in Spyder, which utilises Python code. 

#import libraries
import os
import subprocess


#define location of the denoiser script
denoiserLocation = input('Please enter the directory with the denoiser script: ');
#e.g., denoiserLocation = 'C:/Users/ltah262/Programs/denoiser-master/run_denoise.py'
#define fmriprepped data directory
fmriprepDir = input('Please enter the directory of the derivatives of fmriprep: ');
#e.g, fmriprepDir = 'V:/Vault/NECTAR_data/LENORE/derivatives/fmriprepped_data/derivatives/fmriprep/'
#define output path - where your denoised data will be stored. 
out_path = input('Please enter the output directory for the denoised data to be stored into: ');
#e.g., out_path = 'V:/NECTAR_data/LENORE/derivatives/fMRI_denoised/'

#go into fmriprepped derivatives directory
os.chdir(fmriprepDir)

#list participants (by their output folder)
participants = next(os.walk('.'))[1]
#skip the log file (the first file) in the fmriprep derivatives directory
participants = participants[1:]

#go into output directory
os.chdir(out_path)

for i in participants:

    #define variables
    PAR_NAME = i
   
    #check formatting of the data file names
    img_file = fmriprepDir+PAR_NAME+'/func/'+PAR_NAME+'_task-rest_run-1_space-MNI152NLin6Asym_desc-smoothAROMAnonaggr_bold.nii.gz'
    tsv_file = fmriprepDir+PAR_NAME+'/func/'+PAR_NAME+'_task-rest_run-1_desc-confounds_timeseries.tsv'
   
    #run denoiser 
    # usage: run_denoise.py [-h] [--col_names COL_NAMES [COL_NAMES ...]] [--hp_filter HP_FILTER] [--lp_filter LP_FILTER] [--out_figure_path OUT_FIGURE_PATH] img_file tsv_file out_path
    subprocess.call(["python", denoiserLocation, img_file, tsv_file, out_path, "--col_names", "std_dvars", "dvars", "framewise_displacement", "rmsd", "a_comp_cor_00", "a_comp_cor_01", "a_comp_cor_02", "a_comp_cor_03", "a_comp_cor_04", "cosine00", "cosine01", "cosine02", "cosine03", "trans_x", "trans_y", "trans_z", "rot_x", "rot_y", "rot_z"])
   
    print('Finished:', i)
    #create text file for keeping track of finished participants
    file1 = open("DenoisedFinished.txt", "a+")
    file1.write("Finished:")
    file1.write(i)
    file1.write("\n")
    file1.close()

