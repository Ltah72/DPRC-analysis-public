# DPRC-analysis

Author: Lenore Tahara-Eckl 
Email: Ltah262@aucklanduni.ac.nz

These scripts have been created for analysing and visualising the neuroimaging (e.g. diffusion MRI (dMRI), functional MRI (fMRI)) and neuropsychological data, which has been made for public usage. Examples are made in reference to the Dementia Prevention Research Clinic (DPRC), at the University of Auckland, New Zealand. Modifications which are needed are indicated in comments. 

Regarding dMRI data processing, please read manuals (.docx files, located in the 'diffusion/manuals' folder) to learn more about the scripts generated for this, with reference to the DPRC dMRI data analysis. Specific diffusion analysis include preprocessing/cleaning dMRI data, constrained spherical deconvolution (CSD), fixel-based analysis (FBA), and creating a structural diffusion connectome. Scripts are written mainly in Matlab R2020b, and in R (version 4.0.3) and Python (version 3.8.3), functioning with both code and a wrapper which primarily calls commands from MRtrix3 (Tournier et al., 2019; https://www.mrtrix.org/), and also from FMRIB Software Library (FSL) (Jenkinson, Beckmann, Behrens, Woolrich, & Smith, 2012; https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/FslInstallation). Other softwares programmes / GitHub repositories which are utilised are Advanced Normalisation Tools (ANTs) (Avants et al., 2011; http://stnava.github.io/ANTs/), FreeSurfer (https://surfer.nmr.mgh.harvard.edu/), fmriprep (Esteban et al., 2019; https://fmriprep.org/en/stable/; version 20.2.1), and Denoiser (https://github.com/arielletambini/denoiser) (these must all be downloaded / installed for use).

Please visit websites of the above software programmes and cite the original authors of the programme, if you wish to use this.
