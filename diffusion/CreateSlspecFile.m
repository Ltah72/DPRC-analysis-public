function CreateSlspecFile()
%Automatically create the slspec.txt file from the .json file from the main 
%dwi dataset. This only needs to be done from one participant, e.g. 
%sub-ADPRC0001F0. This function is not used in the main script, but can be 
%used to make the slspec.txt file, which is stored in the 'files' folder.   

%The code for this can be found on the FSL eddy wiki faq page: 
%https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/eddy/Faqeddy/Faq


%define variables for the file
PAR_NAME = input('Please name one participant from your analysis, e.g., sub-ADPRC0001F0: ', 's');
%define dwi datafile
datafile = '_acq_data_dwi';

%open & modfiy the file
fp = fopen([PAR_NAME, datafile, '.json'],'r');
%can also just modify code here instead, e.g., fp = fopen('sub-ADPRC0001F0_acq_data_dwi.json','r');
fcont = fread(fp);
fclose(fp);
cfcont = char(fcont');
i1 = strfind(cfcont,'SliceTiming');
i2 = strfind(cfcont(i1:end),'[');
i3 = strfind(cfcont((i1+i2):end),']');
cslicetimes = cfcont((i1+i2+1):(i1+i2+i3-2));
slicetimes = textscan(cslicetimes,'%f','Delimiter',',');
[sortedslicetimes,sindx] = sort(slicetimes{1});
mb = length(sortedslicetimes)/(sum(diff(sortedslicetimes)~=0)+1);
slspec = reshape(sindx,[mb length(sindx)/mb])'-1;
dlmwrite('DPRC_slspec.txt',slspec,'delimiter',' ','precision','%3d');

end

