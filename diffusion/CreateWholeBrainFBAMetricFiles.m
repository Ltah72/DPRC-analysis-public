function CreateWholeBrainFBAMetricFiles(participants)
%This function will create text files which will have all participants
%whole-brain FBA metrics (FD, log FC, and FDC). Specifically, the mean,
%median, std, std_rv (standard error), min, max, and count will be
%displayed per each metric, per each participant. This will be used for
%group comparisons.

%   Inputs(1) - participants: this will give the number of participants to 
%               calculate the metrics per each of them. 


%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 30/07/2020


%create headers for each text file
%for FD
fid6 = fopen('FD.txt', 'w');
if (fid6 == -1)
    disp('Error in creating the text file.')
else
    fprintf(fid6, '%s       %s          %s      %s  %s %s   %s', 'mn', 'md', 'std', 'std_rv', 'min', 'max', 'count');
    fprintf(fid6, '\n');
    fclose(fid6);
end

%for log FC
fid7 = fopen('FC_log.txt', 'w');
if (fid7 == -1)
    disp('Error in creating the text file.')
else
    fprintf(fid7, '%s         %s           %s        %s   %s      %s     %s', 'mn', 'md', 'std', 'std_rv', 'min', 'max', 'count');
    fprintf(fid7, '\n');
    fclose(fid7);
end

%for FDC
fid8 = fopen('FDC.txt', 'w');
if (fid8 == -1)
    disp('Error in creating the text file.')
else
    fprintf(fid8, '%s       %s          %s      %s  %s %s   %s', 'mn', 'md', 'std', 'std_rv', 'min', 'max', 'count');
    fprintf(fid8, '\n');
    fclose(fid8);
end

%print results onto the text files
for i = 1:length(participants)
    
    %full_name = participants(i).name;
    %PAR_NAME = full_name(1:15);
    
    full_name = participants{i};
    PAR_NAME = full_name(50:64); %modify to fit the participant name
    
    %for FD
    fid6 = fopen('FD.txt', 'a+');
    if (fid6 == -1)
        disp('Error in opening the text file.')
    else
        unix(['mrstats template/fd_smooth/' PAR_NAME '_fd.mif -output mean -output median -output std -output std_rv -output min -output max -output count >> FD.txt']);
        fclose(fid6);
    end
    
    %for FC_log
    fid7 = fopen('FC_log.txt', 'a+');
    if (fid7 == -1)
        disp('Error in opening the text file.')
    else
        unix(['mrstats template/log_fc_smooth/' PAR_NAME '_log_fc.mif -output mean -output median -output std -output std_rv -output min -output max -output count >> FC_log.txt']);
        fclose(fid7);
    end
    
    %for FDC
    fid8 = fopen('FDC.txt', 'a+');
    if (fid8 == -1)
        disp('Error in opening the text file.')
    else
        unix(['mrstats template/fdc_smooth/' PAR_NAME '_fdc.mif -output mean -output median -output std -output std_rv -output min -output max -output count >> FDC.txt']);
        fclose(fid8);
    end
    
end
end

