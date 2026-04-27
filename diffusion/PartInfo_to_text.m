%Print participant demographics information onto a text file. 
%(e.g, Participant ID, scan date, DOB, sex). This script will obtain 
%participant demographics information by reading it off from the DICOM 
%files.     


%Author: Lenore Tahara-Eckl
%Email: Ltah262@aucklanduni.ac.nz
%Date: 28/09/20

clc;
clear all;
close all;


%user input - specify file locations
ParticipantListLocation = input('Where would you like to keep the participant list? ', 's'); 
%e.g. C:\Users\ltah262\Desktop
%or pwd for current directory
ParticipantsDicomDataLocation = input('Where is the dicom data located? ', 's');
%e.g. I:\Psychology\DRC_DementiaResearchClinic

%go to directory to print the list
cd (ParticipantListLocation);

%create a new text file and put in header lines
fid = fopen('DPRC_participant_list.txt', 'w');
if (fid == -1)
    disp('Error in creating the text file.')
else
    fprintf(fid, '%s       %s    %s          %s', 'Participant', 'scan_date', 'DOB', 'sex');
    fclose(fid);
end

%go to dicom data location
cd (ParticipantsDicomDataLocation);

%include all ADPRC participants - modify this according to your participant files names.
ADPRC_count = dir(['ADPRC_0*']);

%organise and extract specific data from each participant
for i = 1:length(ADPRC_count)
    
    %get participant ID
    PAR_NAME = getfield(ADPRC_count, {i}, 'name');
    
    %get information from the dicom data from that participant
    S = dir(PAR_NAME);
    %exclude data and note it down if there are not enough dicom files
    if length(S) < 2000 %most participants have over 2000 files in their dicom folder
        scanDate = 'EXCLUDED';
        DOB = 'EXCLUDED';
        sex = 'EXCLUDED';
    else
        MRFile_DicomData = S(10).name;
        %go into the current participant's folder
        cd (PAR_NAME);
        header=dicominfo(MRFile_DicomData);
        
        %extract participant's scan data
        scanDate = header.StudyDate;
        %reformat the text
        scanDate = ([scanDate(1:4) '/', scanDate(5:6) '/', scanDate(7:8)]);
        
        %extract participant's DOB
        DOB = header.PatientBirthDate;
        %reformat the text
        DOB = ([DOB(1:4) '/', DOB(5:6) '/', DOB(7:8)]);
        
        %extract participant's sex
        sex = header.PatientSex;
    end
    %go to directory to print the list
    cd (ParticipantListLocation);
    
    %print all variables to the file you created earlier
    fid2 = fopen('DPRC_participant_list.txt', 'a+');
    if (fid2 == -1)
        disp('Error in opening DPRC_participant_list.txt')
    else
        fprintf(fid2, '\n');
        fprintf(fid2, '%s     %s   %s   %s', PAR_NAME, scanDate, DOB, sex);
    end
    
    %go back to the dicom data location
    cd (ParticipantsDicomDataLocation);

end 
