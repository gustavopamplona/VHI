% Boost hrf canonical images with temporal derivatives - VHI study - Syringe block

% Gustavo Pamplona, 09.12.22

clear

data_folder='D:\VHI\Data';
n_cons=22;

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=32;

remove_subjs = {'S10','S18','S19','S32','S39'};

pathDayRun{1}='Func1';
pathDayRun{2}='Func2';
pathDayRun{3}='Func3';
pathDayRun{4}='Func4';

for subj=first_subj:n_subj
    
    if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
        
        for con=1:2:n_cons
        
            if length(num2str(con))==1
                con1=[data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\mean_con_000' num2str(con) '.nii'];
            else
                con1=[data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\mean_con_00' num2str(con) '.nii'];
            end
            if length(num2str(con+1))==1
                con2=[data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\mean_con_000' num2str(con+1) '.nii'];
            else
                con2=[data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\mean_con_00' num2str(con+1) '.nii'];
            end
            filename = [data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\boosted_' num2str((con+1)/2) '.nii'];
            
            spm_deriv(con1, con2, filename);
            
            clear con1 con2 filename
            
        end
    end
end