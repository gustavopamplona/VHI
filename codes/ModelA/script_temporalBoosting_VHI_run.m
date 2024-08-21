% Boost hrf canonical images with temporal derivatives - VHI study -
% Syringe block - for each run

% Gustavo Pamplona, 09.12.22

clear

data_folder='D:\VHI\Data';
n_cons=6;

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

remove_subjs = {'S10','S18','S19','S32','S39'};

pathDayRun{1}='Func1';
pathDayRun{2}='Func2';
pathDayRun{3}='Func3';
pathDayRun{4}='Func4';

for subj=first_subj:n_subj
    
    if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
        
        for run = 1:4
            
            if exist([data_folder filesep subject_folder(subj).name '\Func' filesep pathDayRun{run} filesep '1stLevel_movCor2_5s_2\con_0001.nii'], 'file') == 2
                
                for con=1:n_cons
                    
                    con1=[data_folder filesep subject_folder(subj).name '\Func' filesep pathDayRun{run} filesep '1stLevel_movCor2_5s_2\con_00' num2str(con*2-1+10) '.nii'];
                    con2=[data_folder filesep subject_folder(subj).name '\Func' filesep pathDayRun{run} filesep '1stLevel_movCor2_5s_2\con_00' num2str(con*2+10) '.nii'];

                    filename = [data_folder filesep subject_folder(subj).name '\Func' filesep pathDayRun{run} filesep '1stLevel_movCor2_5s_2\boosted_' num2str(con) '.nii'];
                    
                    spm_deriv(con1, con2, filename);
                    
                    clear con1 con2 filename
                    
                end
            end
            
        end
    end
end