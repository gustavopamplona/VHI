% Gustavo Pamplona 25.01.2023

% Creates the vectors for onsets and durations for the VHI experiment -
% second half stimulation block

clear

data_folder='D:\VHI\Data';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

remove_subjs = {'S10','S18','S19'};

for subj=first_subj:n_subj
    
    subject_folder(subj).name
    
    if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
        
        func_folder=dir([data_folder '\' subject_folder(subj).name '\Func']);
        func_folder(1:2)=[];
        n_runs=length(func_folder);
        
        path_data=[data_folder filesep subject_folder(subj).name filesep 'Psych'];
        mkdir([data_folder filesep subject_folder(subj).name],'Cond')
        
        for run=1:n_runs
            
            data_files=dir([path_data '\result*.mat']);
            
            load([path_data filesep data_files(run).name])
            
            [names,onsets,durations]=compute_multiple_VHI_modelB(T);
            
            filename=[data_folder filesep subject_folder(subj).name filesep 'Cond' filesep subject_folder(subj).name '_run' num2str(run) '_B2.mat'];
            save(filename,'names','onsets','durations')
            
        end
    end
end