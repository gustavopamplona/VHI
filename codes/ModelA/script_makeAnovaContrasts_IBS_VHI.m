% Compute ANOVA contrasts and simple contrasts for canonical and temporal contrasts - VHI study - Syringe block

% Gustavo Pamplona, 09.12.22

clear

data_folder='D:\VHI\Data';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

remove_subjs = {'S10','S18','S19'};

pathDayRun{1}='Func1';
pathDayRun{2}='Func2';
pathDayRun{3}='Func3';
pathDayRun{4}='Func4';

for subj=first_subj:n_subj
    
    if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
        
        func_folder=dir([data_folder '\' subject_folder(subj).name '\Func']);
        func_folder(1:2)=[];
        n_runs=length(func_folder);
        
        for run=1:n_runs
            
            subject_folder(subj).name
            run
            
            SPMfile=[data_folder filesep subject_folder(subj).name filesep 'Func\Func' num2str(run) filesep '1stLevel_movCor2_5s_2\SPM.mat'];
            
            % batch
            load('D:\VHI\Analysis\ModelA\batch_makeAnovaContrasts_IBS_VHI.mat')
            
            matlabbatch{1,1}.spm.stats.con.spmmat{1,1}=SPMfile;
            
            spm_jobman('run',matlabbatch);
            
            clear SPMfile
            
        end
    end
end