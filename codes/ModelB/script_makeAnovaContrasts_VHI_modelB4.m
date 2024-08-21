% Compute ANOVA contrasts and simple contrasts for canonical and temporal
% contrasts - VHI study - Second half stimulation block - model B

% Gustavo Pamplona, 26.01.23

% update 07.06.2023: time included as a factor

clear

data_folder='D:\VHI\Data';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=33;

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
            
            SPMfile=[data_folder filesep subject_folder(subj).name filesep 'Func\Func' num2str(run) filesep '1stLevel_modelB3\SPM.mat'];
            
            % batch
            load('D:\VHI\Analysis\ModelB\batch_makeAnovaContrasts_VHI_modelB4.mat')
            
            matlabbatch{1,1}.spm.stats.con.spmmat{1,1}=SPMfile;
            
            spm_jobman('run',matlabbatch);
            
            clear SPMfile
            
        end
    end
end