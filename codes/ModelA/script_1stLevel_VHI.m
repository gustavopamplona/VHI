% Define and estimate 1st level betas for VHI - syringe block

% Gustavo Pamplona, 06.12.22

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
            
            directory=[data_folder filesep subject_folder(subj).name filesep 'Func\Func' num2str(run) filesep '1stLevel_movCor2_5s_2'];
            func_source=[data_folder filesep subject_folder(subj).name filesep 'Func' filesep 'Func' num2str(run)];
            mult_cond=[data_folder filesep subject_folder(subj).name filesep 'Cond' filesep subject_folder(subj).name '_run' num2str(run) '_A3.mat'];
            movOutlier_file=dir([func_source '\art_regression_outliers_and_movement*.mat']);
            movOutlier_name=[func_source '\' movOutlier_file.name];
            
            %func
            func=[];
            func_path{1,:}=func_source;
            funcFiles=dir(char(func_path));
            for j=1:length(funcFiles)
                if length(funcFiles(j).name)>2
                    if strcmp(funcFiles(j).name(1,1:4),'swra')
                        funcName=[func_source '\' funcFiles(j).name];
                        len=length(spm_vol(funcName));
                        if len>330
                            len = 330;
                        end
                        if strcmp(subject_folder(subj).name,'S36') && run == 1
                            len = 322;
                        end
                        for j=1:len
                            func_cell{1,1}{j,1}=char([funcName ',' num2str(j)]);
                        end
                    elseif strcmp(funcFiles(j).name(1,1:2),'rp')
                        len = 330;
                        if strcmp(subject_folder(subj).name,'S36') && run == 1
                            len = 322;
                        end
                    end
                end
            end
            
            % batch
            load('D:\VHI\Analysis\ModelA\batch_1stLevel_VHI.mat')
            
            matlabbatch{1,1}.spm.stats.fmri_spec.dir{1,1}=directory;
            matlabbatch{1,1}.spm.stats.fmri_spec.sess.scans=func_cell{1};
            matlabbatch{1,1}.spm.stats.fmri_spec.sess.multi{1}=mult_cond;
            matlabbatch{1,1}.spm.stats.fmri_spec.sess.multi_reg{1}=movOutlier_name;
            
            spm_jobman('run',matlabbatch);
            
            clear func_cell cov_reg_file mult_cond
            
        end
    end
end