% 2nd Level, model B - VHI study - second half stimulation period

% Gustavo Pamplona, 16.02.23

% update 08.06.2023

clear

data_folder='D:\VHI\Data';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

remove_subjs = {'S10','S18','S19','S32','S39'};

covariates_file = ['D:\VHI\2ndLevel_results' filesep 'covariates.xlsx'];
covariates_mat=cell2mat(table2cell(readtable(covariates_file)));

for cond = 1:7
    
    if  ~(cond == 3 | cond >= 5) % if conditions 1, 2 3, or 5
        load('D:\VHI\Analysis\ModelB\batch_2ndLevel_VHI_modelB1.mat')
    else
        load('D:\VHI\Analysis\ModelB\batch_2ndLevel_VHI_modelB2.mat')
    end
    
    if cond == 1
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\01-Main effect of time';
        contrast_name='Main effect of time';
        con_name='mean_con_0001';
    elseif cond == 2
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\02-Main effect of stim';
        contrast_name='Main effect of stim';
        con_name='mean_con_0002';
    elseif cond == 3
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\03-Main effect of vis';
        contrast_name='Main effect of vis';
        con_name1='mean_con_0003';
        con_name2='mean_con_0004';
    elseif cond == 4
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\04-Interaction time x stim';
        contrast_name='Interaction time x stim';
        con_name='mean_con_0005';
    elseif cond == 5
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\05-Interaction time x vis';
        contrast_name='Interaction time x vis';
        con_name1='mean_con_0006';
        con_name2='mean_con_0007';
    elseif cond == 6
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\06-Interaction stim x vis';
        contrast_name='Interaction stim x vis';
        con_name1='mean_con_0008';
        con_name2='mean_con_0009';
    elseif cond == 7
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelB\07-Interaction time x stim x vis';
        contrast_name='Interaction time x stim x vis';
        con_name1='mean_con_0010';
        con_name2='mean_con_0011';
    end
    
    k=1;
    subj_idx=0;
    cell_subjs=[];
    if  ~(cond == 3 | cond >= 5) % if conditions 1, 2 3, or 5
        for subj=first_subj:n_subj
            
            if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
                
                subj_idx=subj_idx+1;
                
                con_file = [data_folder filesep subject_folder(subj).name filesep '1stLevel_modelB4\' con_name '.nii'];
                
                if exist(con_file, 'file')
                    cell_subjs{k,1} = con_file;
                    cell_age{k,1} = covariates_mat(subj_idx,1);
                    cell_handedness{k,1} = covariates_mat(subj_idx,2);
                    k=k+1;
                end
            end
        end
        matlabbatch{1,1}.spm.stats.factorial_design.des.t1.scans = cell_subjs;
        matlabbatch{1,1}.spm.stats.factorial_design.cov(1).c = cell2mat(cell_age);
        matlabbatch{1,1}.spm.stats.factorial_design.cov(1).cname = 'age';
        matlabbatch{1,1}.spm.stats.factorial_design.cov(2).c = cell2mat(cell_handedness);
        matlabbatch{1,1}.spm.stats.factorial_design.cov(2).cname = 'handedness';
        matlabbatch{1,3}.spm.stats.con.consess{1,1}.tcon.name=contrast_name;
    else 
        for subj=first_subj:n_subj
            
            if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
                
                subj_idx=subj_idx+1;
                
                con_file1 = [data_folder filesep subject_folder(subj).name filesep '1stLevel_modelB4\' con_name1 '.nii'];
                con_file2 = [data_folder filesep subject_folder(subj).name filesep '1stLevel_modelB4\' con_name2 '.nii'];
                
                if exist(con_file, 'file')
                    cell_subjs1{k,1} = con_file1;
                    cell_subjs2{k,1} = con_file2;
                    mat_age(k,1) = covariates_mat(subj_idx,1);
                    cell_handedness{k,1} = covariates_mat(subj_idx,2);
                    mat_handedness(k,1) = covariates_mat(subj_idx,2);
                    k=k+1;
                end
            end
        end
        cell_age1=[mat_age;mat_age];
        cell_handedness1=[mat_handedness;mat_handedness];
        matlabbatch{1,1}.spm.stats.factorial_design.des.anova.icell(1).scans=cell_subjs1;
        matlabbatch{1,1}.spm.stats.factorial_design.des.anova.icell(2).scans=cell_subjs2;
        matlabbatch{1,1}.spm.stats.factorial_design.cov(1).c=cell_age1;
        matlabbatch{1,1}.spm.stats.factorial_design.cov(2).c=cell_handedness1;
        matlabbatch{1,3}.spm.stats.con.consess{1,1}.fcon.name=contrast_name;
    end
    
    spm_jobman('run',matlabbatch);
    
    clear cell_subjs cell_subjs1 cell_subjs2 cell_age cell_age1 cell_age2 cell_handedness cell_handedness1 cell_handedness2
    
end