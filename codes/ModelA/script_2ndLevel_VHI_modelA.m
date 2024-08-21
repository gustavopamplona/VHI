% 2nd Level, model A - VHI study - syringe block

% Gustavo Pamplona, 15.02.23

clear

data_folder='D:\VHI\Data';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

remove_subjs = {'S10','S18','S19','S32','S39'};

% covariates_file = ['D:\VHI\2ndLevel_results' filesep 'covariates.xlsx'];
% covariates_mat=cell2mat(table2cell(readtable(covariates_file)));

for cond = 1:9
    
    if (cond ~= 2 & cond ~= 3) % if not conditions 2 or 3
        load('D:\VHI\Analysis\ModelA\batch_2ndLevel_VHI_modelA4.mat')
    else
        load('D:\VHI\Analysis\ModelA\batch_2ndLevel_VHI_modelA5.mat')
    end
    
    if cond == 1
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\1-Main effect of sync';
        contrast_name='Main effect of sync';
        con_name='boosted_1';
    elseif cond == 2
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\2-Main effect of vis';
        contrast_name='Main effect of vis';
        con_name1='boosted_2';
        con_name2='boosted_3';
    elseif cond == 3
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\3-Interaction';
        contrast_name='Interaction';
        con_name1='boosted_4';
        con_name2='boosted_5';
    elseif cond == 4
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\4-sync high';
        contrast_name='sync high';
        con_name='boosted_6';
    elseif cond == 5
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\5-sync med';
        contrast_name='sync med';
        con_name='boosted_7';
    elseif cond == 6
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\6-sync low';
        contrast_name='sync low';
        con_name='boosted_8';
    elseif cond == 7
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\7-async high';
        contrast_name='async high';
        con_name='boosted_9';
    elseif cond == 8
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\8-async med';
        contrast_name='async med';
        con_name='boosted_10';
    elseif cond == 9
        matlabbatch{1,1}.spm.stats.factorial_design.dir{1,1}='D:\VHI\2ndLevel_results\ModelA\9-async low';
        contrast_name='async low';
        con_name='boosted_11';
    end
    
    k=1;
    subj_idx=0;
    cell_subjs=[];
    if (cond ~= 2 & cond ~= 3) % if not conditions 2 or 3
        for subj=first_subj:n_subj
            
            if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
                
                subj_idx=subj_idx+1;
                
                con_file = [data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\' con_name '.nii'];
                
                if exist(con_file, 'file')
                    cell_subjs{k,1} = con_file;
%                     cell_age{k,1} = covariates_mat(subj_idx,1);
%                     cell_handedness{k,1} = covariates_mat(subj_idx,2);
                    k=k+1;
                end
            end
        end
        matlabbatch{1,1}.spm.stats.factorial_design.des.t1.scans = cell_subjs;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(1).c = cell2mat(cell_age);
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(1).cname = 'age';
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(2).c = cell2mat(cell_handedness);
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(2).cname = 'handedness';
        matlabbatch{1,3}.spm.stats.con.consess{1,1}.tcon.name=contrast_name;
    else % if conditions 2 or 3
        for subj=first_subj:n_subj
            
            if ~nnz(strcmp(subject_folder(subj).name,remove_subjs))
                
                subj_idx=subj_idx+1;
                
                con_file1 = [data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\' con_name1 '.nii'];
                con_file2 = [data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_3\' con_name2 '.nii'];
                
                if exist(con_file, 'file')
                    cell_subjs1{k,1} = con_file1;
                    cell_subjs2{k,1} = con_file2;
%                     mat_age(k,1) = covariates_mat(subj_idx,1);
%                     cell_handedness{k,1} = covariates_mat(subj_idx,2);
%                     mat_handedness(k,1) = covariates_mat(subj_idx,2);
                    k=k+1;
                end
            end
        end
%         cell_age1=[mat_age;zeros(length(mat_age),1)];
%         cell_age1=[mat_age;mat_age];
%         cell_age2=[zeros(length(mat_age),1);mat_age];
%         cell_handedness1=[mat_handedness;zeros(length(mat_handedness),1)];
%         cell_handedness1=[mat_handedness;mat_handedness];
%         cell_handedness2=[zeros(length(mat_handedness),1);mat_handedness];
        matlabbatch{1,1}.spm.stats.factorial_design.des.anova.icell(1).scans=cell_subjs1;
        matlabbatch{1,1}.spm.stats.factorial_design.des.anova.icell(2).scans=cell_subjs2;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(1).c=cell_age1;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(2).c=cell_age2;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(3).c=cell_handedness1;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(2).c=cell_handedness1;
%         matlabbatch{1,1}.spm.stats.factorial_design.cov(4).c=cell_handedness2;
        matlabbatch{1,3}.spm.stats.con.consess{1,1}.fcon.name=contrast_name;
    end
    
    spm_jobman('run',matlabbatch);
    
    clear cell_subjs cell_subjs1 cell_subjs2 cell_age cell_age1 cell_age2 cell_handedness cell_handedness1 cell_handedness2
    
end