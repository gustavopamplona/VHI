% Average ANOVA contrasts and simple contrasts for contrasts across runs - VHI study - Second half stimulation block

% Gustavo Pamplona, 26.01.23

clear

data_folder='D:\VHI\Data';
n_cons=11;

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
        
        directory = [data_folder '\' subject_folder(subj).name '\1stLevel_modelB3'];
        
        mkdir([data_folder '\' subject_folder(subj).name '\1stLevel_modelB3'])
        
        subject_folder(subj).name
        
        % batch
        load('D:\VHI\Analysis\ModelB\batch_averageContrasts_VHI_modelB.mat')
        
        for con=1:n_cons
            
            if length(num2str(con))==1
                image1=[data_folder filesep subject_folder(subj).name filesep 'Func\Func1\1stLevel_modelB3\con_000' num2str(con) '.nii,1'];
                image2=[data_folder filesep subject_folder(subj).name filesep 'Func\Func2\1stLevel_modelB3\con_000' num2str(con) '.nii,1'];
                image3=[data_folder filesep subject_folder(subj).name filesep 'Func\Func3\1stLevel_modelB3\con_000' num2str(con) '.nii,1'];
                if n_runs == 4
                    image4=[data_folder filesep subject_folder(subj).name filesep 'Func\Func4\1stLevel_modelB3\con_000' num2str(con) '.nii,1'];
                end
            else
                image1=[data_folder filesep subject_folder(subj).name filesep 'Func\Func1\1stLevel_modelB3\con_00' num2str(con) '.nii,1'];
                image2=[data_folder filesep subject_folder(subj).name filesep 'Func\Func2\1stLevel_modelB3\con_00' num2str(con) '.nii,1'];
                image3=[data_folder filesep subject_folder(subj).name filesep 'Func\Func3\1stLevel_modelB3\con_00' num2str(con) '.nii,1'];
                if n_runs == 4
                    image4=[data_folder filesep subject_folder(subj).name filesep 'Func\Func4\1stLevel_modelB3\con_00' num2str(con) '.nii,1'];
                end
            end
                        
            matlabbatch{1,con}.spm.util.imcalc.input{1,1}=image1;
            matlabbatch{1,con}.spm.util.imcalc.input{2,1}=image2;
            matlabbatch{1,con}.spm.util.imcalc.input{3,1}=image3;
            if n_runs == 4
                matlabbatch{1,con}.spm.util.imcalc.input{4,1}=image4;
            end
            matlabbatch{1, con}.spm.util.imcalc.outdir{1,1}=directory;
        end
        
        spm_jobman('run',matlabbatch);
        
        clear image1 image2 image3 image4 directory
    end
end