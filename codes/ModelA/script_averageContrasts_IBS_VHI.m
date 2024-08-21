% Average ANOVA contrasts and simple contrasts for canonical and temporal contrasts across runs - VHI study - Syringe block

% Gustavo Pamplona, 09.12.22

clear

data_folder='D:\VHI\Data';
n_cons=22;

first_subj=1;

pathDayRun{1}='Func1';
pathDayRun{2}='Func2';
pathDayRun{3}='Func3';
pathDayRun{4}='Func4';

movementTable = xlsread('C:\Gustavo\Dropbox\Postdoc\Project2 - Virtual-hand illusion\Responders and movement.xlsx');

n_subj=max(movementTable(:,1));

for subj=first_subj:n_subj
    
    if nnz(num2str(subj))==1
        subj_str=['S0' num2str(subj)];
    else
        subj_str=['S' num2str(subj)];
    end
    
    func_folder=dir([data_folder '\' subj_str '\Func']);
    if ~isempty(func_folder)
        func_folder(1:2)=[];
        n_runs=length(func_folder);
        
        directory = [data_folder '\' subj_str '\1stLevel_movCor2_5s_3'];
        mkdir([data_folder '\' subj_str '\1stLevel_movCor2_5s_3'])
        
        % batch
        load('D:\VHI\Analysis\ModelA\batch_averageContrasts_IBS_VHI.mat')
        
        subj_str
        
        for con=1:n_cons
            
            k = 1;
            
            image_filename=[];
            
            for run = 1:4
                
                row=find(movementTable(:,1)==subj & movementTable(:,2)==run); % finds subject and run
                
                if movementTable(row,5)==0 % checks if the subject moved too much in the run
                    if length(num2str(con))==1
                        image_filename{k,1} = [data_folder filesep subj_str filesep 'Func\Func' num2str(run) '\1stLevel_movCor2_5s_2\con_000' num2str(con) '.nii,1'];
                    else
                        image_filename{k,1} = [data_folder filesep subj_str filesep 'Func\Func' num2str(run) '\1stLevel_movCor2_5s_2\con_00' num2str(con) '.nii,1'];
                    end
                    k=k+1;
                end
            end
            
            matlabbatch{1,con}.spm.util.imcalc.outdir{1,1}=directory;
            
            if ~isempty(image_filename)
                if length(image_filename) == 1
                    matlabbatch{1,con}.spm.util.imcalc.expression = 'i1';
                elseif length(image_filename) == 2
                    matlabbatch{1,con}.spm.util.imcalc.expression = '(i1+i2)/2';
                elseif length(image_filename) == 3
                    matlabbatch{1,con}.spm.util.imcalc.expression = '(i1+i2+i3)/3';
                elseif length(image_filename) == 4
                    matlabbatch{1,con}.spm.util.imcalc.expression = '(i1+i2+i3+i4)/4';
                end
                matlabbatch{1,con}.spm.util.imcalc.input=image_filename;
            end
        end
        
        if ~isempty(image_filename)
            spm_jobman('run',matlabbatch);
        end
        clear directory image_filename
    end
end