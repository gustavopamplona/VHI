% ROI analysis for Model B (second half) - for each run and including illusion indication


% posthoc whole-brain analysis clusters

clear

% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\07-Interaction time x stim x vis\Clusters\Selected\nifti';
% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\05-Interaction time x vis\Clusters\Selected\nifti';
% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\04-Interaction time x stim\Clusters\Selected\nifti';
% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\03-Main effect of vis\Clusters\Selected\nifti';
roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\01-Main effect of time\Clusters\Selected\nifti';
roi_folder=dir(roinifti_folder);
roi_folder(1:2)=[];
% n_rois=length(roi_folder);
n_rois=5;
listROIs=dir([roinifti_folder filesep '*.nii']);

data_folder='D:\VHI\Data';
subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

labels={'subj' 'run' 'stim' 'vis' 'time' 'illusion' 'value'};

subjectiveMeasures = readtable('D:\VHI\Analysis\SubjectiveMeasures\Results\table_subjectiveMeasures_VHI.xlsx');
subjectiveMeasuresCell = table2cell(subjectiveMeasures);
subjectiveMeasuresMat = cell2mat(subjectiveMeasuresCell(:,10));
subjectiveMeasuresMatDoubled = repelem(subjectiveMeasuresMat,2,1);

h = waitbar(0,sprintf('running %d jobs',n_subj*n_rois)); % set up the 'waitbar'

for roi = 1:n_rois
    
    k = 1;
    mat_values=[];
    
    roi_file=[roinifti_folder filesep listROIs(1).name];
    
    roi_image=spm_read_vols(spm_vol(roi_file));
    
    idx=find(roi_image==roi);
    
    for subj=first_subj:n_subj
        
        roi
        subj
        
        for run = 1:4
            
            for con = 1:6
                
                if con <= 3
                    stim_str = 'sync';
                else
                    stim_str = 'async';
                end
                
                if mod(con,3) == 1
                    vis_str = 'high';
                elseif mod(con,3) == 2
                    vis_str = 'mid';
                else
                    vis_str = 'low';
                end
                
                for time = 1:2
                    
                    if time == 1
                        conFile=[data_folder filesep subject_folder(subj).name '\Func' filesep 'Func' num2str(run) filesep '1stLevel_modelB3\beta_000' num2str(con) '.nii'];
                        time_str = 'firstHalf';
                    elseif time == 2
                        if nnz(num2str(con+6)) == 1
                            conFile=[data_folder filesep subject_folder(subj).name '\Func' filesep 'Func' num2str(run) filesep '1stLevel_modelB3\beta_000' num2str(con+6) '.nii'];
                        else
                            conFile=[data_folder filesep subject_folder(subj).name '\Func' filesep 'Func' num2str(run) filesep '1stLevel_modelB3\beta_00' num2str(con+6) '.nii'];
                        end
                        time_str = 'secondHalf';
                    end
                    
                    if exist(conFile, 'file') == 2
                        
                        conImage=spm_read_vols(spm_vol(conFile));
                        
                        meanCon=nanmean(conImage(idx));
                        
                        illusion = ~isnan(subjectiveMeasuresMatDoubled(k));
                        
                        X=[num2cell(subj) num2cell(run) cellstr(stim_str) cellstr(vis_str) cellstr(time_str) num2cell(+illusion) num2cell(meanCon)];
                        
                    else
                        meanCon='';
                        illusion='';
                        X=[num2cell(subj) num2cell(run) cellstr(stim_str) cellstr(vis_str) cellstr(time_str) cellstr(illusion) cellstr(meanCon)];
                    end
                    
                    mat_values=[mat_values;X];
                    
                    clear X meanCon illusion
                    k = k+1;
                end
            end
            
            waitbar((subj+(roi-1)*n_subj)/(n_subj*n_rois),h)
            
        end
        
    end
    
    mat_values=[labels;mat_values];
    
    if nnz(num2str(roi))==1
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\01-Main effect of time\Clusters\Selected\tables\table_roi0' num2str(roi) '.xlsx'],mat_values);
    else
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelB\01-Main effect of time\Clusters\Selected\tables\table_roi' num2str(roi) '.xlsx'],mat_values);
    end
end
delete(h)