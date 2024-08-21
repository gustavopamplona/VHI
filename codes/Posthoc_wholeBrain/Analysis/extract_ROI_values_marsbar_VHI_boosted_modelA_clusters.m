% ROI analysis for Model A (syringe block) - using boosted images and not
% using marsbar - for each run and including illusion indication

% posthoc whole-brain analysis clusters

clear

% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelA\1-MainEffect_Stim\Clusters\Selected\nifti';
% roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelA\2-MainEffect_Vis\Clusters\Selected\nifti';
roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelA\3-Interaction\Clusters\Selected\nifti';
roi_folder=dir(roinifti_folder);
roi_folder(1:2)=[];
% n_rois=length(roi_folder);
n_rois=1;
listROIs=dir([roinifti_folder filesep '*.nii']);

data_folder='D:\VHI\Data';
subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

labels={'subj' 'run' 'stim' 'vis' 'illusion' 'value'};

subjectiveMeasures = readtable('D:\VHI\Analysis\SubjectiveMeasures\Results\table_subjectiveMeasures_VHI.xlsx');
subjectiveMeasuresCell = table2cell(subjectiveMeasures);

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
                
                conFile=[data_folder filesep subject_folder(subj).name '\Func' filesep 'Func' num2str(run) filesep '1stLevel_movCor2_5s_2\boosted_' num2str(con) '.nii'];
                
                if exist(conFile, 'file') == 2
                    
                    conImage=spm_read_vols(spm_vol(conFile));
                    
                    meanCon=nanmean(conImage(idx));
                    
                    illusion = ~isnan(cell2mat(subjectiveMeasuresCell(k,10)));
                    
                    X=[num2cell(subj) num2cell(run) cellstr(stim_str) cellstr(vis_str) num2cell(+illusion) num2cell(meanCon)];
                    
                else
                    meanCon='';
                    illusion='';
                    X=[num2cell(subj) num2cell(run) cellstr(stim_str) cellstr(vis_str) cellstr(illusion) cellstr(meanCon)];
                end
                
                mat_values=[mat_values;X];
                
                clear X meanCon illusion
                k = k+1;
                
            end
            
            waitbar((subj+(roi-1)*n_subj)/(n_subj*n_rois),h)
            
        end
        
    end
    
    mat_values=[labels;mat_values];
    
    if nnz(num2str(roi))==1
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelA\3-Interaction\Clusters\Selected\tables\table_roi0' num2str(roi) '.xlsx'],mat_values);
    else
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Posthoc_wholeBrain\ModelA\3-Interaction\Clusters\Selected\tables\table_roi' num2str(roi) '.xlsx'],mat_values);
    end
end
delete(h)