% ROI analysis for Model A (syringe block) - using boosted images and not
% using marsbar

clear

roinifti_folder = 'D:\VHI\Analysis\ROI_analysis\ROIs\ROIs_6mm\Nifti';
roi_folder=dir(roinifti_folder);
roi_folder(1:2)=[];
% n_rois=length(roi_folder);
n_rois=21;
listROIs=dir([roinifti_folder filesep '*.nii']);

data_folder='D:\VHI\Data';
subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

labels={'subj' 'stim' 'vis' 'value'};

j = 0;
h = waitbar(0,sprintf('running %d jobs',n_subj*n_rois)); % set up the 'waitbar'

for roi = 18:n_rois
% for roi = 19
    mat_values=[];
    
    roi_file=[roinifti_folder filesep listROIs(1).name];
    
    roi_image=spm_read_vols(spm_vol(roi_file));
    
    idx=find(roi_image==roi);
    
    for subj=first_subj:n_subj
        
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
            
            conFile=[data_folder filesep subject_folder(subj).name filesep '1stLevel_movCor2_5s_2\boosted_' num2str(con+5) '.nii'];
            
            if exist(conFile, 'file') == 2
                
                conImage=spm_read_vols(spm_vol(conFile));
                
                meanCon=nanmean(conImage(idx));
                
                X=[num2cell(subj) cellstr(stim_str) cellstr(vis_str) num2cell(meanCon)];
                
            else
                meanCon='';
                X=[num2cell(subj) cellstr(stim_str) cellstr(vis_str) cellstr(meanCon)];
            end
            
            mat_values=[mat_values;X];
            
            clear X meanCon
            
        end
        
        j=j+1;
        
        waitbar((subj+(roi-1)*n_subj)/(n_subj*n_rois),h)
        
    end
    
    mat_values=[labels;mat_values];
    
    if nnz(num2str(roi))==1
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Tables\ModelA\1stLevel_movCor2_5s_2_6mm\table_roi0' num2str(roi) '.xlsx'],mat_values);
    else
        xlswrite(['D:\VHI\Analysis\ROI_analysis\Tables\ModelA\1stLevel_movCor2_5s_2_6mm\table_roi' num2str(roi) '.xlsx'],mat_values);
    end
end
delete(h)