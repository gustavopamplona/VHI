% ROI analysis for Model A (syringe block)

clear

data_folder='D:\VHI\Data';
roi_folder='D:\VHI\Analysis\ROI_analysis\ROIs';

subject_folder=dir(data_folder);
subject_folder(1:4)=[];
n_subj=length(subject_folder);
first_subj=1;

labels={'subj' 'run' 'stim' 'vis' 'hrf' 'value'};

roi_files=dir(fullfile(roi_folder, '*.mat'));
n_rois=length(roi_files);

h = waitbar(0,sprintf('running %d jobs',n_subj*n_rois)); % set up the 'waitbar'

stim_cell={'sync';'sync';'sync';'sync';'sync';'sync';'async';'async';'async';'async';'async';'async'};
vis_cell={'high';'high';'mid';'mid';'low';'low';'high';'high';'mid';'mid';'low';'low'};
hrf_cell={'canonical';'temporal';'canonical';'temporal';'canonical';'temporal';'canonical';'temporal';'canonical';'temporal';'canonical';'temporal'};

for roi=1:n_rois
    
    Z=[];
    
    roi_file=[roi_folder filesep roi_files(roi).name(1:end-4) '.mat'];
    R  = maroi(roi_file); % Make marsbar ROI object
    
    for subj=first_subj:n_subj
        
        betas_allruns=[];
        mean_cons=[];
        
        for run=1:4
            
            spmFile=[data_folder filesep subject_folder(subj).name filesep 'Func\Func' num2str(run) filesep '1stLevel_movCor2_5s_2' filesep 'SPM.mat'];
            
            if exist(spmFile, 'file') == 2
                
                D  = mardo(spmFile); % Make marsbar design object
                Y  = get_marsy(R, D, 'mean'); % Fetch data into marsbar data object
                xCon = get_contrasts(D); % Get contrasts from original design
                E = estimate(D, Y); % Estimate design on ROI data
                E = set_contrasts(E, xCon); % Put contrasts from original design back into design object
                b = betas(E); % get design betas
                marsS = compute_contrasts(E, 11:22); % get stats and stuff for all contrasts into statistics structure
                
                beta_values=marsS.con; % contrasts canonical and temporal for each combination of levels
                
            end
            
            subj_vec= num2cell(subj*ones(length(beta_values),1));
            run_vec = num2cell(run*ones(length(beta_values),1));
            value_vec=num2cell(beta_values);
            
            X=[subj_vec run_vec stim_cell vis_cell hrf_cell value_vec];
            
            Z=[Z;X];
            
            clear X subj_vec value_vec
            
            waitbar((subj+(roi-1)*n_subj)/(n_subj*n_rois),h)
            
        end
    end
    
    Z=[labels;Z];
    
    xlswrite(['D:\VHI\Analysis\ROI_analysis\Tables\table_' roi_files(roi).name(1:end-4) '.xlsx'],Z);
    
end
delete(h)