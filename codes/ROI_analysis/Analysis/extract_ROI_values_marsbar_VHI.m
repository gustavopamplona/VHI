clear

data_folder='D:\VHI\Data\Pilot';
roi_folder='D:\VHI\SampleSizeDetermination\ROIs';

subject_folder=dir(data_folder);
subject_folder(1:2)=[];
n_subj=length(subject_folder);
first_subj=1;

n_con=6;

stim_vec={'sync';'sync';'sync';'async';'async';'async'};
vis_vec={'high';'med';'low';'high';'med';'low'};
% labels={'subj' 'run' 'beta' 'stim' 'vis'};
labels={'subj' 'beta' 'stim' 'vis'};

roi_files=dir(fullfile(roi_folder, '*.mat'));
n_rois=length(roi_files);

h = waitbar(0,sprintf('running %d jobs',n_subj*n_rois)); % set up the 'waitbar'

for roi=1:n_rois
    
    Z=[];
    
    roi_file=[roi_folder filesep roi_files(roi).name(1:end-4) '.mat'];
    R  = maroi(roi_file); % Make marsbar ROI object
    
    for subj=first_subj:n_subj
        
        if strcmp(subject_folder(subj).name,'P02')
            n_runs=2;
        else
            n_runs=4;
        end
        
        betas_allruns=[];
        sum_betas=[];
        
        for run=1:n_runs
            
            spmFile=[data_folder filesep subject_folder(subj).name filesep 'Func\Func' num2str(run) filesep '1stLevel' filesep 'SPM.mat'];
            
            D  = mardo(spmFile); % Make marsbar design object
            Y  = get_marsy(R, D, 'mean'); % Fetch data into marsbar data object
            xCon = get_contrasts(D); % Get contrasts from original design
            E = estimate(D, Y); % Estimate design on ROI data
            E = set_contrasts(E, xCon); % Put contrasts from original design back into design object
            b = betas(E); % get design betas
            %         marsS = compute_contrasts(E, 1:n_con); % get stats and stuff for all contrasts into statistics structure
            
            betas_allruns(:,run)=b(1:6);
            
        end
        
        sum_betas=sum(betas_allruns,2);
        
        beta_vec=num2cell(sum_betas);
        subj_vec=num2cell(subj*ones(length(sum_betas),1));
        %             run_vec=num2cell(run*ones(length(beta_vec),1));
        
        %             X=[subj_vec run_vec beta_vec stim_vec vis_vec];
        X=[subj_vec beta_vec stim_vec vis_vec];
        
        Z=[Z;X];
        
        %             clear X beta_vec subj_vec run_vec
        clear X beta_vec subj_vec
        
        
        waitbar((subj+(roi-1)*n_subj)/(n_subj*n_rois),h)
    end
    
    Z=[labels;Z];
    
    xlswrite(['D:\VHI\SampleSizeDetermination\ROIs\Tables_A\table_' roi_files(roi).name(1:end-4) '.xlsx'],Z);
    
end
delete(h)