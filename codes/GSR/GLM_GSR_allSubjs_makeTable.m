clear

n_subj=49;
list=[];

stim_cell={'sync';'sync';'sync';'async';'async';'async'};
vis_cell={'high';'mid';'low';'high';'mid';'low'};

for subj = 1:n_subj
    
    for run = 1:4
        
        subj
        run
        
        load('D:\VHI\Analysis\GSR\pspm_batch.mat')
        
        if nnz(num2str(subj)) == 1
            foldername = ['D:\VHI\Data\S0' num2str(subj) '\GSR'];
        else
            foldername = ['D:\VHI\Data\S' num2str(subj) '\GSR'];
        end
        modelname = [foldername '\model_subj' num2str(subj) '_run' num2str(run) '.mat'];
        
        subj_val = subj*ones(6,1);
        run_val = run*ones(6,1);
        beta_val = nan*ones(6,1);
        
        if exist(modelname, 'file') == 2
            
            load([foldername '\model_subj' num2str(subj) '_run' num2str(run) '.mat'])
            
            beta_val = glm.stats(1:2:12);
                        
        end
        
        list=[list;num2cell(subj_val) num2cell(run_val) stim_cell vis_cell num2cell(beta_val)];
        
    end
end

labels={'subj' 'run' 'stim' 'vis' 'gsr_beta'};

list=[labels;list];

xlswrite('D:\VHI\Analysis\GSR\Tables\table_GSRbetas_VHI_filter_5e-3_5e0.xlsx',list);