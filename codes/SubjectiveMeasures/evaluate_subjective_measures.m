%%% Read and plot subjective measures of RHI - VHI experiment

% Gustavo Pamplona, 11.01.23

clear
close all

data_folder='D:\VHI\Data';

% subject_folder=dir(data_folder);
% subject_folder(1:4)=[];
% n_subj=length(subject_folder);
n_subj=49;
first_subj=1;

n_runs = 4;

pilot=0;

stim_cell={'sync';'sync';'sync';'async';'async';'async'};
vis_cell={'high';'mid';'low';'high';'mid';'low'};
list=[];
Z=[];

min_onset_time = 4;

for subj=1:n_subj
    for run = 1:n_runs
        
        
        
        if pilot == 0
            if nnz(num2str(subj))==1
                folder_name = ['D:\VHI\Data\S0' num2str(subj) '\Psych'];
                file_name = [folder_name filesep 'resultsTable_S0' num2str(subj) '_run' num2str(run) '.mat'];
            else
                folder_name = ['D:\VHI\Data\S' num2str(subj) '\Psych'];
                file_name = [folder_name filesep 'resultsTable_S' num2str(subj) '_run' num2str(run) '.mat'];
            end
        else
            folder_name = ['D:\VHI\Data\Pilot\P0' num2str(subj) '\Psych'];
            file_name = [folder_name filesep 'resultsTable_P0' num2str(subj) '_run' num2str(run) '.mat'];
        end
        
        if exist(file_name)>0
            
            load(file_name);
            
            data_mat=table2cell(T);
            
            for cond = 1:6
                if cond < 4
                    traj_str='sync';
                else
                    traj_str='async';
                end
                if cond == 1 || cond == 4
                    vis_str='high';
                elseif cond == 2 || cond == 5
                    vis_str='mid';
                elseif cond == 3 || cond == 6
                    vis_str='low';
                end
                
                idx=find(strcmp(data_mat(:,12),traj_str) & strcmp(data_mat(:,13),vis_str)==1); % looks for the trial with a specific condition
                q1(run,cond)=cell2mat(data_mat(idx,9));
                q2(run,cond)=cell2mat(data_mat(idx,10));
                q3(run,cond)=cell2mat(data_mat(idx,11));
                illusion_onset(run,cond)=+~isnan(cell2mat(data_mat(idx,3))); % checks if there was an illusion indication
                illusion_onset_time(run,cond)=cell2mat(data_mat(idx,3)); % how long it took for the subject to perceive the illusion
                
            end
        else
            q1(run,:)=nan(1,6);
            q2(run,:)=nan(1,6);
            q3(run,:)=nan(1,6);
            illusion_onset(run,:)=nan(1,6);
            illusion_onset_time(run,:)=nan(1,6);
        end
        
%         Data exclusion
        if subj == 1 && run == 1 % subject didn't react fast enough
            q1(1,6)=nan;
        elseif subj == 3 % subject didn't understand the illusion onset indication % MAYBE STILL CONSIDER THE SUBJECT
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 5 && run == 1 % subject didn't understand the indication of illusion onset in the first run
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 8 && run == 1 % problems with acquisition of the indication of illusion onset in the first run
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 15 && run == 4 % robot was not working in the fourth run
            q1(run,:)=nan;
            q2(run,:)=nan;
            q3(run,:)=nan;
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 21 && run == 1 % subject forgot to press the button for indication of illusion onset; Subject didn't answer right the first 2 questions in the first 2 trials
            q1(1,3)=nan;
            q2(1,3)=nan;
            q1(1,6)=nan;
            q2(1,6)=nan;
        elseif subj == 24 && run == 1 % Inverted subjective measures in the first run. Invert it back in the analysis
            q1=-(q1-50)+50;
            q2=-(q2-50)+50;
            q3=-(q3-50)+50;
        elseif subj == 25 && (run == 1 || run == 2) % Understood the illusion indication wrong in the first two runs. Disconsider them
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 31
            if run == 1 % Subject forgot to indicate the illusion in the first run
                illusion_onset(run,:)=nan;
                illusion_onset_time(run,:)=nan;
            elseif run == 3 % Run3: subject reported that the brush was not touching his finger
                q1(run,:)=nan;
                q2(run,:)=nan;
                q3(run,:)=nan;
                illusion_onset(run,:)=nan;
                illusion_onset_time(run,:)=nan;
            end
        elseif subj == 39 && run == 1 % Run1: she only provided either full left or full right questions. Discard subjective measures for this run.
            q1(run,:)=nan;
            q2(run,:)=nan;
            q3(run,:)=nan;
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        elseif subj == 42 && (run == 1 || run == 2) % Run 1 & 2: don't use answers / illusion indications; participant didn't understand the questions
            q1(run,:)=nan;
            q2(run,:)=nan;
            q3(run,:)=nan;
            illusion_onset(run,:)=nan;
            illusion_onset_time(run,:)=nan;
        end
        
%         if subj == 8 && run == 4
%             1
%         end
        subj_val = subj*ones(6,1);
        run_val = run*ones(6,1);
        q1_val = q1(run,:)';
        q2_val = q2(run,:)';
        q3_val = q3(run,:)';
        illusion_onset_val = illusion_onset(run,:)';
        illusion_onset_time_val = illusion_onset_time(run,:)';
        
        % combined scores
        q_combined_val = q1_val+q2_val-2*q3_val;
        
        illusion_onset_time_val(illusion_onset_time_val < min_onset_time) = NaN; % illusion onsets lower than 10 seconds will not be considered
        illusion_onset_val(illusion_onset_time_val < min_onset_time) = 0; % illusion onsets lower than 10 seconds will not be considered
        
        list=[list;num2cell(subj_val) num2cell(run_val) stim_cell vis_cell num2cell(q1_val) num2cell(q2_val) num2cell(q3_val) num2cell(q_combined_val) num2cell(illusion_onset_val) num2cell(illusion_onset_time_val)];
        
        clear q1_val q2_val q3_val q_combined_val illusion_onset_val illusion_onset_time_val 
        
    end
    
    if exist(folder_name)
        mean_q1_subj(subj,:)=mean(q1,1);std_q1_subj(subj,:)=std(q1,1);
        mean_q2_subj(subj,:)=mean(q2,1);std_q2_subj(subj,:)=std(q2,1);
        mean_q3_subj(subj,:)=mean(q3,1);std_q3_subj(subj,:)=std(q3,1);
        mean_illusionOnset_subj(subj,:)=mean(illusion_onset,1);std_illusionOnset_subj(subj,:)=std(illusion_onset,1);
    end
    
%     list=[list;num2cell(subj_val) stim_cell vis_cell num2cell(mean_q1_subj') num2cell(mean_q2_subj') num2cell(mean_q3_subj') num2cell(mean_illusionOnset_subj')];
    
    clear q1 q2 q3 illusion_onset illusion_onset_time
    
end

labels={'subj' 'run' 'stim' 'vis' 'q1' 'q2' 'q3' 'q_combined' 'illusion_perceived','illusion_onset_time'};
% labels={'subj' 'stim' 'vis' 'q1' 'q2' 'q3' 'illusion_onset'};

list=[labels;list];

xlswrite(['D:\VHI\Analysis\SubjectiveMeasures\Results\table_subjectiveMeasures_VHI.xlsx'],list);
% xlswrite(['D:\VHI\Analysis\SubjetiveMeasures\Results\table_subjectiveMeasures_VHI_average.xlsx'],list);

l = cell(1,6);
l{1}='sync & high'; l{2}='sync & med'; l{3}='sync & low'; l{4}='async & high'; l{5}='async & med'; l{6}='async & low';

figure;h=bar(mean_q1_subj);title('question 1 - touch illusion');xlabel('Subjects');ylabel('Score');legend(h,l);
figure;h=bar(mean_q2_subj);title('question 2 - vision illusion');xlabel('Subjects');ylabel('Score');legend(h,l);
figure;h=bar(mean_q3_subj);title('question 3 - control');xlabel('Subjects');ylabel('Score');legend(h,l);
figure;h=bar(mean_illusionOnset_subj);title('Indicated illusion? (proportion)');xlabel('Subjects');ylabel('Score');legend(h,l);

