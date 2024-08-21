clear
% close all

n_subj=49;

for subj = 1:n_subj
    
    for run = 1:4
        
        subj
        run
        
        load('D:\VHI\Analysis\GSR\pspm_batch.mat')
        
        if nnz(num2str(subj)) == 1
            foldername = ['D:\VHI\Data\S0' num2str(subj) '\GSR'];
            filename = [foldername '\grs_subject0' num2str(subj) '_run' num2str(run) '.txt'];
            condFile = ['D:\VHI\Data\S0' num2str(subj) '\Psych\resultsTable_s0' num2str(subj) '_run' num2str(run) '.mat'];
        else
            foldername = ['D:\VHI\Data\S' num2str(subj) '\GSR'];
            filename = ['D:\VHI\Data\S' num2str(subj) '\GSR\grs_subject' num2str(subj) '_run' num2str(run) '.txt'];
            condFile = ['D:\VHI\Data\S' num2str(subj) '\Psych\resultsTable_s' num2str(subj) '_run' num2str(run) '.mat'];
        end
        
        if exist(filename, 'file') == 2
            
            load(condFile);
            
            dataArray = dlmread(filename,'',9);
            unfiltered_signal = dataArray(:,2);
            % unfiltered_signal = unfiltered_signal(1:700000);
            
            y=fft(unfiltered_signal); % power spectrum analysis
            
            signalToFilter = unfiltered_signal;
            
            fs = 1000;             % sampling rate
            res = 1/fs;             % resolution
            n = length(unfiltered_signal);          % number of samples
            f = (0:n-1)*(fs/n);     % frequency range
            power = abs(y).^2/n;    % power of the DFT
            total_time=720;
            % total_time=700;
            t=res:res:total_time;
            
            figure;
            
            %             hold on
            %             patch([69 80 80 69], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             patch([181 192 192 181], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             patch([293 304 304 293], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             patch([405 416 416 405], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             patch([517 528 528 517], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             patch([629 640 640 629], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            %             plot(t,unfiltered_signal);
            %             hold off
            
            upperLimitFreqPoint = 500;
            nSDs=10; %% ARBITRARY, BUT PROBABLY FINE
            
            lowerLimitFreq = .03; % ARBITRARY, BUT HARD TO FIND INFORMATION ABOUT IT
            lowerLimitFreqPoint = nnz(f<lowerLimitFreq); % checks for the point with frequency 0.05 Hz
            powerForFiltering = power(lowerLimitFreqPoint:upperLimitFreqPoint);
            
            meanSignal=mean(powerForFiltering);
            stdSignal=std(powerForFiltering);
            cutoff = meanSignal+nSDs*stdSignal;
            
            figure;plot(f(5:upperLimitFreqPoint),power(5:upperLimitFreqPoint))
            hold on
            plot(f(lowerLimitFreqPoint:upperLimitFreqPoint),ones(length(f(lowerLimitFreqPoint:upperLimitFreqPoint)))*cutoff,'--');
            xlabel('Frequency')
            ylabel('Power')
            hold off
            
            while 1
                if nnz(max(powerForFiltering) > cutoff) % checks whether there's a peak higher than the cut-off
                    
                    [~,peakInFilterInterval]=max(powerForFiltering); % finds the point in which the peak is located (in the interval of interest)
                    
                    f0 = f(peakInFilterInterval+lowerLimitFreqPoint-1); % notch frequency (in the power spectrum)
                    fn = fs/2;              % Nyquist frequency
                    freqRatio = f0/fn;      % ratio of notch freq. to Nyquist freq.
                    
                    notchWidth = 0.0001;       % width of the notch %% ARBITRARY, BUT PROBABLY OK
                    
                    % Compute zeros
                    notchZeros = [exp( sqrt(-1)*pi*freqRatio ), exp( -sqrt(-1)*pi*freqRatio )];
                    
                    % Compute poles
                    notchPoles = (1-notchWidth) * notchZeros;
                    
                    b = poly( notchZeros ); %  Get moving average filter coefficients
                    a = poly( notchPoles ); %  Get autoregressive filter coefficients
                    
                    % notch-filter signal
                    filtered_signal = filter(b,a,signalToFilter);
                    
                    y2=fft(filtered_signal); % power spectrum analysis
                    power2 = abs(y2).^2/n;    % power of the DFT
                    
                    powerForFiltering = power2(lowerLimitFreqPoint:upperLimitFreqPoint);
                    meanSignal=mean(powerForFiltering);
                    stdSignal=std(powerForFiltering);
                    cutoff = meanSignal+nSDs*stdSignal;
                    
                    pause(.3);
                    
                    plot(f(5:upperLimitFreqPoint),power2(5:upperLimitFreqPoint));
                    hold on
                    plot(f(lowerLimitFreqPoint:upperLimitFreqPoint),ones(length(f(lowerLimitFreqPoint:upperLimitFreqPoint)))*cutoff,'--');
                    xlabel('Frequency')
                    ylabel('Power')
                    hold off
                    
                    signalToFilter = filtered_signal; % iterative process in which we further filter the filtered signal
                    
                else
                    y2=signalToFilter;
                    break
                end
            end
            
            back2signal=ifft(y2); % power spectrum analysis
            
            figure;plot(1:n,zscore(unfiltered_signal),1:n,zscore(back2signal))
            
%             filtered_signal_bandpass = bpfilt(back2signal, .005, .6, fs); % our stimulus repeats in a frequency of ~0.0089 Hz %% ARBITRARY, MAYBE WE CAN FIND INFO
%             filtered_signal_bandpass = bpfilt(back2signal, .005, 2.5, fs); % our stimulus repeats in a frequency of ~0.0089 Hz %% ARBITRARY, MAYBE WE CAN FIND INFO
            filtered_signal_bandpass = bpfilt(back2signal, .005, 5, fs); % our stimulus repeats in a frequency of ~0.0089 Hz %% ARBITRARY, MAYBE WE CAN FIND INFO
            figure;plot(t,filtered_signal_bandpass);
            hold on
            patch([69 80 80 69], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            patch([181 192 192 181], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            patch([293 304 304 293], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            patch([405 416 416 405], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            patch([517 528 528 517], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            patch([629 640 640 629], [max(ylim) max(ylim) min(ylim) min(ylim)], 'r')
            plot(t,filtered_signal_bandpass, 'k', 'LineWidth',1)
            hold off
            
            filtered_signal_bandpass_trimmed=filtered_signal_bandpass(50000:650000);
            
            if nnz(num2str(subj)) == 1
                filteredFilename = [foldername '\grs_subject0' num2str(subj) '_run' num2str(run) '_filtered.txt'];
            else
                filteredFilename = [foldername '\grs_subject' num2str(subj) '_run' num2str(run) '_filtered.txt'];
            end
            dlmwrite(filteredFilename,filtered_signal_bandpass_trimmed);
            
            condCell = table2cell(T);
            
            index1 = find(strcmp(condCell(:,12),'sync') & strcmp(condCell(:,13),'high'));
            onsetCond(1) = cell2mat(condCell(index1,5))+cell2mat(condCell(index1,17))-50;
            index2 = find(strcmp(condCell(:,12),'sync') & strcmp(condCell(:,13),'mid'));
            onsetCond(2) = cell2mat(condCell(index2,5))+cell2mat(condCell(index2,17))-50;
            index3 = find(strcmp(condCell(:,12),'sync') & strcmp(condCell(:,13),'low'));
            onsetCond(3) = cell2mat(condCell(index3,5))+cell2mat(condCell(index3,17))-50;
            index4 = find(strcmp(condCell(:,12),'async') & strcmp(condCell(:,13),'high'));
            onsetCond(4) = cell2mat(condCell(index4,5))+cell2mat(condCell(index4,17))-50;
            index5 = find(strcmp(condCell(:,12),'async') & strcmp(condCell(:,13),'mid'));
            onsetCond(5) = cell2mat(condCell(index5,5))+cell2mat(condCell(index5,17))-50;
            index6 = find(strcmp(condCell(:,12),'async') & strcmp(condCell(:,13),'low'));
            onsetCond(6) = cell2mat(condCell(index6,5))+cell2mat(condCell(index6,17))-50;
            
            pause(1);
            
            matlabbatch{1, 1}.pspm{1, 1}.prep{1, 1}.import.datatype.txt.datafile = cellstr(filteredFilename);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.modelfile = ['model_subj' num2str(subj) '_run' num2str(run)];
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.outdir = cellstr(foldername);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(1).onsets
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(1).onsets = onsetCond(1);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(2).onsets = onsetCond(2);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(3).onsets = onsetCond(3);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(4).onsets = onsetCond(4);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(5).onsets = onsetCond(5);
            matlabbatch{1, 2}.pspm{1, 1}.first_level{1, 1}.scr{1, 1}.glm_scr.session.data_design.condition(6).onsets = onsetCond(6);
            
            pspm_jobman('run',matlabbatch);
            
            load([foldername '\model_subj' num2str(subj) '_run' num2str(run) '.mat'])
            
            figure;plot(glm.stats(1:2:12));
            
            pause(1);
            
            close all
            
        end
    end
end