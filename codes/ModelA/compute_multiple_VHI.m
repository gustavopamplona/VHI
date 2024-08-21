function [names,onsets,durations]=compute_multiple_VHI(T)

dataSubj=table2cell(T);

names{1,1}='sync-high';
names{1,2}='sync-med';
names{1,3}='sync-low';
names{1,4}='async-high';
names{1,5}='async-med';
names{1,6}='async-low';
names{1,7}='stimulation';
names{1,8}='baseline';
names{1,9}='questionnaire';

index=zeros(1,6);
for i=1:6
    if strcmp(dataSubj(i,12),'sync')
        if strcmp(dataSubj(i,13),'high')
            index(i)=1;
        elseif strcmp(dataSubj(i,13),'mid')
            index(i)=2;
        elseif strcmp(dataSubj(i,13),'low')
            index(i)=3;
        end
    elseif strcmp(dataSubj(i,12),'async')
        if strcmp(dataSubj(i,13),'high')
            index(i)=4;
        elseif strcmp(dataSubj(i,13),'mid')
            index(i)=5;
        elseif strcmp(dataSubj(i,13),'low')
            index(i)=6;
        end
    end
end

for i=1:6
    onsets{1,i}=cell2mat(dataSubj(find(index==i),5))+cell2mat(dataSubj(find(index==i),17));
%     durations{1,i}=cell2mat(dataSubj(find(index==i),6))-cell2mat(dataSubj(find(index==i),5))-cell2mat(dataSubj(find(index==i),17));
    durations{1,i}=5; % update GSPP: fixed duration of 5 s. That's enough for the needle "penetrate" completely and much shorter than the ~10 s defined before. This long time would prevent the HRF to delay its peak
end

onsets{1,7}=cell2mat(dataSubj(:,2)); % stimulation block
durations{1,7}=cell2mat(dataSubj(:,4))-cell2mat(dataSubj(:,2));
onsets{1,8}=cell2mat(dataSubj(:,7)); % baseline block
durations{1,8}=cell2mat(dataSubj(:,8))-cell2mat(dataSubj(:,7));
onsets{1,9}=cell2mat(dataSubj(:,8)); % questionnaire block
durations{1,9}=18;