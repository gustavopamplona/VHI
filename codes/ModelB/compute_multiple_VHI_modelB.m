function [names,onsets,durations]=compute_multiple_VHI_modelB(T)

dataSubj=table2cell(T);

names{1,1}='1stHalf-sync-high';
names{1,2}='1stHalf-sync-med';
names{1,3}='1stHalf-sync-low';
names{1,4}='1stHalf-async-high';
names{1,5}='1stHalf-async-med';
names{1,6}='1stHalf-async-low';
names{1,7}='2ndHalf-sync-high';
names{1,8}='2ndHalfsync-med';
names{1,9}='2ndHalfsync-low';
names{1,10}='2ndHalfasync-high';
names{1,11}='2ndHalfasync-med';
names{1,12}='2ndHalfasync-low';
names{1,13}='syringe';
names{1,14}='baseline';
names{1,15}='questionnaire';

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
    onsets{1,i}=cell2mat(dataSubj(find(index==i),2));
    durations{1,i}=(cell2mat(dataSubj(find(index==i),4))-cell2mat(dataSubj(find(index==i),2)))/2;
end
for i=1:6
    onsets{1,i+6}=cell2mat(dataSubj(find(index==i),2))+((cell2mat(dataSubj(find(index==i),4))-cell2mat(dataSubj(find(index==i),2)))/2);
    durations{1,i+6}=(cell2mat(dataSubj(find(index==i),4))-cell2mat(dataSubj(find(index==i),2)))/2;
end

onsets{1,13}=cell2mat(dataSubj(:,5))+cell2mat(dataSubj(:,17)); % syringe block
durations{1,13}=cell2mat(dataSubj(:,6))-cell2mat(dataSubj(:,5))-cell2mat(dataSubj(:,17));
onsets{1,14}=cell2mat(dataSubj(:,7)); % baseline block
durations{1,14}=cell2mat(dataSubj(:,8))-cell2mat(dataSubj(:,7));
onsets{1,15}=cell2mat(dataSubj(:,8)); % questionnaire block
durations{1,15}=18;

if nnz(~isnan(cell2mat(dataSubj(:,3)))) > 0
    names{1,16}='buttonPress';
    buttonPressOnsets=cell2mat(dataSubj(:,2))+cell2mat(dataSubj(:,3));
    onsets{1,16}=buttonPressOnsets(~isnan(buttonPressOnsets));
    durations{1,16}=0;
end
