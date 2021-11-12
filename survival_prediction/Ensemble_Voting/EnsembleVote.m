clc;
clear;

files = dir('Data\\');
files(1:2) = [];
for i=1 : size(files,1)
    filename = files(i).name;
    f = ['Data\' filename];
    data = readtable(f,'ReadVariableNames',true );
    ensemble(:,i) = table2array(data(:,end));
end

% 
% windowSize = 5;
% b = (1/windowSize)*ones(1,windowSize);
% ensemble = filter(b,1,ensemble);

[m ,n] = size(ensemble);
for j = 1 : m
    en_max(j,1) = max(ensemble(j,:));
    en_min(j,1) = min(ensemble(j,:));
    
    en_mean(j,1) = mean(ensemble(j,:));
    en_median(j,1) = median(ensemble(j,:));
end


% csvwrite('en_max_filter.txt',en_max)
% csvwrite('en_min_filter.txt',en_min)
% csvwrite('en_mean_filter.txt',en_mean)
% csvwrite('en_median_filter.txt',en_median)


csvwrite('max.txt',en_max)
csvwrite('min.txt',en_min)
csvwrite('mean.txt',en_mean)
csvwrite('median.txt',en_median)


    windowSize = 5;
    b = (1/windowSize)*ones(1,windowSize);
    y = filter(b,1,en_mean);
    csvwrite('filter_en_mean.txt',y)
    y = filter(b,1,en_median);
    csvwrite('filter_en_median.txt',y)
    y = filter(b,1,en_min);
    csvwrite('filter_en_min.txt',y)
    y = filter(b,1,en_max);
    csvwrite('filter_en_max.txt',y)
%     Q = 1;
%     R = 1;
%     N = 0;
%     [kalmf,L,P] = kalman(en_mean,Q,R,N);


