%[data,times]=image_data_prepare(bbband,max_time)
% 
% function to prepare data for image_basin
%
% Kevin D. LePage
% 20/4/98
% SACLANTCEN
%
% INPUTS
%
% bbband		third octave frequency desired to build "data" out of
% max_time	maximum time (will truncate or pad with zeros)
%
% OUTPUTS
%
% data		snapshots x (times x angles)
% times		corresponding time vectors

function[data,times]=image_data_prepare(bbband,max_time)

% the function will load all the *_snr.mat files in the current directory

junk='ls *snr.mat'
eval(junk);
out=ans;
ind_end=find(out==46)-1;
ind_eol=find(out==10);
ind_eol=ind_eol(1:length(ind_eol)-1);
ind_beg=find(out==32);
last_beg=ind_beg(length(ind_beg));
ind_beg=[1 ind_beg(find(diff(ind_beg)~=1))+1 last_beg+1];
ind_beg=[ind_beg ind_eol+1];
ind_beg=sort(ind_beg);

% loop through file names, loading and rearranging results into "data"

for j=1:length(ind_beg)

fprintf('%s\n',['load ' out(ind_beg(j):ind_end(j)) ])

eval(['load ' out(ind_beg(j):ind_end(j)) ])

bband=find(bands==bbband);

little_data=(amb(:,bband)*ones(1,length(times))).*snr(:,length(times)*(bband-1)+1:length(times)*bband)/source(bband);

[val,min_time_ind]=max(little_data');

min_time_ind=min(min_time_ind);

if j==1

dt=min(diff(times));

max_time=ceil(max_time/dt);

end

little_data=little_data(:,min_time_ind:size(little_data,2));

little_data=[little_data amb(:,bband)*ones(1,max_time-size(little_data,2))/source(bband)];

data((j-1)*max_time*size(little_data,1)+1:j*max_time*size(little_data,1),1)=reshape(little_data',max_time*size(little_data,1),1);

end

times=[0:max_time-1]*dt;


