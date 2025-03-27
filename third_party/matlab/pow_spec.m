%[pow,freq,n_ave]=pow_spec(ts,fs,win)
%
% Kevin D. LePage
% SACLANTCEN
% 17/4/98
%
% function to compute power spectral estimate of ts
%
% INPUTS:
%
% ts	time series (length time x num_chan)
% fs	sample frequency (Hz)
% win	averaging window
%
% OUPUTS
%
% pow	power spectral estimate (ts units/root(Hz)) (length win/2+1 x num_chan)
% freq	corresponding frequencies
% n_ave	number of averages

function[pow,freq,n_ave]=pow_spec(ts,fs,win)

% determine the length of the window

l_win=length(win);

% determine the number of averages

n_ave=floor(size(ts,1)/l_win);

% predefine spectral estimate

pow=zeros(l_win,size(ts,2));

% build up spectral estimate

for j=1:n_ave

pow=pow+abs(ifft(ts((j-1)*l_win+1:j*l_win,:))).^2/n_ave;

end

% normalize by the frequency bin width and eliminate 
% the negative frequencies 

delta_f=fs/l_win;

pow=pow(1:l_win/2+1,:)/delta_f*2;

freq=[0:l_win/2]*delta_f;
