%[ts_mod]=demod(ts,fs,fc)
%
% function to demodulate time series by carrier frequency
%
% Henrik Schmidt
% MIT
% 16/2/00
%
% INPUTS 
%
% ts		time series 
% fs		sample frequency (Hz)
% fc            carrier frequency (Hz)
% 
% OUTPUTS
%
% ts_demod	demodulated time series

function[ts_demod]=demod(ts,fs,fc)


nt=length(ts);
twin=nt/fs;
df=1/twin;
nf=nt/2

fsp=fft(ts);
shift=fc/df+1;
for i=1:nf-shift
 buf(i)=buf(shift+i-1);
end





