%function[ts_out]=trf_time_series_oases3d(out,z,range,f,fo,omegim,msuft,t,bw,azimuth)
%
% function to compute range-depth time series for comparison to NB approx
%
% Kevin D. LePage
% 19/04/00
% SACLANTCEN
%
% INPUTS
% 
% out		complex transfer function (length(z)xlength(r)xlength(f))
% z		receiver depths (m)
% range		receiver ranges (m)
% f		receiver frequencies (Hz)
% fo		center frequency (Hz)
% omegim        imaginary radian frequency
% msuft         number of Fourier harmonics
% t		vector of desired times (s)
% bw		bandwidth (Hz)
% azimuth       optional azimuthal angle for computational plane (deg)
%
% OUTPUTS
%
% ts_out	range depth time series for all desired times superimposed

function[ts_out]=trf_time_series_oases3d(out,z,range,f,fo,omegim,msuft,t,bw,azimuth)

if nargin<10

azimuth=0;

else

azimuth=azimuth*pi/180;

end

% make f a column vector

f=f(:);

% make t a row vector

t=t(:)';

%make time vector'

time=exp(i*(f*2*pi+i*omegim)*t).*(exp(-(f-fo).^2/bw^2/2)*ones(1,length(t)))*max(diff(f))*2*pi;

% take inner product of out with time, gives depth x range x time, 
% then sum across time

temp=zeros(size(out,2),size(out,4));

junk=zeros(length(t),size(out,2));

for jj=1:size(out,3)

for j=1:size(out,1)

temp(:,:)=out(j,:,jj,:);

if length(t)>1

%ts_out(j,:)=sum((temp*time).');

if jj==1

ts_out(j,:,:)=((temp*time).');

else


junk(:,:)=ts_out(j,:,:);

ts_out(j,:,:)=junk+((temp*time).')*((jj/2-floor(jj/2))*2*sin(azimuth*floor(jj/2))+((jj-1)/2-floor((jj-1)))*2*cos(azimuth*floor(jj/2)));

end

else

if jj==1

ts_out(j,:)=((temp*time).');

else

ts_out(j,:)=ts_out(j,:)+((temp*time).')*((jj/2-floor(jj/2))*2*sin(azimuth*floor(jj/2))+((jj-1)/2-floor((jj-1)))*2*cos(azimuth*floor(jj/2)));end

end

end

end



