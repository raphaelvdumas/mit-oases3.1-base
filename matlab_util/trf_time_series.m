%function[ts_out]=trf_time_series(out,z,range,f,fo,omegim,t,bw)
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
% t		vector of desired times (s)
% bw		bandwidth (Hz)
%
% OUTPUTS
%
% ts_out	range depth time series for all desired times superimposed

function[ts_out]=trf_time_series(out,z,range,f,fo,omegim,t,bw)

% make f a column vector

f=f(:);

% make t a row vector

t=t(:)';

%make time vector

time=exp(i*(f*2*pi+i*omegim)*t).*(exp(-(f-fo).^2/bw^2/2)*ones(1,length(t)))*max(diff(f))*2*pi;

% take inner product of out with time, gives depth x range x time, 
% then sum across time

temp=zeros(size(out,2),size(out,3));

for j=1:size(out,1)

temp(:,:)=out(j,:,:);

if length(t)>1

%ts_out(j,:)=sum((temp*time).');

ts_out(j,:,:)=((temp*time).');

else

ts_out(j,:)=(temp*time).';

end

end




