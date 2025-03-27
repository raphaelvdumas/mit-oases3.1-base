%[out]=nearfield(ts,x,depth,z_b,fs,xx,yy,times)
%
% function to compute scattering from the near regions of the seafloor
%
% Kevin D. LePage
% 23/4/98
% SACLANTCEN
%
% INPUTS
%
% ts	time series on hla (num_samp x num_chan)
% x	axial position along array (num_chan) (m)
% depth	depth of array
% z_b	bottom depth
% fs	sample frequency
% xx	desired across track locations (m)
% yy	desired along track positions (m)
% times	desired times for the image
%
% OUTPUTS
%
% out	time series along receivers in y-z plane 

function[out]=nearfield(ts,x,depth,z_b,fs,xx,yy,times)

% determine the indicies for the times

time_in=[0:size(ts,1)-1]/fs;

for j=1:length(times)

[val,ind(j)]=min(abs(times(j)-time_in));

end

% zero pad the time series

ts=[ts;zeros(2^nextpow2(size(ts,1))-size(ts,1),size(ts,2))];

nfft=size(ts,1);

% fourier transform the time series

for j=1:size(ts,2)

fprintf('\rFFT j=%d out of %d',j,size(ts,2))

ts(:,j)=fft(ts(:,j));

end

% determine the vector of wavenumbers

ko=2*pi*([[0:nfft/2]'*fs/nfft;[-nfft/2+1:-1]'/nfft*fs])/1500;

%loop over all the receiver locations

for j=1:length(xx)

for k=1:length(yy)

% determine the ranges to all the potential scatterer locations

%range=((xx*ones(size(x))-ones(size(xx))*x).^2+(z_b-depth).^2+yy.^2).^(.5);

range=((xx(j)-x(:)').^2+(z_b-depth).^2+yy(k).^2).^(.5);

% make the propagator and multiply it by the FT

temp=zeros(nfft,1);

for l=1:length(x)

temp=temp+ts(:,l).*exp(-i*ko*range(l));

fprintf('\rj=%d out of %d, k=%d out of %d, l=%d out of %d',j,length(xx),k,length(yy),l,length(x))

end

temp=ifft(temp);

out(:,(j-1)*length(yy)+k)=temp(ind);

end

save out out

end




