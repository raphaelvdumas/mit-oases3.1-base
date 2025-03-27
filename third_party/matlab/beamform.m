%[t_beam]=beamform(ts,z,fs,beam,co)
%
% function to bartlett beamform time series received on a vertical
% line array
%
% Kevin D. LePage
% SACLANTCEN
% 14/1/98
%
% INPUTS 
%
% ts		time series (time x depth)
% z		corresponding depths
% fs		sample frequency (Hz)
% beam		desired look angles (deg)
% co		local sound speed used for generating replica vectors
% 
% OUTPUTS
%
% t_beam	beamformed time series (time x beam)

function[t_beam]=beamform(ts,z,fs,beam,co)

if nargin<5

co=1500;

end

z=z-mean(z);

% get good zero padding length

N=2^nextpow2(size(ts,1));

% fft data

ts_f=fft(ts,N);

% get correponding frequencies

f=[[0:fs/N:fs/2] [(-N/2+1)/N*fs:fs/N:-fs/N]]';

% get ko

ko=2*pi*f/co;

t_beam=zeros(N,length(beam));

% shading vector

%win=hanning(length(z));

win=sin((z-min(z))/(max(z)-min(z))*pi).^2;

win=win(:)';

% shade the data

% ts_f=ts_f.*(ones(length(ko),1)*win);

% normailizer

den=sum(win);

% build steering vectors

for j=1:length(beam)

fprintf('\rbeam %d out of %d',j,length(beam))

steer=exp(i*ko*sin(beam(j)*pi/180)*z(:)');

% shade the steering vector

steer=steer.*(ones(size(ko,1),1)*win);

% beamform

ts_f_b=real(sum((ifft(steer.*ts_f).')).')/den;

%t_beam(:,j)=real(ifft(ts_f_b));

t_beam(:,j)=ts_f_b;

end


