%[ts]=ray_auv(D,zs,vec_patch,vec_receiver,co,wave,t)
%
% function to compute straight line travel times from topas to auv
% and bottom reflected travel times from patch
%
% Kevin D. LePage
% SACLANTCEN
% 24/5/98 
%
% INPUTS
%
% D		water depth (m)
% zs		source depth (m)
% vec_patch	[x y] vector of patch where beam was steered (m)
% vec_receiver  [X,Y,Z] vector of various receiver positions (m)
% co		optional sound speed (1500 m/s default)
% wave		waveform
% t		time axis of time series
%
% OUPUTS
%
% time		travel times for direct, surface, bottom, patch direct, 
%		              patch surface multiple (length(X) x 5)
% ts		synthetic time series

function[ts]=ray_auv(D,zs,vec_patch,vec_receiver,co,wave,t)

if nargin<5

co=1500;

end

if nargin<7

fo=8000;

fs=50000;

t=[0:4999]/fs;

band=[2000 15000];

t=t(:);

% Ricker replica

sigma_sq=(1/(sqrt(2)*pi*fo))^2;

wave=(t.^2/sigma_sq-1).*exp(-t.^2/2/sigma_sq);

% truncate if necessary

wave=wave(1:length(freq));

mask_ind_low=find(abs(freq)<band(1));

mask_ind_middle_low=find((freq>=band(1))&(freq<=band(2)));

mask_ind_middle_high=find((freq<=-band(1))&(freq>=-band(2)));

mask_ind_high=find(abs(freq)>band(2));

mask=ones(length(wave),1);

mask(mask_ind_low)=zeros(length(mask_ind_low),1);

mask(mask_ind_high)=zeros(length(mask_ind_high),1);

mask(mask_ind_middle_low)=hanning(length(mask_ind_middle_low));

mask(mask_ind_middle_high)=hanning(length(mask_ind_middle_high));

end

fs=1/(t(2)-t(1));

% frequencies

freq=[[0:floor(length(t)/2)] [-(floor(length(t)/2)-1):-1]]'*fs/length(t);

x=vec_patch(1);

y=vec_patch(2);

X=vec_receiver(:,1);

Y=vec_receiver(:,2);

Z=vec_receiver(:,3);

wave=fft(wave);

% first compute all the distances

X=X(:)';

Y=Y(:)';

Z=Z(:)';

direct=((zs-Z).^2+X.^2+Y.^2).^(.5);

%surface=((zs+Z).^2+X.^2+Y.^2).^(.5);

%bottom=(((D-zs)+(D-Z)).^2+X.^2+Y.^2).^(.5);

patch=sqrt((D-zs)^2+(x).^2+(y)^2)+((D-Z).^2+(x-X).^2+(y-Y).^2).^(.5);

p_s_m=sqrt((D-zs)^2+(x).^2+(y)^2)+((D+Z).^2+(x-X).^2+(y-Y).^2).^(.5);

%for j=1:length(X)

%ts(:,j)=real(ifft(wave.*(exp(-i*2*pi*freq*(direct(j)-direct(j))/co)-exp(-i*2*pi*freq*(surface(j)-direct(j))/co)-exp(-i*2*pi*freq*(bottom(j)-direct(j))/co)+exp(-i*2*pi*freq*(patch(j)-direct(j))/co)-exp(-i*2*pi*freq*(p_s_m(j)-direct(j))/co))));

%ts(:,j)=real(ifft(wave.*(exp(-i*2*pi*freq*(patch(j)-direct(j))/co)-exp(-i*2*pi*freq*(p_s_m(j)-direct(j))/co))));

%time(:,j)=[direct(j)/co surface(j)/co bottom(j)/co patch(j)/co p_s_m(j)/co]';

%end

ts=real(ifft(wave*ones(1,length(X)).*(exp(-i*2*pi*freq*(patch-direct)/co)-exp(-i*2*pi*freq*(p_s_m-direct)/co))));

