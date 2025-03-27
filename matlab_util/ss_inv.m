%function[ss,ang,patch]=ss_inv(t_beam,t,beam,wo,bw,Zs,Zr,D,L,sigma,c)
%
% Kevin D. LePage
% 27/2/98
% SACLANTCEN
%
% Function to invert scattering strength from time angle amplitude
% measurements
%
% INPUTS:
%
% t_beam	beamformed time series (amplitude) (time x beam)
% t		corresponding time axis
% beam		beam corresponding beams (degrees)
% wo		center frequency (Hz)
% bw		bandwidth of time series (Hz)
% Zs		source depth (m)
% Zr		receiver (array) depth (m)
% D		bottom depth
% L		source aperture (m)
% sigma		standard deviation of angle smoother on data (degrees)
% c		sound speed (default 1500 m/s)
%
% OUTPUTS:
%
% ss		scattering strength (power)
% ang		corrsponding grazing anles (deg)

function[ss,ang,patch_bw,patch_beam]=ss_inv(ts_beam,t,beam,wo,bw,Zs,Zr,D,L,sigma,c)

if nargin<11

c=1500;

end

% determine approximate bclottom ranges correponding to data times

r=(c^2*t.^2-4*(D-Zr).^2).^(.5)/2;

% call the ray_trace

[tt,ang]=overlay(D,Zs,Zr,0,r,c,1);

tt=[0;tt(:,1);tt(size(tt,1),1)+1];

ang=[90;ang(:,1);ang(size(ang,1),1)+1];

% interpolate

ang=interp1(real(tt),real(ang),t);

% compute patch size for each range

% first, patch size due to bandwidth (assumes exp(-(t-r/c)^2bw^2/2))

bw=bw*2*pi;

patch_bw=2*c/bw*cos(ang*pi/180).^(-1);

% second, patch size due to array aperture (assumes exp(-x^2/2/L^2)
% shading)

wo=2*pi*wo;

patch_beam=2*c/wo/L*(D-Zr)*(cos(ang*pi/180).*sin(ang*pi/180)).^(-1);

% replace undefined values with infinity

patch_beam(find(isnan(patch_beam)))=Inf*ones(size(find(isnan(patch_beam))));

%  select smaller patch size (meters)

patch=min(patch_bw,patch_beam);

% now we have to get an average ss value for each time step by
% integrating across the beams

% loop through times, integrating scattering across beams

for j=1:length(t)

win=exp(-(beam-ang(j)).^2/2/sigma/sigma);

% unity gain

win=win/sum(win);

% make win a row vector

win=win(:)';

ss(j)=win*abs(flipud(ts_beam(j,:)')).^2/patch(j)*(t(j)*c/2)^3;

%ss(j)=win*abs(flipud(ts_beam(j,:)')).^2/patch(j);

end


