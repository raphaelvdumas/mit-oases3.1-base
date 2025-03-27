%[f_out,ts_out]=volume_scatter(f,bw,T,Zs,Zr,c,rho,rough,R,Z)
%
% function to compute volume scattering realization using spectral
% integral
%
% Kevin D. LePage
% SACLANTCEN
% 30/1/98
%
% INPUTS
%
% f	center frequency (Hz)
% bw	bandwidth (Hz)
% T	max time (sec)
% Zs	source height above interface (m)
% Zr	vector of receiver heights above interface (m)
% c	vector of sound speeds above and below interface (m/s) (2 x 1)
% rho	corresponding vector of densitties (gm/cm^3) (2 x 1)
% rough	lower halfspace volume sound speed perturbations (m/s)(ranges x depths)
% R 	length of range realization (m)
% Z	depth of range realization (m)
%
% OUTPUTS
%
% f_out	matrix of transfer functions to all receivers of scattered field
% ts_outcorrsponding matrix of time series to all receivers of scattered field

function[f_out,ts_out]=volume_scatter(f,bw,T,Zs,Zr,c,rho,rough,R,Z)

% number of range and depth bins

NR=size(rough,2);

NZ=size(rough,1);

% length of the patch defines dk

dk=2*pi/R;

% minimum phase speed included

c_min=min(c)*.8;

% length of time defines dw

dw=2*pi/T;

% number of cw calculations

NW=ceil(bw*2/dw);

% transform patch in range

rough_kx=(fftshift(fft2(rough))*/R/D;

for j=NW:-1:1

% frequency 

w=2*pi*(f-bw)+j*dw;

% number of wavenumbers

NK=floor((w/c_min)/dk);

% check to make sure there are enough wavenumber bins in rough_kx

if NK>size(rough,2)

fprintf('the required patch length is %d instead of %d\n',NK,size(rough,2))

end


