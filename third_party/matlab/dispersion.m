%function[D,S]=dispersion(f,kr,phi,z,C,Z);
%
% function to compute dispersion from information at center frequency
%
% Kevin D. LePage
% SACLANTCEN
% 11/27/97
%
% INPUTS
%
% f	center frequency (Hz)
% kr	wavenumbers (rad/m)
% phi	mode shape functions (length(z) x length(kr))
% z	corresponding depths (m)
% C	sound speed profile (m/s)
% Z	corresponding depths (m)

function[D,S]=dispersion(f,kr,phi,z,C,Z);

w=2*pi*f;

% interpolate ssp

c=interp1(Z,C,z);

c(find(isnan(c)))=zeros(size(find(isnan(c))));

% dk/dw

S=w*kr.^(-1).*trapz(z(:),(c(:).^(-2)*ones(1,length(kr))).*phi.^(2))./trapz(z(:),phi.^2);

% d^2k/dw^2

D=-S.^2./kr+kr.^(-1).*trapz(z(:),(phi.^2).*(c(:).^(-2)*ones(1,length(kr))));
