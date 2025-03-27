%function[S,D]=make_group_disp(freq,c,rho,z,kr,mode,Z)
% 
% Function to estimate group speed and dispersion given mode shapes
% eigenvalues and ssp and density profile
%
% Kevin D. LePage
% 1/8/98
% SACLANTCEN
%
% INPUTS
%
% freq	frequency (Hz)
% c	ssp (m/s)
% rho	density profile (correct units, usually gm/cm^3
% z	corresponding depths
% kr	modal eigenvalues (rad/m)
% mode	mode shape functions (length Z x length kr)
% Z	corresponding depths
%
% OUTPUTS
%
% S	slowness (s/m)
% D	dispersion (s^2/rad/m)

function[S,D]=make_group_disp(freq,cc,rhoo,z,kr,mode,Z)

% interpolate c and rho onto mode shape function grid

c=interp1(z,cc,Z);

rho=interp1(z,rhoo,Z);

% omega

w=2*pi*freq;

% extend mode shape functions into the bottom

% bottom depths

lambda=2*pi/max(real(kr));

z_b=[abs(max(diff(Z))):abs(max(diff(Z))):3*lambda]';

% first vertical wavenumber

k_b=((w/cc(length(cc)))^2-kr.^2).^(.5);

k_b=(real(k_b(:))-i*abs(imag(k_b(:)))).';

% get mode shape functions in bottom

%mode_b=(ones(length(z_b),1)*mode(length(Z),:)).*(exp(-z_b*k_b)*rhoo(length(rhoo)-1)/rhoo(length(rhoo)));

mode_b=(ones(length(z_b),1)*mode(length(Z),:)).*exp(-z_b*k_b);

% append mode shape function

mode=[mode;mode_b];

Z=[Z(:);z_b(:)+Z(length(Z))];

% append density and sound speed

c=[c(:);cc(length(cc))*ones(length(z_b),1);];

rho=[rho(:);rhoo(length(rhoo))*ones(length(z_b),1);];

% slowness

for j=1:length(kr)

fprintf('\rj=%d out of %d',j,length(kr))

S(j)=w/kr(j)*trapz(Z(:),mode(:,j).^2.*(c.^(-2)./rho));

end

% dispersion

D=S/w-S.^2./kr;
