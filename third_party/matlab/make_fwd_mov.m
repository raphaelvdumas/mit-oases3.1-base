%function[mov]=make_fwd_mov(f,d_f,phi,Z,Zs,Zr,Cp,Cg,D,T,R)
%
% function to make forward propagation movie using 
% narrowband approximation
%
% Kevin D. LePage
% SACLANTCEN
% 10/2/97
%
% Inputs
%
%	f	center frequency (Hz)
%	d_f	bandwidth (Hz)
% 	phi	mode shape functions (length(Z)xlength(Cp))
%	Z	mode shape function depths (m)
%	Zs	source depth (m)	
%	Zr	desired synthesis depths
% 	Cp	modal phase speeds (m/s)
%	Cg	modal group speeds (m/s)
%	D	modal dispersion (d^2k/dw^2)
%	T	desired time steps
%	R	desired range steps (km)

function[mov]=make_fwd_mov(f,d_f,phi,Z,Zs,Zr,Cp,Cg,D,T,R)

mov=moviein(length(T));

R=R*1000;

for j=1:length(Cp)
phi_s(:,j)=interp1(Z,phi(:,j),Zs)*ones(length(Zr),1);
if isempty(find(Zr(:)-Z(1:length(Zr))>1e-3))==0
phi_r(:,j)=interp1(Z,phi(:,j),Zr);
else
phi_r(:,j)=phi(1:length(Zr),j);
end
end

d_w=d_f*2*pi;

R=R(:)';

k=2*pi*f*(real(Cp)-i*abs(imag(Cp))).^(-1);

k=k(:);

S=(real(Cg)+i*abs(imag(Cg))).^(-1);

S=S(:);

D=real(D(:));

for j=1:length(T)
fprintf('\rj=%d out of %d',j,length(T))
t=T(j);

% get envelope in range for each mode at time t

env=exp(-(S*R-t).^2.*(4/d_w/d_w-i*D*R/2).^(-1)).*(pi*(1/d_w/d_w-i*D*R/2).^(-1)).^(.5);

% multiply the envelopes by the propagators

env=env.*exp(-i*(2*pi*f*t-k*R));

% divide by the sqrt of the modal eigenvalues and the square root
% of the range

env=env.*(k*R).^(-.5)*sqrt(2*pi);

%make the propagated field

field=(phi_s.*phi_r)*env;

% take real part

%field=real(field)

%imagesc(R/1000,Zr,real(field));
imagesc(R/1000,Zr,20*log10(abs(field)+1e-3))
colormap('jet');
colorbar
drawnow

mov(:,j)=getframe;

end



