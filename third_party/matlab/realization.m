%[reverb]=realization(wo,d_w,phi,Z,Zs,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,frac,pow,N)
%
% function to determine realizations of reverberation
%
% Kevin D. LePage
% SACLANTCEN
% 10/27/97
%
% INPUTS:
%
%	wo	center frequency (Hz)
%	d_w	effective width of Gaussian pulse (Hz)
%	phi	mode functions (num depths x M)
%	Z	depths of mode functions (num depths x 1) 
%	Zs	source depth (m, scalar)
%	Zr	receiver depths (m, num rec depths x 1)
%	phi_ib	mode function incident on bottom (1 x M)
%	phi_rb	mode function reflected from bottom (1xM)
%	Cp	modal phase speeds (m/s x M)
%	Cg	group speed vector (units m/s x M)
%	D	dispersion term vector (units s^2/m x M)
%	T	desired vector of reverb times
%	ell	correlation length scale of scattering (m)
%       frac    fractal dimension (2 < frac < 3, -1 for Gaussian)
%	pow	power of scatterer process
%	N	desired number of realizations
%
% OUTPUTS
%
%	reverb	sta of reverb

function[reverb]=realization(wo,d_w,phi,Z,Zs,Zr,phi_ib,phi_rb,Cp,Cg,D,T,corr,frac,pow,N)

% number of modes

M=length(Cp);

% convert various frequencies to radians

d_w=d_w*2*pi;

wo=wo*2*pi;

% wavenumber vector

K=wo*Cp(:).^(-1);

% e-iwt time dependence

K=real(K)+i*abs(imag(K));

% slowness vector

S=real(Cg(:).^(-1));

% dispersion vector

D=real(D(:));

% get ranges out to twice the required range

R=real(max(T)*max(Cg));

% get the "required" range discretization

dr=real(min(pi/2/real(max(K)),corr/5))

% get the range vector

r=[dr:dr:R];

% get the spatial filter function to convolve with the scatterer 
% distribution

if (frac>0)

model=['goff' int2str((frac-2)*2+2)]

else

model='gauss'

end

% we want closed form product, first define some constants

Snm=S*ones(1,M)+ones(M,1)*S.';

Snm=Snm(:);

Knm=K*ones(1,M)+ones(M,1)*K.';

Knm=Knm(:);

Dnm=D*ones(1,M)+ones(M,1)*D.';

Dnm=Dnm(:);

% determine mode shape functions at source depth

Phi_s=interp1(Z,phi,Zs);

% determine mode shape functions at receiver depths

Phi_r=interp1(Z,phi,Zr);

Phi_out=Phi_s(:).*phi_ib(:);

Phi_back=(ones(length(Zr),1)*phi_rb(:).').*Phi_r;

Mode=zeros(length(Zr),M^2);

for j=1:length(Zr)

Mode(j,:)=reshape(Phi_out(:)*Phi_back(j,:),1,M^2);

end

% predefine the reverb matrix

reverb=zeros(length(Zr)*N,length(T));

% get the field at the desired times

for kk=1:N

l=(R-dr)*2^nextpow2(length(r))/length(r)

eta=make_rough(2^nextpow2(length(r)),(R-dr)*2^nextpow2(length(r))/length(r),corr*sqrt(2),model,1)'.*([0:dr:(2^nextpow2(length(r))-1)*dr]'*corr^2*4*pi).^(-.5)*sqrt(2)*pi;

%load -ascii eta.out

for j=1:length(T)

t=T(j);

for k=1:M^2

A=1e-2;

a=Snm(k)^2;

b=-log(A)*i*2*Dnm(k)-2*Snm(k)*t;

c=t^2+log(A)*(4/d_w/d_w);

rhigh=max(real((-b+(b^2-4*a*c).^(.5))/2/a),real((-b-(b^2-4*a*c).^(.5))/2/a));

rlow=min(real((-b+(b^2-4*a*c).^(.5))/2/a),real((-b-(b^2-4*a*c).^(.5))/2/a));

on=[max(1,real(rlow/dr)):max(1,min(length(r),real(rhigh/dr)))];

win=exp(-(t-Snm(k)*r(on)).^2.*(4/d_w^2-i*2*Dnm(k)*r(on)).^(-1));

win=win.*(pi*(1/d_w^2-i*Dnm(k)*r(on)/2).^(-1)).^(.5);

reverb(length(Zr)*(kk-1)+1:length(Zr)*kk,j)=reverb(length(Zr)*(kk-1)+1:length(Zr)*kk,j)+Mode(:,k)*((win.*exp(-i*(wo*t-Knm(k)*r(on)))/sqrt(K(floor((k-1)/M)+1)*K(k-M*floor((k-1)/M))))*eta(on));

end

%fprintf('phi_s*phi_r=%f\n',Mode(1,M^2))

fprintf('\rrealization %d, time step %d out of %d, mode pair %d out of %d, %f',kk,j,length(T),k,M^2,on(1))

end

end


















