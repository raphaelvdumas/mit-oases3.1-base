%[reverb]=real_rd(f,df,mode,Z,Zr,phi_ib,phi_rb,km,sm,dm,T,R,ell,frac,pow,N)
%
% function to determine realizations of reverberation for 
% range dependent environments
%
% Kevin D. LePage
% SACLANTCEN
% 4/8/98
%
% INPUTS:
%
%	f	center frequency (Hz)
%	df	effective width of Gaussian pulse (Hz)
%	phi	mode functions at source-receiver range (num depths x M)
%	Z	depths of mode functions (num depths x 1) 
%	Zr	receiver depths (m, num rec depths x 1)
%	phi_ib	mode AMPLITUDE incident on bottom (FROM RAM) (M x num_R)
%	phi_rb	mode function reflected from bottom (M x num_R)
%	km	effective modal wavenumbers (rad/m) (M x num_R)
%	sm	effective group slowness vector (units s/m) (M x num_R)
%	dm	effective dispersion term vector (units s^2/m) (M x num_R)
%	T	desired vector of reverb times
%       R	ranges at which phi_ib, phi_rb, Cp, Cg and D are evaluated (m)
%	ell	correlation length scale of scattering (m)
%       frac    fractal dimension (2 < frac < 3, -1 for Gaussian)
%	pow	power of scatterer process
%	N	desired number of realizations
%
% OUTPUTS
%
%	reverb	sta of reverb

function[reverb]=real_rd(f,df,phi,Z,Zr,phi_ib,phi_rb,K,S,D,T,R,corr,frac,pow,N)

% number of modes

M=size(phi,2);

% number of ranges

N=size(K,2);

% convert various frequencies to radians

d_w=df*2*pi;

wo=f*2*pi;

% e-iwt time dependence

K=real(K)+i*abs(imag(K));

% slowness vector

S=real(S);

% dispersion vector

D=real(D);

% get ranges out to twice the required range

max_R=real(max(T)/min(min(S)));

% get the "required" range discretization

dr=real(min(pi/2/real(max(max(K))),corr/5));

% get the step size

dR=diff(R);

% if the diff of dR is equal to zero, we can do things faster

if sum(diff(dR))<1

dR=dR(1);

else

fprintf('cannot deal with non-uniform spacing right now, breaking\n')

break

end

% make the range discretization an integer fraction of the step size

dr=dR/ceil(dR/dr);

fac=round(dR/dr);

% get the range vector

r=[dr:dr:max(R)];

% get the spatial filter function to convolve with the scatterer 
% distribution

if (frac>0)

model=['goff' int2str((frac-2)*2+2)]

else

model='gauss'

end

% determine mode shape functions at receiver depths

Mode=interp1(Z,phi,Zr);

% predefine the reverb matrix

reverb=zeros(length(Zr)*N,length(T));

% get the field at the desired times

for kk=1:N

l=(max(R)-dr)*2^nextpow2(length(r))/length(r)

eta=make_rough(2^nextpow2(length(r)),(max(R)-dr)*2^nextpow2(length(r))/length(r),corr*sqrt(2),model,1)'/sqrt(corr^2*4*pi)*sqrt(2)*pi;

for j=1:length(T)

t=T(j);

for k=1:M^2

Knm=K(floor((k-1)/M)+1,:)+K(k-floor((k-1)/M)*M,:);

Snm=S(floor((k-1)/M)+1,:)+S(k-floor((k-1)/M)*M,:);

Dnm=D(floor((k-1)/M)+1,:)+D(k-floor((k-1)/M)*M,:);

phiib=phi_ib(floor((k-1)/M)+1,:);

phirb=phi_rb(k-floor((k-1)/M)*M,:);

% find closest values of Snm

[val,ind]=min(t-Snm.*R);

Snm_local=Snm(ind);

Dnm_local=Dnm(ind);

A=1e-2;

a=Snm_local^2;

b=-log(A)*i*2*Dnm_local-2*Snm_local*t;

c=t^2+log(A)*(4/d_w/d_w);

rhigh=max(real((-b+(b^2-4*a*c).^(.5))/2/a),real((-b-(b^2-4*a*c).^(.5))/2/a));

rlow=min(real((-b+(b^2-4*a*c).^(.5))/2/a),real((-b-(b^2-4*a*c).^(.5))/2/a));

on=[max(1,real(rlow/dr)):max(1,min(length(r),real(rhigh/dr)))];

Knm=reshape(ones(fac,1)*Knm(ceil(on/fac)),1,length(on)*fac);

Knm=Knm(1:length(on));

Snm=reshape(ones(fac,1)*Snm(ceil(on/fac)),1,length(on)*fac);

Snm=Snm(1:length(on));

Dnm=reshape(ones(fac,1)*Dnm(ceil(on/fac)),1,length(on)*fac);

Dnm=Dnm(1:length(on));

phiib=reshape(ones(fac,1)*phiib(ceil(on/fac)),1,length(on)*fac);

phiib=phiib(1:length(on));

phirb=reshape(ones(fac,1)*phi_rb(ceil(on/fac)),1,length(on)*fac);

phirb=phirb(1:length(on));

win=exp(-(t-Snm.*r(on)).^2.*(4/d_w^2-i*2*Dnm.*r(on)).^(-1));

win=win.*(pi*(1/d_w^2-i*Dnm.*r(on)/2).^(-1)).^(.5);

reverb(length(Zr)*(kk-1)+1:length(Zr)*kk,j)=reverb(length(Zr)*(kk-1)+1:length(Zr)*kk,j)+Mode(k-floor((k-1)/M)*M)*((win.*exp(-i*(wo*t-Knm.*r(on))).*phiib.*phirb/sqrt(K(floor((k-1)/M)+1)*K(k-M*floor((k-1)/M))))*eta(on));

end

%fprintf('phi_s*phi_r=%f\n',Mode(1,M^2))

fprintf('\rrealization %d, time step %d out of %d, mode pair %d out of %d, %f',kk,j,length(T),k,M^2,on(1))

end

end


















