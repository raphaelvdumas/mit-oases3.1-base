%[reverb]=bistar(wo,d_w,phi,Z,Zs,Zr,Yr,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,skew,frac,pow,N)
%
% function to determine realizations of bistatic  reverberation
%
% Kevin D. LePage
% SACLANTCEN
% 11/27/97
%
% INPUTS:
%
%	wo	center frequency (Hz)
%	d_w	effective width of Gaussian pulse (Hz)
%	phi	mode functions (num depths x M)
%	Z	depths of mode functions (num depths x 1) 
%	Zs	source depth (m, scalar)
%	Xr	receiver x locations (m, num rec x 1)
%	Yr	receiver y locations (m, num rec x 1)
%	Zr	receiver depths (m, num rec x 1)
%	phi_ib	mode function incident on bottom (1 x M)
%	phi_rb	mode function reflected from bottom (1 x M)
%	Cp	modal phase speeds (m/s x M)
%	Cg	group speed vector (units m/s x M)
%	D	dispersion term vector (units s^2/m x M)
%	T	desired vector of reverb times (s)
%	ell	corr length scale vector of scattering [lx,ly] (m)
%	skew	skew angle (degrees, zero degrees -> lx along x axis)
%       frac    fractal dimension (2 < frac < 3, -1 for Gaussian)
%	pow	power of scatterer process
%	N	desired number of realizations
%
% OUTPUTS
%
%	reverb	realizations of reverb time series

function[reverb]=realization(wo,d_w,phi,Z,Zs,Xr,Yr,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,skew,frac,pow,N)

% number of modes

M=length(Cp);

MM=ones(M^2,1);

% convert various frequencies to radians

d_w=d_w*2*pi;

wo=wo*2*pi;

% wavenumber vector

K=wo*Cp(:).^(-1);

% slowness vector

S=real(Cg(:).^(-1));

% dispersion vector

D=real(D(:));

% get ranges out to twice the required range

R=max(T)*max(Cg);

% get the "required" range discretization

drx=ell(1)/5;

dry=ell(2)/5;

% determine the number of roughness realizations required in space

num_x=floor(2*R/1024/drx);

num_x=floor(num_x/2)*2;

num_y=floor(2*R/1024/dry);

num_y=floor(num_y/2)*2;

% we want closed form product, first define some constants

Sn=S*ones(1,M);

Sn=Sn(:);

Sm=ones(M,1)*S.';

Sm=Sm(:);

Kn=K*ones(1,M);

Kn=Kn(:);

Km=ones(M,1)*K.';

Km=Km(:);

Dn=D*ones(1,M);

Dn=Dn(:);

Dm=ones(M,1)*D.';;

Dm=Dm(:);

% determine mode shape functions at source depth

Phi_s=interp1(Z,phi,Zs);

% determine mode shape functions at receiver depths

Phi_r=interp1(Z,phi,Zr);

Phi_out=Phi_s(:).*phi_ib(:);

Phi_back=(ones(length(Zr),1)*phi_rb(:).').*Phi_r;

for j=1:length(Zr)

Mode(:,j)=reshape(Phi_out(:)*Phi_back(j,:),M^2,1);

end

% predefine the reverb matrix

reverb=zeros(length(Zr)*N,length(T));

% define the rotated position of all the receivers (rotate to skew angle)

Rx=Xr*cos(skew*pi/180)-Yr*sin(skew*pi/180);

Ry=Xr*sin(skew*pi/180)+Yr*cos(skew*pi/180);

% loop over desired scatterer profiles

for n=1:N

% loop over the number of spatial realizations required 
% for each total realization

for l=1:num_x

for ll=1:num_y

eta=make_rough_2([1024 1024],1024*[drx dry],ell,skew,frac+1,1);

% determine xo and yo of lower lefthand corner

xo=-num_x/2+(l-1);

yo=-num_x/2+(l-1);

toc1=0;
toc2=0;
toc3=0;
toc4=0;
toc5=0;

% get the field due to the various ranges

for j=1:1024

for jj=1:1024

rx=xo+drx*j;

ry=yo+dry*jj;

fprintf('\rrealization %d, patch %d, range %d out of %d (rx=%f, ry=%f),toc4=%d, toc5=%d',n,l,(j-1)*1024+jj,1024^2,rx,ry,toc4,toc5)

toc1=0;
toc2=0;
toc3=0;
toc4=0;
toc5=0;

% get range from source to patch

rs=sqrt(rx^2+ry^2);

% get range from patch to receivers

tic;

rr=((Rx-rx).^2+(Ry-ry).^2).^(.5);

rr=rr(:);

toc1=toc;

A=log(1e-2);

a=1;

for r=1:length(Zr)

b=-2*(Sn*rs+Sm*rr(r));

c=A*2/d_w/d_w+A*i*2*(Dn*rs+Dm*rr(r))+(Sn*rs+Sm*rr(r)).^2;

thigh=max(max([real((-b+(b.^2-4*a*c).^(.5))/2/a) real((-b-(b.^2-4*a*c).^(.5))/2/a)]));

tlow=min(min([real((-b+(b.^2-4*a*c).^(.5))/2/a) real((-b-(b.^2-4*a*c).^(.5))/2/a)]));

on=find(T>tlow & T<thigh);

OO=ones(1,length(on));

tic

win=exp(-(MM*(T(on).^2)-2*(Sn*rs+Sm*rr(r))*T(on)+(Sn*rs+Sm*rr(r)).^2*OO).*((2/d_w^2+i*2*(Dn*rs+Dm*rr(r))).^(-1)*OO));

%win=exp(-((2/d_w^2+i*2*(Dn*rs+Dm*rr(r)).^(-1))*T(on).^2-2*((Sn*rs+Sm*rr(r))./(2/d_w^2+i*2*(Dn*rs+Dm*rr(r))))*T(on))).*(exp(-(Sn*rs+Sm*rr(r)).^2./(2/d_w^2+i*2*(Dn*rs+Dm*rr(r))))*OO);

toc4=toc4+toc;

tic

reverb((n-1)*length(Zr)+r,on)=reverb((n-1)*length(Zr)+r,on)+sum(((Mode(:,r).*(Kn.*Km*rs*rr(r)).^(-.5).*exp(i*(Km*rr(r)+Kn*rs)))*OO).*(MM*exp(-i*(wo*T(on)))).*win*eta(j,jj));

toc5=toc5+toc;

% end of receiver loop r

end

% end of loop over x dimension of individual patches j

end

% end of loop over y dimension of individual patches jj

end

% end of loop over y patches for individual realizations l

end

% end of loop over x patches for individual realizations l

end

% end of loop over total number of required realizations n

end


