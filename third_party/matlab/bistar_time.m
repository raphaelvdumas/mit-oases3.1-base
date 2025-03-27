%[reverb]=bistar_time(wo,d_w,phi,Z,Zs,Zr,Yr,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,skew,frac,pow,N)
%
% function to determine realizations of bistatic  reverberation
% determines total spatial contribution at given time
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

function[reverb]=bistar_time(wo,d_w,phi,Z,Zs,Xr,Yr,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,skew,frac,pow,N)

% number of modes

M=length(Cp);

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

R=max(T)*real(max(Cg));

% get the "required" range discretization

drx=ell(1)/5;

dry=ell(2)/5;

% determine the number of roughness realizations required in space

num_x=ceil(2*(R+max(abs(Xr)))/512/drx);

num_x=ceil(num_x/2)*2;

[aa,bb]=max(abs(Xr));

x_base=Xr(bb);

num_y=ceil(2*(R+max(abs(Yr)))/512/dry);

num_y=ceil(num_y/2)*2;

[aa,bb]=max(abs(Yr));

y_base=Yr(bb);

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

eta=make_rough_2([512 512],512*[drx dry],ell,skew,frac+1,'gauss',1);

eta=ones(size(eta));

% determine xo and yo of lower lefthand corner

xo=(-num_x/2+(l-1))*512*drx+x_base;

yo=(-num_y/2+(ll-1))*512*dry+y_base;

toc4=0;
toc5=0;

% get the field at required times from all contributing ranges

for t=1:length(T)

tt=T(t);

rx=xo+drx*ones(512,1)*[1:512];

ry=yo+dry*[1:512]'*ones(1,512);

% get range from source to patch

rs=(rx.^2+ry.^2).^(.5);

% loop over receivers

for r=1:length(Zr)

% get range from patch to receivers

rr=((Rx(r)-rx).^2+(Ry(r)-ry).^2).^(.5);

fprintf('\rrealization %d, patch %d, receiver %d out of %d, time %d out of %d,toc4=%d, toc5=%d',n,(l-1)*num_y+ll,r,length(Zr),t,length(T),toc4,toc5)

toc4=0;
toc5=0;

% loop over incident modes

for j=1:1

% loop over scattered modes

for jj=M:M

tic

win=exp(-(tt-Sn(j)*rs-Sm(jj)*rr).^2./(2/d_w^2+i*2*(Dn(j)*rs+Dm(jj)*rr)));

toc4=toc4+toc;

tic

win=Mode((jj-1)*M+j,r)*(Kn(j)*Km(jj)*rs.*rr).^(-.5).*exp(i*(Km(jj)*rr+Kn(j)*rs-wo*tt)).*win.*eta;

imagesc(rx(1,:),ry(:,1),real(win+randn(size(win))*1e-32));colormap('jet');axis([[-1 1]*num_x/2*512*drx+x_base [-1 1]*num_y/2*512*drx+y_base]);drawnow;hold on;

reverb((n-1)*length(Zr)+r,t)=reverb((n-1)*length(Zr)+r,t)+sum(sum(win));

toc5=toc5+toc;

% end of loop over incident modes j

end

% end of loop over scattered modes jj

end

% end of receiver loop r

end

% end of loop over time t

end

% end of loop over required number of scattering patches l

end

% end of loop over required number of scattering patches ll

end

% end of loop over total number of required realizations n

end


