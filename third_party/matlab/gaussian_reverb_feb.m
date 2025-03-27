%[reverb]=gaussian_reverb_feb(wo,d_w,phi,Z,Zs,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,flag)
%
% function to determine sta of reverberation in February
%
% Kevin D. LePage
% SACLANTCEN
% 2/8/98
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
%       flag    ==1 for intermediate plots of kernel
%
% OUTPUTS
%
%	reverb	sta of reverb

function[reverb]=gaussian_reverb(wo,d_w,phi,Z,Zs,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,flag)

% correlation length

ell=ell^2;

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

% we want closed form product, first define some constants

Snm=S*ones(1,M)+ones(M,1)*S.';

snm=Snm(:)*ones(1,M*M);

snmp=ones(M*M,1)*Snm(:)';

Knm=K*ones(1,M)+ones(M,1)*K.';

knm=Knm(:)*ones(1,M*M);

knmp=ones(M*M,1)*Knm(:)';

knmnmp=K*K.';

knmnmp=knmnmp(:);

knmnmp=knmnmp*knmnmp';

% determine mode shape functions at source depth

Phi_s=interp1(Z,phi,Zs);

% determine mode shape functions at receiver depths

Phi_r=interp1(Z,phi,Zr);

Phi_out=Phi_s(:).*phi_ib(:);

Phi_back=(ones(length(Zr),1)*phi_rb(:).').*Phi_r;

for j=1:length(Zr)

Mode(j,:)=reshape(Phi_out(:)*Phi_back(j,:),1,M^2);

end

% predefine the reverb matrix

reverb=zeros(size(Mode,1)^2,length(T));

for j=1:length(T)

fprintf('\rj=%d out of %d',j,length(T))

t=T(j);

sig=d_w^(-2)-i*(D*ones(1,M)+ones(M,1)*D.')*t.*Snm.^(-1)/2;

sig=sig(:);

s1=sig*ones(1,M*M);

s1i=s1.^(-1);

s2=ones(M*M,1)*sig';

s2i=s2.^(-1);

ratio=(1+2*s2.*(snmp.^(-2))/ell).^(-1);

% here is the expression

one=-(t^2*(s1i/4+s2i.*(1-ratio)/4)+i*t*knmp./snmp.*ratio+knmp.^2.*s2.*snmp.^(-2).*ratio);

two=(t*(snmp.*s2i.*(ratio-1)/2-snm.*s1i/2)-i*(knm-knmp.*(1-ratio))).^2./(snm.^2.*s1i/4+snmp.^2.*s2i.*(1-ratio)/4)/4;

if flag==2

max(max(imag(one+two)))

min(min(imag(one+two)))

max(max(real(one+two)))

min(min(real(one+two)))

figure(2)
subplot(2,2,1)
imagesc(real(one))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(one))
colorbar
colormap('jet')

wavei(dbp(abs(exp(one))),[1 M^2],[1 M^2],-100,0,4,3)

figure(3)
subplot(2,2,1)
imagesc(real(two))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(two))
colorbar
colormap('jet')

wavei(dbp(abs(exp(two))),[1 M^2],[1 M^2],-100,0,4,3)

elseif flag==1

%if(j==floor(j/10)*10)

%figure(1)
subplot(2,2,1)
imagesc(real(one+two))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(one+two))
colorbar
colormap('jet')

wavei(real(one+two),[1 M^2],[1 M^2],-20,0,4,1)
wavei(imag(one+two),[1 M^2],[1 M^2],-2000,2000,4,2)
wavei(dbp(abs(exp(one+two))),[1 M^2],[1 M^2],-100,0,4,3)

%end

end

onetwo=one+two;

indxx=find(real(onetwo)>1);

if length(indxx) > 0

fprintf('\rwarning, one+two>1, in %d places, replacing with 0',length(indxx))

onetwo(indxx)=zeros(size(indxx));

end

onetwo=exp(onetwo);

three=(pi*s1i).^(.5).*(pi*s2i).^(.5);

four=(pi*(snmp.^2.*s2i/4+1/2/ell).^(-1)).^(.5);

five=(pi*(snm.^2.*s1i/4+snmp.^2.*s2i.*(1-ratio)/4).^(-1)).^(.5);

six=(snm.*snmp./knmnmp).^(.5)/t;

seven=2*pi^2*(2*pi*ell).^(-.5);

if flag==3

figure(5)

imagesc(10*log10(abs(Mode*(onetwo.*three.*four.*five.*six)*Mode')))
colormap('jet')
colorbar
drawnow

%max(max(imag(Mode*(onetwo.*three.*four.*five./six)*Mode')))
%min(min(imag(Mode*(onetwo.*three.*four.*five./six)*Mode')))


end

reverb(:,j)=reshape(Mode*(onetwo.*three.*four.*five.*six*seven)*Mode',length(Zr)^2,1);

m=length(Cp);

%keyboard
%fprintf('\none(m+1)=%f+i%f\n',one(m+1),imag(one(m+1)))
%fprintf('two(m+1)=%f+i%f\n',two(m+1),imag(two(m+1)))
%fprintf('onetwo(m+1)=%f+i%f\n',onetwo(m+1),imag(onetwo(m+1)))
%fprintf('three(m+1)=%f+i%f\n',three(m+1),imag(three(m+1)))
%fprintf('four(m+1)=%f+i%f\n',four(m+1),imag(four(m+1)))
%fprintf('five(m+1)=%f+i%f\n',five(m+1),imag(five(m+1)))
%fprintf('six(m+1)=%f+i%f\n',six(m+1),imag(six(m+1)))



if j==floor(j/10)*10

figure(1)

imagesc(T(1:j),Zr,10*log10(abs(reverb(1:length(Zr)+1:length(Zr)^2,1:j))))
colormap('jet')
colorbar
drawnow

drawnow

end

end






