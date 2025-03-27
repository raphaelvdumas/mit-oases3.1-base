%[reverb]=gaussian_reverb(wo,d_w,phi,Z,Zs,Zr,phi_ib,phi_rb,Cp,Cg,D,T,ell,flag)
%
% function to determine the product of two gaussian envelopes
%
% Kevin D. LePage
% SACLANTCEN
% 9/8/97
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

S=Cg(:).^(-1);

% dispersion vector

D=D(:);

% we want closed form product, first define some constants

Snm=S*ones(1,M)+ones(M,1)*S.';

snm=Snm(:)*ones(1,M*M);

snmp=ones(M*M,1)*Snm(:)';

Knm=K*ones(1,M)+ones(M,1)*K.';

knm=Knm(:)*ones(1,M*M);

knmp=ones(M*M,1)*Knm(:)';

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

sig=d_w^(-2)-i*(D*ones(1,M)+ones(M,1)*D')*t.*Snm.^(-1);

sig=sig(:);

s1=sig*ones(1,M*M);

s2=ones(M*M,1)*sig';

s1_s2=s1+s2;

s1s2=s1.*s2;

rat2=(1+s1./s2)./s1_s2;

% here is the expression

one=-(t^2*(1+s2./s1).*(s1_s2.^(-1)+1/ell*snmp.^(-2))/2+i*t*knmp./snmp+knmp.^2./(snmp.^2.*rat2*2))./(1+(ell*snmp.^2.*rat2).^(-1));

two=(t^2*snm.^2.*(1+s2./s1).^2.*s1_s2.^(-2).*(1+((1+s2./s1)*ell./s1_s2.*snmp.*snm+(1+s2./s1)./(1+s1./s2).*snm./snmp).^(-1)).^2+2*i*t*snm.*(1+s2./s1)./s1_s2.*(1+((1+s2./s1)*ell./s1_s2.*snmp.*snm+(1+s2./s1)./(1+s1./s2).*snm./snmp).^(-1)).*(knm-knmp.*(1-(1+(ell*snmp.^2.*rat2).^(-1)).^(-1)))-(knm-knmp.*(1-(1+(ell*snmp.^2.*rat2).^(-1)).^(-1))).^2)./((2*snm.^2.*(1+s2./s1)./s1_s2.*(1+s1_s2./(ell*snmp.^2.*(1+s1./s2)))+2/ell)./(1+s1_s2./(ell*snmp.^2.*(1+s1./s2))));


if flag==2

max(max(imag(one+two)))

min(min(imag(one+two)))

figure(2)
subplot(2,2,1)
imagesc(real(one))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(one))
colorbar
colormap('jet')
subplot(2,2,3)
imagesc(10*log10(abs(exp(one))))
colorbar
colormap('jet')
drawnow

figure(3)
subplot(2,2,1)
imagesc(real(two))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(two))
colorbar
colormap('jet')
subplot(2,2,3)
imagesc(10*log10(abs(exp(two))))
colorbar
colormap('jet')
drawnow

elseif flag==1

if(j==floor(j/10)*10)

figure(4)
subplot(2,2,1)
imagesc(real(one+two))
colorbar
colormap('jet')
subplot(2,2,2)
imagesc(imag(one+two))
colorbar
colormap('jet')
subplot(2,2,3)
imagesc(10*log10(abs(exp(one+two))))
colorbar
colormap('jet')
drawnow

end

end
keyboard
onetwo=exp(one+two);

three=(pi*2*s1.^(-1)).^(.5).*(pi*2*s2.^(-1)).^(.5);

four=(pi*(snmp.^2.*rat2/2+1/2/ell).^(-1)).^(.5);

five=(pi*((snm.^2-2*snm.*snmp+snmp.^2+(snm./s1+snmp./s2).^2.*s1s2)./s1_s2/2-(4*(snmp.^2.*rat2/2).^2)./(snmp.^2.*rat2/2+1/ell/2)/4).^(-1)).^(.5);

six=(snm.*snmp).^(.5)*t;

if flag==3

figure(5)

imagesc(10*log10(abs(Mode*(onetwo.*three.*four.*five./six)*Mode')))
colormap('jet')
colorbar
drawnow

%max(max(imag(Mode*(onetwo.*three.*four.*five./six)*Mode')))
%min(min(imag(Mode*(onetwo.*three.*four.*five./six)*Mode')))


end

reverb(:,j)=reshape(Mode*(onetwo.*three.*four.*five./six)*Mode',length(Zr)^2,1);

if j==floor(j/10)*10

figure(1)

imagesc(T(1:j),Zr,10*log10(abs(reverb(1:length(Zr)+1:length(Zr)^2,1:j))))
colormap('jet')
colorbar
drawnow

drawnow

end

end






