%[reverb]=gaussian_reverb_test(wo,d_w,phi_s,phi_ib,phi_rb,phi_r,Cp,Cg,D,T,ell,flag)
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
%	phi_s	mode shape function evaluated at source depth
%	phi_ib	mode shape function incident on bottom
%	phi_rb	mode shape function reflected from bottom
%	phi_r	mode shape function eval at receiver depth
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

function[reverb]=gaussian_reverb_test(wo,d_w,phi_s,phi_ib,phi_rb,phi_r,Cp,Cg,D,T,ell,flag)

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

Snm=S*ones(1,M)+ones(M,1)*S';

snm=Snm(:)*ones(1,M*M);

snmp=ones(M*M,1)*Snm(:)';

Knm=K*ones(1,M)+ones(M,1)*K';

knm=Knm(:)*ones(1,M*M);

knmp=ones(M*M,1)*Knm(:)';

for j=1:length(T)

fprintf('\rj=%d out of %d',j,length(T))

t=T(j);

Phi_out=phi_s(:)*phi_ib(:)';

Phi_out=Phi_out(:);

Phi_back=phi_rb(:)*phi_r(:)';

Phi_back=Phi_back(:);

Phi_total=Phi_out*Phi_back';

sig=d_w^(-2)-i*(D*ones(1,M)+ones(M,1)*D')*t.*Snm.^(-1);

sig=sig(:);

s1=sig*ones(1,M*M);

s2=ones(M*M,1)*sig';

s1_s2=s1+s2;

s1s2=s1.*s2;

rat2=(1+s1./s2)./s1_s2;

% here is the expression

one=-(t^2*s1_s2./s1s2/2-(4*t^2*snmp.^2.*rat2.^2/4-4*i*knmp*t.*snmp.*rat2/2-knmp.^2)./(snmp.^2.*rat2/2+1/ell/2)/4);

one_prime=-(t^2*(1+s2./s1)./s1_s2/2+i*t*knmp./snmp+knmp.^2./(snmp.^2.*rat2*2))./(1+s1_s2./(ell*snmp.^2.*(1+s1./s2)));

one-one_prime

two=(((4*(2*t*snmp.*rat2/2-i*knmp).*(snmp.^2.*rat2/2))./(snmp.^2.*rat2/2+1/ell/2)/4-t*(snm./s1+snmp./s2)-i*(knm-knmp)).^2)./((snm.^2-2*snm.*snmp+snmp.^2+(snm./s1+snmp./s2).^2.*s1s2)./s1_s2/2-(4*(snmp.^2.*rat2/2).^2)./(snmp.^2.*rat2/2+1/ell/2)/4)/4;

if flag==1

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

onetwo=exp(one+two);

three=(pi*2*(sig*ones(1,M*M)).^(-1)).^(.5).*(pi*2*(ones(M*M,1)*sig').^(-1)).^(.5);

four=(pi*(snmp.^2.*rat2/2+1/2/ell).^(-1)).^(.5);

five=(pi*((snm.^2-2*snm.*snmp+snmp.^2+(snm./s1+snmp./s2).^2.*s1s2)./s1_s2/2-(4*(snmp.^2.*rat2/2).^2)./(snmp.^2.*rat2/2+1/ell/2)/4).^(-1)).^(.5);

six=snm*t;

if flag==1

figure(5)

imagesc(10*log10(abs(onetwo.*three.*four.*five.*Phi_total./six)))
colormap('jet')
colorbar
drawnow

end

reverb(j)=sum(sum(onetwo.*three.*four.*five.*Phi_total./six));

if j==floor(j/100)*100

figure(1)

plot(T(1:j),10*log10(abs(reverb)),'b')

drawnow

end

end






