%[pp,pp_sta,pp_sta_var]=signal_env(wo,d_w,phi,c,rho,Z,Zs,Zr,Cp,Cg,D,T,r,eof,pow,corr,num,Diag)
%
% function to compute envelope of time series, its expected value
% and its variance, as a function of channel characteristics, 
% bandwidth and the power of the phase and group speed variance 
% in individual modes
% 
% Kevin D. LePage
% SACLANTCEN
% 10/23/97
%
% INPUTS
%
% f	center frequency (Hz)
% d_f	bandwidth (Hz)
% phi	mode shape functions (length(z) x length(Cp))
% c	sound speed vs depth (length(z), m/s)
% rho   density vs depth (length(z), gm/cm^3 or what's assumed in phi)
% z	corresponding depths 
% zs	source depth (in units of z)
% zr	desired receiver depths (same units as z)
% cp	phase velocity (m/s, length m, complex ok)
% cg	group speed (m/s, length m, real)
% d	dispersion (d^2k/dw^2, length m, units s^2 rad/m, real)
% t	reduced time about mode one travel time at range r
% r	desired range (km)
% eof	EOF's of the propagation environment (length(z) x num EOFs)
% pow	variance of each EOF (length num EOFs, units m^2/s^2)
% corr  correlation length scale (m)
% num	number of random realizations required (0 returns unperturbed pp only)
% Diag	if 1 then only give diagonal elements of pp_sta
%
% OUTPUTS
%
% pp		coherent calculation for signal with no variability
%		(has random realizations appended if num>0)
% pp_sta	ensemble average of the short time average or envelope
% pp_sta_var	variance of the above

function[pp,pp_sta,pp_sta_var,t]=signal_env(f,d_f,phi,c,rho,z,zs,zr,cp,cg,d,T,r,eof,pow,corr,num,Diag)

if nargin<18

Diag=1;

end

% convert range to m

r=r*1e3;

T=T(:)';

NN=length(T);

N=length(zr);

% get number of modes

M=size(phi,2);

% define w

w=2*pi*f;

% get bandwidth

dw=2*pi*d_f;

% convert cp to kr

kr=w*cp.^(-1);

kr=kr(:);

% assume e(-iwt) time dependence

kr=real(kr)+i*abs(imag(kr));

% convert group speed to slowness

S=real(cg(:)).^(-1);

% make sure d is real

D=real(d(:));

% make sure pow is a column vector

pow=pow(:);

% since pow is m^2/s^2 and rms of eof is 1/sqrt(Depth), multiply pow by
% Depth to make sure units of realization are in m/s
% Note: eof's must obey int_{0}^{Depth} eof.^2 dz==1 for this to work

pow=pow*max(z);

% make sure corr is a column vector

corr=corr(:);

% determine mode shape functions at source depth

Phi_s=interp1(z,phi,zs);

% determine mode shape functions at receiver depths

Phi_r=interp1(z,phi,zr);

for j=1:M

Mode(:,j)=Phi_r(:,j)*Phi_s(j);

end

% get the inner products of all EOFs with power of mode shape 
% functions and integrate over c^3

% first get dz

dz=abs(min(diff(z)));

Psi=eof'*(phi.^2.*(((c.^(-3)).*(rho.^(-1)))*ones(1,M)))*dz;

% Note: Psi is neof x M 

neof=size(eof,2);

% get wavenumber deviation associated with modes and EOFs

dK=real((ones(neof,1)*(-w^2*kr.^(-1)).').*Psi);

% get the slowness deviation associated with modes and EOFs

dS=real((ones(neof,1)*(-(2-w*S./kr)./kr*w).').*Psi);

% get the curvature deviation associated with modes and EOFs

dD=real((ones(neof,1)*(-(2/w-4*S./kr+2*w*S.^2./kr./kr-w*D./kr)./kr*w).').*Psi);

if nargout>0

KK=kr*ones(1,NN);
SS=S*ones(1,NN);
DD=D*ones(1,NN);
TT=ones(M,1)*T;

pp=Mode*(exp(-i*f*2*pi*(TT+S(1)*r)+i*KK*r).*exp(-((SS-S(1))*r-TT).^2.*(2/dw/dw-4*i*DD*r).^(-1)).*(pi*(1/2/dw/dw-i*DD*r).^(-1)).^(.5).*(KK*r).^(-.5));

%pp=(exp(-i*f*2*pi*(TT+S(1)*r)+i*KK*r).*exp(-((SS-S(1))*r-TT).^2.*(2/dw/dw-2*i*DD*r).^(-1)).*(pi*(2/dw/dw-2*i*DD*r).^(-1)).^(.5).*(KK*r).^(-.5));

% flag for random realizations

if num>0

for k=1:num

rand('seed',sum(100*clock))

fprintf('\rnum=%d out of %d',k,num)

% make range realizations of sound speed perturbations using make_rough.m

%delta_c=zeros(size(eof,1),floor(2^nextpow2(r*(1+exl)/corr*100)));

for l=1:neof

% extra range calculated in order for mean not to be just zero wavenumber 
% bin, whose value is independent of realization length!

exl=rand;

% make the realization (unit variance)

rough=make_rough(2^nextpow2(r*(1+exl)/corr(l)*100),r*(1+exl),corr(l),'goff3',1);

%delta_c=delta_c+eof(:,l)*rough(1:floor(length(rough)/(1+exl)));

% take average and scale according to power in variable pow

ruff(k,l)=sqrt(pow(l))*mean(rough(1:floor(length(rough)/(1+exl))));

end

%save delta_c delta_c

DK=(ruff(k,:)*dK).'*ones(1,NN);
DS=(ruff(k,:)*dS).'*ones(1,NN);
DDD=(ruff(k,:)*dD).'*ones(1,NN);

% corresponding random realization of time series

pp(N*k+1:N*(k+1),:)=Mode*(exp(-i*f*2*pi*(TT+S(1)*r)+i*(KK+DK)*r).*exp(-(((SS-S(1))+DS)*r-TT).^2.*(2/dw/dw-4*i*(DD+DDD)*r).^(-1)).*(pi*(1/2/dw/dw-i*(DD+DDD)*r).^(-1)).^(.5).*(KK*r).^(-.5));

%keyboard

% check to see if coupled mode calculations are desired

if nargout>3

% make the total SSP perturbation realization



end

end

end

end

if nargout>1

pow=pow.*corr;

sigma=r*pi;

% define Theta_nm from memo

Theta_nm=(((dK.^2).'*pow*ones(1,M)+ones(M,1)*pow'*conj(dK.^2))-2*dK'*diag(pow)*conj(dK))*sigma/2;

% define Theta_1 from memo

Theta_1=((dK.*dS).'*pow*ones(1,M)-dS.'*diag(pow)*conj(dK))*sigma;

% define Theta_2 from memo (probably just Theta_1.', but I'm not feeling very
% bright today...)

Theta_2=(ones(M,1)*pow'*(conj(dK).*dS)-dK.'*diag(pow)*dS)*sigma;

% define Theta_11 from memo

Theta_11=(dS.^2).'*pow*ones(1,M)*sigma/2+(1/2/dw/dw-i*D*ones(1,M)*r);

% define Theta_22 from memo (still not feeling bright)

Theta_22=ones(M,1)*pow'*(conj(dS).^2)*sigma/2+(1/2/dw/dw+i*ones(M,1)*D.'*r);

% define Theta_12 from memo

Theta_12=dS.'*diag(pow)*conj(dS)*sigma;

% define the difference between the n and m wavenumbers

Knm=kr*ones(1,M)-ones(M,1)*kr';

% define Tau, the travel time of the individual modes

Tau_n=r*S*ones(1,M);

Tau_n=Tau_n-Tau_n(1);

Tau_m=r*ones(M,1)*S';

Tau_m=Tau_m-Tau_m(1);

% define the denominator of the modal sum

Den=1/r*(kr*kr').^(-.5);

% get the whole expected time series for all mode pairs

% first predefine the answer

if Diag~=1

pp_sta=zeros(N^2,length(T));

else

pp_sta=zeros(N,length(T));

end

% loop over desired time indicies

for j=1:length(T)

fprintf('\rtime=%d out of %d',T(j),max(T))

t=T(j);

ts_m=exp(i*Knm*r-Theta_nm+(Theta_2.^2-(t-Tau_m).^2-i*2*Theta_2.*(t-Tau_m))./Theta_22/4+((Theta_1-(2*i*(Theta_12).*(t-Tau_m)-2*(Theta_2.*Theta_12))./Theta_22/4+i*(t-Tau_n)).^2./(Theta_11-Theta_12.^2./Theta_22/4)/4)).*(Theta_22).^(-.5).*(Theta_11-Theta_12.^2./Theta_22/4).^(-.5).*Den*pi;

if Diag~=1

pp_sta(:,j)=reshape(Mode*ts_m*Mode',N^2,1);

else

pp_sta(:,j)=reshape(diag(Mode*ts_m*Mode'),N,1);

end

end

end

% check to see if variance is desired

if nargout>2

dKn=[];
dSn=[];
dKm=[];
dSm=[];
Dn=[];
Dm=[];

for j=1:M
dKn=[dKn dK];
dKm=[dKm dK(:,j)*ones(1,M)];
dSn=[dSn dS];
dSm=[dSm dS(:,j)*ones(1,M)];
Dn=[Dn;D];
Dm=[Dm;D(j)*ones(M,1)];
end

MM=M^2;

% define Theta_nm from memo

Theta_nmnpmp=((dKn.^2).'*pow*ones(1,MM) ...
+(dKm.^2)'*pow*ones(1,MM) ...
+ones(MM,1)*pow'*dKn.^2 ...
+ones(MM,1)*pow'*conj(dKm.^2) ...
+2*dKn.'*diag(pow)*dKn ...
-2*(dKn.*conj(dKm)).'*pow*ones(1,MM) ...
-2*dKn.'*diag(pow)*conj(dKm) ...
-2*dKm'*diag(pow)*dKn ...
-2*ones(MM,1)*pow'*(dKn.*conj(dKm)) ...
+2*dKm'*diag(pow)*conj(dKm))*sigma/2;

% define Theta_1 from memo

Theta_1=((dKn.*dSn).'*pow*ones(1,MM)+dSn.'*diag(pow)*dKn-(dKm.*dSn).'*pow*ones(1,MM)-dSn.'*diag(pow)*conj(dKm))*sigma;

% define Theta_2 from memo 

Theta_2=-((dKn.*conj(dSm)).'*pow*ones(1,MM)+dSm'*diag(pow)*dKn-(dKm.*conj(dSm)).'*pow*ones(1,MM)-dSm'*diag(pow)*conj(dKm))*sigma;

% define Theta_1p from memo

Theta_1p=(dKn.'*diag(pow)*dSn+ones(MM,1)*pow'*(dKn.*dSn)-dKm.'*diag(pow)*dSn-ones(MM,1)*pow'*(conj(dKm).*dSn))*sigma;

% define Theta_2p from memo

Theta_2p=-(dKn.'*diag(pow)*conj(dSm)+ones(MM,1)*pow'*(dKn.*conj(dSm))-dKm.'*diag(pow)*conj(dSm)-ones(MM,1)*pow'*(conj(dKm).*conj(dSm)))*sigma;

% define Theta_11 from memo

Theta_11=(dSn.^2).'*pow*ones(1,MM)*sigma/2+(1/2/dw/dw-i*Dn*ones(1,MM)*r);

% define Theta_22 from memo

Theta_22=(dSm.^2)'*pow*ones(1,MM)*sigma/2+(1/2/dw/dw+i*Dm*ones(1,MM)*r);

% define Theta_1p1p from memo 

Theta_1p1p=ones(MM,1)*pow'*(dSn.^2)*sigma/2+(1/2/dw/dw-i*ones(MM,1)*Dn.'*r);

% define Theta_2p2p from memo 

Theta_2p2p=ones(MM,1)*pow'*(conj(dSm).^2)*sigma/2+(1/2/dw/dw+i*ones(MM,1)*Dm'*r);

% define Theta_12 from memo

Theta_12=(dSn.*conj(dSm)).'*pow*ones(1,MM)*sigma;

% define Theta_11p from memo

Theta_11p=dSn.'*diag(pow)*dSn*sigma;

% define Theta_12p from memo

Theta_12p=dSn.'*diag(pow)*conj(dSm)*sigma;

% define Theta_21p from memo

Theta_21p=dSm'*diag(pow)*dSn*sigma;

% define Theta_22p from memo

Theta_22p=dSm'*diag(pow)*conj(dSm)*sigma;

% define Theta_1p2p from memo

Theta_1p2p=ones(MM,1)*pow'*(dSn.*conj(dSm))*sigma;

% define the difference between the n and m wavenumbers

Knm=kr*ones(1,M)-ones(M,1)*kr';

Knmnpmp=Knm(:)*ones(1,MM);

Knmnpmp=Knmnpmp+Knmnpmp.';

% define Tau, the travel time of the individual modes

Tau_n=r*S*ones(1,M);

Tau_n=Tau_n-Tau_n(1);

Tau_n=Tau_n(:)*ones(1,MM);

Tau_np=Tau_n.';

Tau_m=r*ones(M,1)*S';

Tau_m=Tau_m-Tau_m(1);

Tau_m=Tau_m(:)*ones(1,MM);

Tau_mp=Tau_m.';

% define the denominator of the modal sum

Den=1/r*(kr*kr').^(-.5);

Den=Den(:)*ones(1,MM);

Den=Den.*Den.';

% define the various integration constants

A_2p=Theta_2p2p;

A_1p=Theta_1p1p-Theta_1p2p.^2./A_2p/4;

C2_12p=-Theta_12p.^2./A_2p/4;

C2_22p=-Theta_22p.^2./A_2p/4;

C_21=Theta_12-2*Theta_22p.*Theta_12p./Theta_2p2p/4;

C_21p=Theta_21p-2*Theta_1p2p.*Theta_22p./A_2p/4;

C_11p=Theta_11p-2*Theta_1p2p.*Theta_12p./A_2p/4;

C_12=2*C_21p.*C_11p./A_1p/4-C_21;

A_2=Theta_22+C2_22p-C_21p.^2./A_1p/4;

A_1=Theta_11+C2_12p-C_11p.^2./A_1p/4-C_12.^2./A_2/4;

clear C2_22p C2_12p

% loop over desired times

for j=1:length(T)

t=T(j);

% this constant often pops up..

const=(Theta_2p-i*(t-Tau_mp))./A_2p/4;

% a, b, c and d are the arguments to the exponents from each of the four
% completions of the squares

a=((Theta_2p-i*(t-Tau_mp)).^2./A_2p/4);

b=(Theta_1p+i*(t-Tau_np)+2*Theta_1p2p.*const).^2./A_1p/4;

c=(Theta_2-i*(t-Tau_m)-2*Theta_22p.*const ...
+2*C_21p.*(Theta_1p+i*(t-Tau_np)+2*Theta_1p2p.*const)./A_1p/4).^2./A_2/4;

d=(Theta_1+i*(t-Tau_n)+2*Theta_12p.*const ...
-2*C_11p.*(Theta_1p+i*(t-Tau_np)+2*Theta_1p2p.*const)./A_1p/4 ...
-2*C_12.*(Theta_2-i*(t-Tau_m)-2*Theta_22p.*const+2*C_21p.* ...
(Theta_1p+i*(t-Tau_np)+2*Theta_1p2p.*const)./A_1p/4)./A_2/4).^2./A_1/4;

% the "intermodal coherence" function

rho=pi^2*(A_1.*A_2.*A_1p.*A_2p).^(-.5).*exp(-Theta_nmnpmp+a+b+c+d);

% loop over desired receiver depths

for k=1:N

%mode=Mode(k,:)'*Mode(k,:);

%mode=mode(:)*mode(:)';

mode=Mode(k,:).'*conj(Mode(k,:));

mode=mode(:)*ones(1,MM);

mode=mode.*mode.';

pp_sta_var(k,j)=sum(sum(mode.*rho.*exp(i*Knmnpmp*r).*Den));

end

fprintf('pp_sta(%d)=%6.2e+i%6.2e, pp_sta_var.^(.5)(%d)=%6.2e+i%6.2e, diff=%6.2e+i%6.2e\n',j,real(pp_sta(1,j)),imag(pp_sta(1,j)),j,real(pp_sta_var(1,j).^(.5)),imag(pp_sta_var(1,j).^(.5)),real(-pp_sta(1,j)+pp_sta_var(1,j).^(.5)),imag(-pp_sta(1,j)+pp_sta_var(1,j).^(.5)))

figure(3)

plot(T(1:j),real(pp_sta(1:N+1:size(pp_sta,1),1:j)),'r')

hold on

plot(T(1:j),real(pp_sta(1:N+1:size(pp_sta,1),1:j))+real((pp_sta_var(:,1:j)-pp_sta(1:N+1:size(pp_sta,1),1:j).^2).^(.5)),'m')

plot(T(1:j),real(pp_sta(1:N+1:size(pp_sta,1),1:j))-real((pp_sta_var(:,1:j)-pp_sta(1:N+1:size(pp_sta,1),1:j).^2).^(.5)),'c')

drawnow

hold off

%keyboard

end

end

t=T+S(1)*r;

%save t t
