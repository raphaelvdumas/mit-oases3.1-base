%[pp_sta,pp_sta_var,pp]=signal_env(wo,d_w,phi,c,rho,Z,Zs,Zr,Cp,Cg,D,T,r,eof,pow,corr)
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
%
% OUTPUTS
%
% pp_sta	ensemble average of the short time average or envelope
% pp_sta_var	variance of the above
% pp		coherent calculation for signal with no variability

function[pp_sta,pp_sta_var,pp]=signal_env(f,d_f,phi,c,rho,z,zs,zr,cp,cg,d,T,r,eof,pow,corr)

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

dK=(ones(neof,1)*(-w^2*kr.^(-1)).').*Psi;

% get the slowness deviation associated with modes and EOFs

dS=(ones(neof,1)*(-(2+w*S./kr)./kr*w).').*Psi;

% define Theta_nm from memo

Theta_nm=(((dK.^2).'*pow*ones(1,M)+ones(M,1)*pow'*(dK.^2))-2*dK'*diag(pow)*conj(dK))*r*corr/2;

% define Theta_1 from memo

Theta_1=((dK.*dS).'*pow*ones(1,M)-dS.'*diag(pow)*conj(dK))*r*corr;

% define Theta_2 from memo (probably just Theta_1.', but I'm not feeling very
% bright today...)

Theta_2=(ones(M,1)*pow'*(conj(dK).*dS)-dK.'*diag(pow)*dS)*r*corr;

% define Theta_11 from memo

Theta_11=(dS.^2).'*pow*ones(1,M)*r*corr/2+(1/2/dw/dw-i*D*ones(1,M)*r);

% define Theta_22 from memo (still not feeling bright)

Theta_22=ones(M,1)*pow'*(conj(dS).^2)*r*corr/2+(1/2/dw/dw+i*ones(M,1)*D.'*r);

% define Theta_12 from memo

Theta_12=dS.'*diag(pow)*conj(dS)*r*corr;

% define the difference between the n and m wavenumbers

Knm=kr*ones(1,M)-ones(M,1)*kr';

% define Tau, the travel time of the individual modes

Tau_n=r*S*ones(1,M);

Tau_n=Tau_n-Tau_n(1);

Tau_m=r*ones(M,1)*S';

Tau_m=Tau_m-Tau_m(1);

% define the denominator of the modal sum

Den=1/r*(kr*kr').^(.5);

% get the whole expected time series for all mode pairs

% first predefine the answer

pp_sta=zeros(length(zr)^2,length(T));

for j=1:length(T)

fprintf('\rtime=%d out of %d',T(j),max(T))

t=T(j);

ts_m=exp(i*Knm*r-Theta_nm+(Theta_2.^2-(t-Tau_m).^2-i*2*Theta_2.*(t-Tau_m))./Theta_22/4+((Theta_1+(2*i*(Theta_12).*(t-Tau_m)-2*(Theta_2.*Theta_12))./Theta_22/4+i*(t-Tau_n)).^2./(Theta_11-Theta_12.^2./Theta_22/4)/4)).*(Theta_22).^(-.5).*(Theta_11-Theta_12.^2./Theta_22/4).^(-.5).*Den*pi;

pp_sta(:,j)=reshape(Mode*ts_m*Mode',length(zr)^2,1);

end

if nargout>1

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

dKm=

% define Theta_nm from memo

Theta_nmnpmp=((dKn.^2).'*pow*ones(1,M^2)+(dKm.^2)'*pow*ones(1,M^2)+ones(M^2,1)*diag(pow)*dKn.^2+ones(M^2,1)*diag(pow)*conj(dKm.^2)+2*dKn.'*diag(pow)*dKn-2*(dKn.*conj(dKm)).'*pow*ones(1,M^2)-2*dKn.'*diag(pow)*conj(dKm)-2*dKm'*diag(pow)*dKn-2*ones(M^2,1)*diag(pow)*(dKn.*conj(dKm))+2*dKm'*diag(pow)*conj(dKm))*r*corr/2;

% define Theta_1 from memo

Theta_1=((dKn.*dSn).'*pow*ones(1,M^2)+dSn.'*diag(pow)*dKn-(dKm.*dSn).'*pow*ones(1,M^2)-dSn.'*diag(pow)*conj(dKm))*r*corr;

% define Theta_2 from memo 

Theta_2=((dKn.*conj(dSm)).'*pow*ones(1,M^2)+dSm'*diag(pow)*dKn-(dKm.*conj(dSm)).'*pow*ones(1,M^2)-dSm'*diag(pow)*conj(dKm))*r*corr;

% define Theta_1p from memo

Theta_1p=(dKn.'*pow*dSn+ones(M^2,1)*diag(pow)*(dKn.*dSn)-dKm.'*pow*dSn-ones(M^2,1)*diag(pow)*conj(dKm).*dSn)*r*corr;

% define Theta_2p from memo

Theta_2p=(dKn.'*pow*conj(dSm)+ones(M^2,1)*diag(pow)*(dKn.*conj(dSm))-dKm.'*pow*conj(dSm)-ones(M^2,1)*diag(pow)*conj(dKm).*conj(dSm))*r*corr;

% define Theta_11 from memo

Theta_11=(dSn.^2).'*pow*ones(1,M^2)*r*corr/2+(1/2/dw/dw-i*Dn*ones(1,M^2)*r);

% define Theta_22 from memo

Theta_22=(dSm.^2)'*pow*ones(1,M^2)*r*corr/2+(1/2/dw/dw+i*Dm*ones(1,M^2)*r);

% define Theta_1p1p from memo 

Theta_1p1p=ones(M,1)*pow'*(dSn.^2)*r*corr/2+(1/2/dw/dw-i*ones(M^2,1)*Dn.'*r);

% define Theta_2p2p from memo 

Theta_2p2p=ones(M,1)*pow'*(conj(dSm).^2)*r*corr/2+(1/2/dw/dw+i*ones(M^2,1)*Dm'*r);

% define Theta_12 from memo

Theta_12=-(dSn.*conj(dSm)).'*pow*ones(1,M^2)*r*corr;

% define Theta_11p from memo

Theta_11p=dSn.'*diag(pow)*dSn*r*corr;

% define Theta_12p from memo

Theta_12p=-dSn.'*diag(pow)*conj(dSm)*r*corr;

% define Theta_21p from memo

Theta_21p=-dSm'*diag(pow)*dSn*r*corr;

% define Theta_22p from memo

Theta_22p=dSm'*diag(pow)*conj(dSm)*r*corr;

% define Theta_1p2p from memo

Theta_1p2p=-ones(M^2,1)*diag(pow)*(dSn.*conj(dSm))*r*corr;

% define the difference between the n and m wavenumbers

Knm=kr*ones(1,M)-ones(M,1)*kr';

Knmnpmp=Knm(:)*ones(1,M^2);

Knmnpmp=Knmnpmp+Knmnpmp.';

% define Tau, the travel time of the individual modes

Tau_n=r*S*ones(1,M);

Tau_n=Tau_n-Tau_n(1);

Tau_n=Tau_n(:)*ones(1,M^2)

Tau_np=Tau_n.';

Tau_m=r*ones(M,1)*S';

Tau_m=Tau_m-Tau_m(1);

Tau_m=Tau_m(:)*ones(1,M^2)

Tau_mp=Tau_m.';

% define the denominator of the modal sum

Den=1/r*(kr*kr').^(.5);

Den=Den(:)*ones(1,M^2);

Den=Den.*Den.';



end

if nargout>2

end