%[env_prod]=gaussian_xform(d_w,Cg1,Cg2,D1,D2,ro,r,flag)
%
% function to determine the product of two gaussian envelopes
%
% Kevin D. LePage
% SACLANTCEN
% 9/5/97
%
% INPUTS:
%
%	d_w	effective width of Gaussian (Hz)
%	Cg1	group speed 1 (units m/s)
%	Cg2	group speed 2 (units m/s)
%	D1	dispersion term 1 (units s^2/m)
%	D2	dispersion term 2 (units s^2/m)
%	ro	desired center range (km)
%	r	desired range points around center range (m)
%       flag    ==1 for analytic computation
%
% OUTPUTS
%
%	env_prod	complex envelope product 

function[env_prod]=gaussian_product(d_w,Cg1,Cg2,D1,D2,ro,r,flag)

r=r(:);

r=r';

d_w=d_w*2*pi;

S1=1/Cg1;

S2=1/Cg2;

% center range

Ro=ro*1000;

%all ranges

R=ro*1000+r;

% time at center range

T=ro*1000*S1;

if flag~=1

% define vector of frequencies about center frequency\

W=[-d_w*3:d_w/10:d_w*3]'*ones(1,length(r));

% define ranges around range of stationary response

R=ones(size(W,1),1)*R;

% determine complex envelope at all ranges

kern1=exp(-(W.^2/2/d_w/d_w)).*exp(-i*(W.*(T-S1*R))).*exp(i*W.^2*D1.*R/2);

kern2=exp(-(W.^2/2/d_w/d_w)).*exp(-i*(W.*(T-S2*R))).*exp(i*W.^2*D2.*R/2);

% sum down columns for envelope

env1=sum(kern1)*d_w/10;

env2=sum(kern2)*d_w/10;

env_prod=env1.*env2;

% analytic envelope

env_anal1=exp(-((S1*R(1,:)-T).^2/4)./(1/2/d_w^2-i*D1*R(1,:)/2)).*sqrt(pi*(1/2/d_w^2-i*D1*R(1,:)/2).^(-1));

env_anal2=exp(-((S2*R(1,:)-T).^2/4)./(1/2/d_w^2-i*D2*R(1,:)/2)).*sqrt(pi*(1/2/d_w^2-i*D2*R(1,:)/2).^(-1));

else

% we want closed form product, first define some constants

a=S1*Ro;

b=S2*R;

sig1=d_w^(-2)-i*D1*Ro;

sig2=d_w^(-2)-i*D2*Ro;
 
% here is the expression

env_prod=exp(-((T-(a./sig1/2+b./sig2/2)./(sig1.^(-1)/2+sig2.^(-1)/2)).^2/2)./(sig1.*sig2.*((sig1+sig2).^(-1)))).*sqrt(pi*2*sig1.^(-1)).*sqrt(pi*2*sig2.^(-1)).*exp(-(a.^2-2*a.*b+b.^2)./(sig1+sig2)/2);

end


