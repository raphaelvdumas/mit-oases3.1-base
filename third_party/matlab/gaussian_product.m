%[env_prod]=gaussian_xform(d_w,Cg1,Cg2,D1,D2,r,t,flag)
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
%	r	range (km)
% 	t	desired time points about group delay
%       flag    ==1 for analytic computation
%
% OUTPUTS
%
%	env_prod	complex envelope product 

function[env_prod]=gaussian_product(d_w,Cg1,Cg2,D1,D2,r,t,flag)

t=t(:);

t=t';

d_w=d_w*2*pi;

S1=1/Cg1;

S2=1/Cg2;

% range

R=r*1000;

% time at actual range

t=(t+R*S1);

if flag~=1

% define vector of frequencies about center frequency\

W=[-d_w*3:d_w/10:d_w*3]'*ones(1,length(t));

% define times around time of stationary response

T=ones(size(W,1),1)*t;

% determine complex envelope at all times

kern1=exp(-(W.^2/2/d_w/d_w)).*exp(-i*(W.*(T-S1*R))).*exp(i*W.^2*D1*R/2);

kern2=exp(-(W.^2/2/d_w/d_w)).*exp(-i*(W.*(T-S2*R))).*exp(i*W.^2*D2*R/2);

% sum down columns for envelope

env1=sum(kern1)*d_w/10;

env2=sum(kern2)*d_w/10;

env_prod=env1.*env2;

% analytic envelope

env_anal1=exp(-(S1*R-T(1,:)).^2/4/(1/2/d_w^2-i*D1*R/2))*sqrt(pi/(1/2/d_w^2-i*D1*R/2));

env_anal2=exp(-(S2*R-T(1,:)).^2/4/(1/2/d_w^2-i*D2*R/2))*sqrt(pi/(1/2/d_w^2-i*D2*R/2));

else

% we want closed form product, first define some constants

a=S1*R;

b=S2*R;

sig1=d_w^(-2)-i*D1*R;

sig2=d_w^(-2)-i*D2*R;

% here is the expression
keyboard
env_prod=exp(-(t-(a/2/sig1+b/2/sig2)/(1/2/sig1+1/2/sig2)).^2/2/(sig1*sig2/(sig1+sig2)))*sqrt(pi*2/sig1)*sqrt(pi*2/sig2)*exp(-(a^2-2*a*b+b^2)/(sig1+sig2)/2);

end


