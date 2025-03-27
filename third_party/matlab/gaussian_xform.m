%[env,env_anal,ts]=gaussian_xform(wo,d_w,C,Cg,D,r,t)
%
% function to determine the time series of a gaussian pulse with dispersion
%
% Kevin D. LePage
% SACLANTCEN
% 9/4/97
%
% INPUTS:
%
%	wo	center frequency (Hz)
%	d_w	effective width of Gaussian (Hz)
%	C	sound speed (m/s)
%	Cg	group speed (units m/s)
%	D	dispersion term (units s^2/m)
%	r	range (km)
% 	t	desired time points about group delay
%
% OUTPUTS
%
%	env	complex envelope from computation
%	env_analcomplex envelope from analytic evaluation of integral
%	ts	time series

function[env,env_anal,ts]=gaussian_xform(wo,d_w,C,Cg,D,r,t)

t=t(:);

t=t';

wo=wo*2*pi;

d_w=d_w*2*pi;

K=wo/C;

S=1/Cg;

% define vector of frequencies about center frequency\

W=[-d_w*3:d_w/10:d_w*3]'*ones(1,length(t));

% range

R=r*1000;

% define times around time of stationary response

T=ones(size(W,1),1)*(t+R*S);

% determine complex envelope at all times

%keyboard

kern=exp(-(W.^2/2/d_w/d_w)).*exp(-i*(W.*(T-S*R))).*exp(i*W.^2*D*R/2);

% sum down columns for envelope

env=sum(kern)*d_w/10;

% analytic envelope

env_anal=exp(-(S*R-T(1,:)).^2/4/(1/2/d_w^2-i*D*R/2))*sqrt(pi/(1/2/d_w^2-i*D*R/2));

% modulate by carrier and sum and take real part for time series

ts=sum(kern.*(exp(-i*wo*T)*exp(i*K*R)))*d_w/10;



