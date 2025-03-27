%function[cov]=form_covariance(ts,z,T,nave,flag,z_desired,t_desired)
%
% Kevin D. LePage
% SACLANTCEN
% 12/11/97
%
% function to compute covariance of vector time series obtained over 
% independent realizations
%
% INPUTS
%
% ts		vector time series (nave*length(z) x length(T)
% z		corresponding receiver depths (m) 
% T		corresponding time series (s)
% nave		corresponding number of realizations
% flag		1 for full covariances (default diagonal term only)
% z_desired	desired depths of covariance (default all)
% t_desired	desired times of covariances (default all)
%
% OUTPUTS
%
% cov		sample covariance 
% size full covariance (length(z_desired)^2 x length(t_desired))
% size diagonal variance (length(z_desired) x length(t_desired))

function[cov]=form_covariance(ts,z,T,nave,flag,z_desired,t_desired)

if nargin<5

flag=-1;

end

if nargin<6

z_desired=z;

end

if nargin<7

t_desired=T;

end

NT=length(t_desired);

NZ=length(z_desired);

if flag==1

cov=zeros(NZ^2,NT);

else

cov=zeros(NZ,NT);

end

for tt=1:NT

t_ind=find(T==t_desired(tt));

for j=1:nave

%fprintf('\rtt=%d out of %d, realization=%d out of %d',tt,NT,j,nave)

fprintf('\rtt=%d out of %d',tt,NT)

if flag==1

cov(:,tt)=cov(:,tt)+ts((j-1)*NZ+1:j*NZ,t_ind)*ts((j-1)*NZ+1:j*NZ,t_ind)'/nave;

else

cov(:,tt)=cov(:,tt)+diag(ts((j-1)*NZ+1:j*NZ,t_ind)*ts((j-1)*NZ+1:j*NZ,t_ind)')/nave;

end

end

end