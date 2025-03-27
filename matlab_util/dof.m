%[S,S_norm,f,ind]=dof(ts,fs,n_fft,n_overlap)
%
% function to determine the frequency dependent rank of vector time series
% over a frequency band of interest
%
% Kevin D. LePage
% SACLANTCEN
% 18/5/98
%
% INPUTS
%
% ts		vector time series (n_time x n_elements)
% fs		sample frequency (Hz)
% n_fft 	desired length of ffts to build up covariances
% n_overlap	number of overlaps in consecutive ffts
%
% OUTPUTS
%
% S		singular value power (n_fft/2 x n_elements)
% S_norm	normalized singular values (first==1) (n_fft x n_elements)
% f		frequencies (Hz) (n_fft/2)
% ind		svd indicies == 1:n_elements

function[S,S_norm,f,ind]=dof(ts,fs,n_fft,n_overlap)

% determine the number of elements

num_n=size(ts,2);

% determine the number of time samples

num_t=size(ts,1);

% determine n_advance

n_advance=n_fft-n_overlap;

% determine the number of spectral estimates that fit into data

num_e=floor(num_t/n_advance)-1;

% predefine the covariance matrix

inter=zeros(n_fft/2,num_n*num_n);

% loop over length of time series available, building up covariance

for j=1:num_e

temp=fft(ts((j-1)*n_advance+1:(j-1)*n_advance+n_fft,:));

for k=1:n_fft/2

inter(k,:)=inter(k,:)+reshape(temp(k,:).'*conj(temp(k,:)),1,num_n*num_n);

if j==num_e

S(k,:)=svd(reshape(inter(k,:),num_n,num_n))';

S_norm(k,:)=S(k,:)/S(k,1);

end

end

end

f=[0:fs/n_fft:fs/2];

ind=[1:num_n];
