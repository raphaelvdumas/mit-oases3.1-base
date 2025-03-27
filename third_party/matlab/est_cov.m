%function[cov]=est_cov(ts,dt,freq,n)
%
% function to load fname containing array timeseries and estimate
% covariance matrix at freq freq using hanning weighted snapshots 
% n samples, overlapped 50%.
%
% Henrik Schmidt
% SACLANTCEN
% 280703
%
% INPUTS
%
% ts		timeseries (n_time x n_elements);
% dt            sampling interval
% freq		frequency for covariance matrix
% n		samples in each snapshot
%
% OUTPUTS
%
% cov	        covariance matrix at frequency freq

  function[cov]=est_cov(ts,dt,freq,n)

  fs=1/dt;
  nt=size(ts,1);
  nelm=size(ts,2);
  T=n*dt;
  df=1.0/T;
  nf=freq/df+1;
  nh=n/2;
  nsnap=(nt-nh)/nh-2
  hh=hanning(n);
  cov=zeros(nelm,nelm);
  x=[1:nelm];
  y=[nelm:-1:1];
  for isnap=1:nsnap
   it1=isnap*nh+1
   it2=it1+n-1
   for ielm=1:nelm
    ff=fft(ts(it1:it2,ielm).*hh);
    fc(ielm)=ff(nf);
   end
   cov=cov+fc'*fc/nsnap;
% plot
   tit= [ 'Covariance - ' num2str(isnap) ' ensemples']
   wavei(flipud(dba(cov)));
   title(tit);
   drawnow
  end
  cc=[real(reshape(cov,nelm*nelm,1)) imag(reshape(cov,nelm*nelm,1)) ];
  save -ascii cov.asc cc
end





