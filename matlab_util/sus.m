%[fp,p]=sus(w,r,z,freq,nt,nb);
%
% Function to generate time series of SUS charges based
% on J. Wakeley Jr., JASA 63 (6) June 1978, p1820-1823
%
%    w=weight in kg
%    r=range in m
%    z depth in m
%    freq=sampling frequency in Hz
%    nt=number of time samples
%    nb=number of bubble pulses
%
%    p=pressure time series out in uPa
%   fp=fft of p

function[fp,p]=sus(w,r,z,freq,nt,nb);

%hydrostatic depth
if(nb > 4)
  fprintf('Too many bubble pulses, maximum is 4\n')
  break;
end;
z0=z+10.1;
% weight in gram
wg=1000*w;
w13=wg^(1/3);
wr=wg^(1/3)/r;
% time
dt=1/freq;
t=0:dt:(nt-1)*dt;
% positive pressures:
pp(1)=3.74e12*wr^(1.13);
pp(2)=9.02e11*wr;
pp(3)=0.22*pp(2);
pp(4)=0.1*pp(2);
pp(5)=0.03*pp(2);
% bubble pulse period:
tb(1)=0;
tb(2)=0.21*w13*z0^(-5/6);
tb(3)=1.71*tb(2);
tb(4)=2.28*tb(2);
tb(5)=2.81*tb(2);
tb;
% time constants:
ts(1)=1.75e-5*w13*wr^(-.26);
ts(2)=1.48e-4*w13*z0^(-1/6);
ts(3)=1.91*ts(2);
ts(4)=2.79*ts(2);
ts(5)=2.79*ts(2);
% negative pressures:
for k=2:5
  pn(k-1)=-pi/(2*(tb(k)-tb(k-1)))*(pp(k-1)*ts(k-1)+ ...
           pp(k)*ts(k));
end;
% pressure signature
p=zeros(size(1:nt));
p=pp(1)*exp(-t/ts(1));
for k=1:nb
  u1=zeros(size(t));
  u2=zeros(size(t));
  ind1=fix(tb(k+1)/dt)+1;
  ind2=fix(tb(k)/dt)+1;
  u1(ind1:nt)=ones(size(1:nt-ind1+1));
  u2(ind2:nt)=ones(size(1:nt-ind2+1));
  p=p+pp(k+1)*exp(-abs((t-tb(k+1))/ts(k+1)))+pn(k)*sin(pi* ...
    (t-tb(k))/(tb(k+1)-tb(k))).*(u2-u1);
end;
fp=conj(fft(p));
fp=fp(1:nt/2);
end;
