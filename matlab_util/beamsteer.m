function [beams]=beamsteer(a,z,ang,freq,cw);

clear i;

for ii=1,size(a,2);
  amean=mean(a(:,ii));
  a(:,ii)=a(:,ii)-amean;
  clear amean;
end;

%a=detrend(a);

nfft=size(a,1);
bla=2.^[0:30];
len=max(find(bla<nfft))
len=len;
a=[a;zeros(2^len-nfft,size(a,2))];
nfft=2^len;
t=0:1/freq:nfft/freq;
fa=fft(a,nfft);
df=1/t(size(t,2));
dw=2*pi*df;
wfreq=2*pi*freq;
w=-wfreq/2:dw:wfreq/2-dw;
w=fftshift(w);

zw(:)=z(:)./cw(:);
zw=zw.';
beams=zeros(size(a,1),length(ang));
for j=1:size(ang,2);
fprintf('\rj=%d out of %d',j,size(ang,2))
  wz=sin(ang(j)/180*pi)*w;
  exps=exp(i*wz(:)*zw);
  fa1=fa.*exps;
  fa2=sum(fa1.');

beams(:,j)=ifft(fa2.');

end;






