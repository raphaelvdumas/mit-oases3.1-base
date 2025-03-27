function [ug,f,gm] = gabor(t,sig,range,nfft,overlap)
n=length(t);
dt = t(2)-t(1);
fs = 1/dt;
delta = floor((100-overlap)*nfft/100);
ng = floor((n-nfft)/delta)
ug = [];
gm = [];

for i=1:ng
    indx1 = 1+(i-1)*delta
    indx2 = indx1 + nfft -1
    u = range/t(indx1+nfft/2);
    [g,f] = pwelch(sig(indx1:indx2),[],[],[],fs,'onesided');
    gm = [gm ; g'];
    ug = [ug ; u']; 
end
