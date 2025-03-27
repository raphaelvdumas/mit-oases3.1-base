function [y,th,bp]=bmwid3db(w,thlookdeg,g)
% bw3db=BMWID3DB(w,thlookdeg,g)
% Returns the 3 dB down beamwidth in degrees for a beam pointed
% in the direction thlookdeg (broadside=0 degrees) for a
% uniformly spaced array with shading coefficients in the vector
% w and the scaling factor g which may be chosen according to
%
%    g = f/fd  where f is the desired frequency
%              and fd the design freq
%
%    g = (2*d)/lam  where d is the element spacing
%                   and lam is the wavelength
%
% Note:  Optional second and third output arguments
%        return the beampattern in dB
%
%           [bw3db,thdeg,bpdb]=bmwid3db(w,thlookdeg,g)
%

n=length(w);
dlook=exp(j*pi*(0:n-1)'*sin(thlookdeg*pi/180)*g);
w=dlook.*w(:)/sum(w);
nfft=max(2048,2^(4+nextpow2(length(w))));
bp=fftshift(abs(fft(w,nfft)));
inz=find(bp>0);
bp=20*log10(bp(inz)/max(bp));
th=real(asin(2*(-(nfft/2):(nfft/2-1))'/(nfft*g))*180/pi);
th=th(inz);
ind=find(bp>=-3);
th0=th(min(ind));
th1=th(max(ind));
y=th1-th0;
