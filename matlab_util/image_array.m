% function[t_focus]=image_array(t,fs,za,x,z,C,Z)
%
% function to compute nearfield image from vector time series
%
% Kevin D. Lepage
% SACLANTCEN
% 11/28/97
%
% INPUTS
%
% t		time series (length time x number of receivers)
% fs		sample frequency (Hz)
% za		receiver depths (m)
% x		matrix of required range locations (m)
% z		matrix of required depth locations (m)
% C		ssp (m/s)
% Z		corresponding depths (m)
%
% OUTPUTS
%
% t_focus	focused image of reverb as a function of time

function[t_focus]=image(t,fs,za,x,z,C,Z)

tf=fft(t);

za=za(:)';

x=x(:);

z=z(:);

r=(x.^2*ones(1,length(za))+(z*ones(1,length(za))-ones(length(x),1)*za).^2).^(.5);

w=[0:2*pi/length(t)*fs:2*pi*(length(t)-1)*fs/length(t)]';

w(floor(length(w)/2):length(w))=w(floor(length(w)/2):length(w))-2*pi*fs;

t_focus=zeros(length(t),length(r));

for j=1:length(r)

fprintf('\rj=%d out of %d',j,length(r))

t_focus(:,j)=real(ifft(sum((exp(-i*w/1500*r(j,:)).*tf).'))).';

end

