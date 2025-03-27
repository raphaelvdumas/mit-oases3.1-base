%[ts_f,mov]=focus(ts,replica,fs,xs,co,x,z,t)
%
% function to focus backscattered time series back onto hypothetical 
% scatterer locations
%
% Kevin D. LePage
% SACLANTCEN
% 18/5/98
%
% INPUTS
% 
% ts		vector time series (num_t x num_n)
% replica	signal replica vector
% fs		sample frequency (Hz)
% xs		locations along linear array
% co		hypothetical sound speed (m/s)
% x 		hypothetical locations in range (m)
% z		hypothetical locations in depth (m)
% t		desired output itmes on x-z grid
%
% OUTPUTS
%
% ts_f	focused time series at times t (length(t) x (length(x)xlength(z)))
% mov	movie matrix

function[ts_f,mov]=focus(ts,fs,xs,co,x,z,t)

% put xs into row array

xs=xs(:)';

% first determine distances to all hypothetical scatterer locations

oldx=x;

oldz=z;

x=ones(length(z),1)*x(:)';

x=x(:)*ones(1,length(xs));

z=z(:)*ones(1,length(oldx));

z=z(:)*ones(1,length(xs));

r=((x-ones(size(x,1),1)*xs).^(2)+z.^(2)).^(.5);

% determine the number of times

num_t=length(ts);

% determine the desired indicies

for j=1:length(t)

[val,ind(j)]=min(abs(t(j)-[0:1/fs:(num_t*4-1)/fs]));

end

% fft the time series

ts=fft([zeros(size(ts,1)*3/2,size(ts,2));ts;zeros(size(ts,1)*3/2,size(ts,2))]);

% determine relevant vector of frequencies

w=2*pi*[[0:fs/num_t/4:fs/2] [-fs*(num_t/2-1/4)/num_t:fs/num_t/4:-fs/num_t/4]]';

% make the steering vectors

for j=1:length(r)

fprintf('\rj=%d out of %d',j,length(r))

temp=ifft(sum((exp(-i*w/co*r(j,:)).*ts).').');

%figure(5)

%plot(real(temp))

%axis([0 length(w) -2 2])

%drawnow

ts_f(:,j)=temp(ind);

end

figure(6)

mov=moviein(length(t));

for j=1:size(ts_f,1)

wavei(reshape(abs(hilbert(ts_f(j,:))),length(oldz),length(oldx)),oldx,oldz,0,3)

drawnow

mov(:,j)=getframe;

end




