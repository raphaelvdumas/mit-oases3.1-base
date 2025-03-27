%[image_out]=image(ts,replica,fs,xs,co,x,z,thresh)
%
% function to image backscattered time series back onto hypothetical 
% scatterer locations using a source replica-deals with multiple pings
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
% thresh	threshold on the matched filter detector (optional)
%
% OUTPUTS
%
% image_out	focused time series at times t (length(t) x (length(x)xlength(z)))

function[image_out]=image(ts,replica,fs,xs,co,x,z,thresh)

replica=replica(:);

if nargin<8

thresh=.8*max(conv(replica,flipud(replica)));

end

% match filter vector time series with replica to find all start times

ts_mf=abs(hilbert(conv(ts,replica)));

% determine the start times

for j=1:size(ts,2)

ind=[sort(find(-ts_mf(:,j)<-thresh));size(ts_mf,1)];

ind_ind=find(diff(ind)>10);

ind(:,j)=ind(ind_ind)-100;

end

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

% fft the replica vector

replica=fft([replica;zeros(1024-length(replica),1)]);

% determine relevant vector of frequencies

w=2*pi*[[0:fs/1024:fs/2] [-fs*(1024/2-1)/1024:fs/1024:-fs/1024]]';

% make the steering vectors

for j=1:length(r)

fprintf('\rj=%d out of %d',j,length(r))

% make vector matched filter

temp=flipud(ifft(exp(-i*w/co*2*r(j,:)).*(replica*ones(size(ts,2)))));

% loop over pings

for pp=1:length(ind)

% convolve and add

for l=1:size(ts,2)

out(:,l)=conv(ts(ind(pp,l):ind(pp,l)+1023,l),temp)

end

image_out(pp,j)=max(abs(hilbert(sum(out.'))));

end

end





