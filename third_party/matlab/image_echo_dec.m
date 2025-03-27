%[image_out]=image_echo_dec(ts,replica,fs,xs,co,x,z,fo,f_lim,thresh)
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
% fo		fir filter center frequency (Hz)
% f_lim		lower and upper limits of band (Hz)
% thresh	threshold on the matched filter detector (optional)
%
% OUTPUTS
%
% image_out	focused time series at times t (length(t) x (length(x)xlength(z)))

function[image_out]=image_echo_dec(ts,replica,fs,xs,co,x,z,fo,f_lim,thresh)

replica=[replica(:);zeros(size(ts,1)-length(replica),1)];

if nargin<9

f_lim=[7000 9000];

end

if nargin<8

fo=8000;

end

% determine num_chan

num_chan=size(ts,2);

% base band data

nfft=2^nextpow2(size(ts,1));

ts=fft(ts,nfft);

for j=1:num_chan

temp=fftshift(ts(floor(nfft*f_lim(1)/fs):ceil(nfft*f_lim(2)/fs),j));

nfft_dec=2^(nextpow2(length(temp)));

ts_new(:,j)=[temp(1:ceil(length(temp)/2));zeros(nfft_dec-ceil(length(temp)/2)*2+1,1);temp(ceil(length(temp)/2)+1:length(temp))];

end

ts=ifft(ts_new);

clear ts_new;

% baseband replica

replica=fft(replica,nfft);

temp=fftshift(replica(floor(nfft*f_lim(1)/fs):ceil(nfft*f_lim(2)/fs)));

nfft_dec=2^(nextpow2(length(temp)));

replica_new=[temp(1:ceil(length(temp)/2));zeros(nfft_dec-ceil(length(temp)/2)*2+1,1);temp(ceil(length(temp)/2)+1:length(temp))];

replica=ifft(replica_new);

clear replica_new

% redefine fs

fs=nfft_dec/nfft;

if nargin<10

thresh=.8*max(ifft(fft(replica).*conj(fft(replica))));

else

thresh=thresh*max(ifft(fft(replica).*conj(fft(replica))));

end

% match filter vector time series with replica to find all start times

ts_mf=abs(hilbert(ifft(fft(ts).*(conj(fft(replica))*ones(1,num_chan)))));

keyboard

% determine the start times

for j=1:num_chan

iind=[sort(find(-ts_mf(:,j)<-thresh));size(ts_mf,1)];

ind_ind=find(diff(iind)>10)

ind(:,j)=iind(ind_ind)

end

clear ts_mf

nfft
nfft_dec

% put xs into row array

xs=xs(:)';

% fit indicies to a zero lag index for source location

for j=1:size(ind,1)

a=mean((ind(j,:)-mean(ind(j,:)))./(xs-mean(xs)));

b=mean(ind(j,:)-a*xs);

loc(j)=round(b);

end

% first determine distances to all hypothetical scatterer locations

oldx=x;

oldz=z;

x=ones(length(z),1)*x(:)';

x=x(:)*ones(1,length(xs));

z=z(:)*ones(1,length(oldx));

z=z(:)*ones(1,length(xs));

r=((x-ones(size(x,1),1)*xs).^(2)+z.^(2)).^(.5);

ro=(x.^(2)+z.^(2)).^(.5);

nfft=2^(nextpow2(length(replica)));

replica=conj(fft(replica,nfft));

ts=fft(ts,nfft).*(replica*ones(1,num_chan));

% determine relevant vector of frequencies

w=2*pi*[[0:fs/nfft:fs/2] [-fs*(nfft/2-1)/nfft:fs/nfft:-fs/nfft]]';

% make the steering vectors

image_out=zeros(num_chan,length(r));

for j=1:length(r)

fprintf('\rj=%d out of %d',j,length(r))

% circular convolution of matched filter for 
% return from hypothesized location

temp=w/co*(ro(j,:)+r(j,:));

temp=exp(i*temp);

temp=ts.*temp;

temp=sum(ifft([temp(1:nfft/2,:);zeros(nfft/2,length(loc))]).').';

image_out(:,j)=abs(temp(loc));

%plot(image_out(:,j))

%axis([1 length(loc) 0 thresh])

%drawnow

fprintf('\na=%d, b=%d\n',floor(j/length(oldx)),j/length(oldx))

%if floor(j/length(oldx))==j/length(oldx)

%for j=1:num_chan

%wavei(dba(reshape(image_out(j,:),length(oldz),length(oldx))),oldx,oldz,-20,20,9,j)

%drawnow

%end

%end

end





