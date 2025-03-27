%[freq_beam,mov]=abel_nearfield(fname,chan,ping,xs,x,z,type,co,dec,shading,freqs)
%
% Kevin D. LePage
% 5/21/98
% SACLANTCEN
%
% INPUTS
%
% fname		filename		
% chan		chan vector (assumed to overlap number of cards if
%               larger than noCard)
% ping          ping vector
% xs		element locations (m)
% x		axial look locations (m)
% z		radial look locations (m)
% type          'time' for time movie, 'freq' for freq movie, 'none' for none
% co		sound speed (m/s) (default 1500 m/s)
% dec		decimation of movies
% shading	spatial shading (default hanning(length(z)))
% freqs         frequencies at which beamforming is desired (default all)
%
% OUTPUTS
%
% freq_beam	frequency-beam output of beamformer (plot using 20log10(abs))
% mov		output movie

function[freq_beam,mov]=abel_nearfield(fname,chan,ping,xs,x,z,type,co,dec,shading,freqs)

if nargin<7

type='none';

end

if nargin<8

co=1500;

end

if nargin<9

dec=10;

end

if nargin<10

shading=hanning(length(xs));

end

for j=1:length(ping)

%fprintf('j=%d out of %d\n',j,length(ping))

[data,fs]=abel_reader(fname,chan,ping(j));

data=detrend(data');

% condition data

temp=abs(data);

vec=mean(mean(temp));

for ll=1:size(data,2)

spike_ind=find(temp(:,ll)>100*vec);

spike_ind=ones(150,1)*spike_ind(:)'+[-75:74]'*ones(1,length(spike_ind));

data(spike_ind(:),ll)=zeros(length(spike_ind(:)),1);

end

clear temp;

figure(2)

atsplot(data,10);

drawnow

fprintf('Click at the start and finish of the desired segment for nearfield focusing: ')

[ind1,ind2]=ginput(2);

ind1=round(ind1);

fprintf('\nThank you\n')

data=detrend(data(ind1(1):ind1(1)+2^(nextpow2(ind1(2)-ind1(1)))-1,:));

data=fft(data);

if j==1

num_l=size(data,1);

num_b=length(x)*length(z);

% define frequency vector

if nargin<11

freq=[0:fs/num_l:fs/num_l*(num_l/2-1)];

iind=[1:num_l/2];

else

for k=1:length(freqs)

[val,ind]=min(abs(freqs(k)-[0:fs/num_l:fs/num_l*(num_l/2-1)]));

freq(k)=(ind-1)/num_l*fs;

iind(k)=ind;

end

end

freq_beam=zeros(num_b,length(freq)*length(ping));

% set up ranges

num_z=length(z);

num_s=length(xs);

x_old=x;

x=ones(num_z,1)*x(:)';

num_x=length(x_old);

z_old=z;

z=z(:)*ones(1,num_x);

r=((x(:)*ones(1,num_s)-ones(num_x*num_z,1)*xs(:)').^2+(z(:)*ones(1,num_s)).^2).^(.5);

r=r-r(:,size(r,2)/2)*ones(1,size(r,2));

% end of if loop about first ping

end

% make matched filter

t=[-length(freq)/2:length(freq)/2-1]/fs;

sigma=1/(sqrt(2)*pi*8000);

filter=(t.^2/sigma^2-1).*exp(-t.^2/2/sigma^2);

filter=conj(fft(fftshift(filter)));

%filter=ones(size(filter));

tmp=zeros(length(freq)*2,size(data,2));

for k=1:length(freq)

fprintf('\rj=%d out of %d, freq=%d out of %d',j,length(ping),k,length(freq))

tmp(k,:)=filter(k)*data(iind(k),:);

if k==length(freq)

tmp=ifft(tmp);

atsplot(real(tmp),10)

drawnow

end

% build steering vectors

steer=filter(k)*(ones(num_b,1)*shading(:)').*exp(i*freq(k)*2*pi/co*r);

freq_beam(:,(j-1)*length(freq)+k)=steer*data(iind(k),:).';

end

clear data

% end of loop over pings

end

if type~='n'

figure(3)

if type(1)~='f'

freq_beam=[freq_beam.';zeros(2^nextpow2(size(freq_beam,2)*2)-size(freq_beam,2),size(freq_beam,1))];

freq_beam=abs(ifft(freq_beam));

maxx=max(max(dba(freq_beam)));

wavei(dba(reshape(freq_beam(1,:),num_z,num_x)),x_old,z_old,maxx-30,maxx)

mov=moviein(size(freq_beam,1)/dec);

for j=1:size(freq_beam,1)/dec*(.5+.5*(type(1)~='f'));

wavei(dba(reshape(freq_beam((j-1)*dec+1,:),num_z,num_x)),x_old,z_old,maxx-30,maxx)

drawnow;

mov(:,j)=getframe;

end

else

freq_beam=freq_beam.';

maxx=max(max(dba(freq_beam)));

wavei(dba(reshape(freq_beam(1,:),num_z,num_x)),x_old,z_old,maxx-30,maxx)

mov=moviein(size(freq_beam,1)/dec);

for j=1:size(freq_beam,1)/dec*(.5+.5*(type(1)~='f'));

wavei(dba(reshape(freq_beam((j-1)*dec+1,:),num_z,num_x)),x_old,z_old,maxx-30,maxx)

drawnow;

mov(:,j)=getframe;

end

end

end






































