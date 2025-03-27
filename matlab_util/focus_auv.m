%[out]=focus_auv(X,Y,Z,x,y,z,head,t_start,num,delay,zs,Z_rec,xdata,fo,band,t)
%
% function to do simple Bartlett beamforming
% 
% Kevin D. LePage
% 8/7/98
% SACLANTCEN
%
% INPUTS
%
% X		hypothesized X locations (vector) 
% Y		hypothesized Y locations (vector)
% Z		hypothesized bottom depths (scalar)
% x		x locations of aperture
% y		y locations of aperture (length(x))
% z		z depths of aperture (length(x))
% head		heading (degrees from north) (length(x))
% t_start	time after acquisition start of data (to correct x,y,z)
% num		number of pings per acquisition collected
% delay		inter-ping time (sec)
% zs		topas source depth (m)
% Z_rec 	locations of swordfish receivers
% xdata 	data on all receivers
% fo		center frequency of ricker
% band          frequency limits vector
% t		time series
%
% OUTPUTS
%
% out	image

function[out]=focus_auv(X,Y,Z,x,y,z,head,t_start,num,delay,Zs,Z_rec,xdata,fo,band,t)

% make waveform

len=length(t);

t=[t(:);t(len)+(t(2)-t(1))*[1:len]'];

fs=1/(t(2)-t(1));

sigma_sq=(1/(sqrt(2)*pi*fo))^2;

wave=((t-.01).^2/sigma_sq-1).*exp(-(t-.01).^2/2/sigma_sq);

wave=[wave(floor(.01*fs):length(wave));wave(1:floor(.01*fs)-1)];

% frequencies

freq=[[0:floor(length(t)/2)] [-(floor(length(t)/2)-1):-1]]'*fs/length(t);

% truncate if necessary

wave=wave(1:length(freq));

mask_ind_low=find(abs(freq)<band(1));

mask_ind_middle_low=find((freq>=band(1))&(freq<=band(2)));

mask_ind_middle_high=find((freq<=-band(1))&(freq>=-band(2)));

mask_ind_high=find(abs(freq)>band(2));

mask=ones(length(wave),1);

mask(mask_ind_low)=zeros(length(mask_ind_low),1);

mask(mask_ind_high)=zeros(length(mask_ind_high),1);

mask(mask_ind_middle_low)=hanning(length(mask_ind_middle_low));

mask(mask_ind_middle_high)=hanning(length(mask_ind_middle_high));

% filter data 

for hh=1:size(xdata,1)

xdata(hh,:)=real(ifft(mask(1:2:length(wave))'.*fft(xdata(hh,:))));

end

wave=real(ifft(mask.*fft(wave)));

dec=nextpow2(fs/band(2))/2;

wave=wave(1:dec:length(wave));

xdata=xdata(:,1:dec:size(xdata,2));

t=t(1:dec:length(t));

% make data an envelope function

xdata=abs(hilbert(xdata'))';

%figure(2)

%atsplot(xdata',20)

%drawnow

co=1520;

head=head*pi/180;

dummy1=ones(length(x),1);

dummy2=ones(1,length(Z_rec));

% get distance between navigation

dtime=[0:delay:(num-1)*delay]';

for jjj=1:length(x)/num-1

range(:,jjj)=(t_start((jjj-1)*num+1)+dtime)/10*sqrt((x((jjj-1)*num+1)-x(jjj*num+1))^2+(y((jjj-1)*num+1)-y(jjj*num+1))^2);

head_new(:,jjj)=ones(num,1)*atan2(x(jjj*num+1)-x((jjj-1)*num+1),y(jjj*num+1)-y((jjj-1)*num+1));

if jjj==length(x)/num-1

range(:,jjj+1)=(t_start(jjj*num+1)+dtime)/t_start((jjj-1)*num+1)*range((jjj-1)*num+1);

head_new(:,jjj+1)=head(jjj*num+1:(jjj+1)*num);

end

end

head_new=head_new(:);

[head_new head];

head=head_new;

range=range(:);

% get delta on x and y due to t_start

delta_x=sin(head).*range;

delta_y=cos(head).*range;

out=zeros(length(X)*length(Y),size(xdata,1));

for k=1:length(Y)

for j=1:length(X)

fprintf('\rj=%d, k=%d',j,k)

% make the receiver vectors

tempx=(x(:)*dummy2+(dummy1.*sin(head))*Z_rec(:)'+delta_x*dummy2)'; 

tempy=(y(:)*dummy2+(dummy1.*cos(head))*Z_rec(:)'+delta_y*dummy2)';

tempz=(z(:)*dummy2)';

temphead=(head_new(:)*dummy2)';

vec_receiver=[tempx(:) tempy(:)];

vec_receiver_3D=[tempx(:) tempy(:) tempz(:) temphead(:)*180/pi];

save vec_receiver vec_receiver

save vec_receiver_3D vec_receiver_3D

clear tempx tempy

[ts]=ray_auv(Z,Zs,[X(j) Y(k)],vec_receiver_3D,co,wave,t);

ts=ts(1:len,:);

ts=abs(hilbert(ts));

%figure(3)

%atsplot(ts,20,t(1:size(ts,1)));

%drawnow

for lll=1:size(xdata,1)

if lll==1

out((j-1)*length(Y)+k,lll)=xdata(lll,:)*ts(1:size(xdata,2),lll);

else

out((j-1)*length(Y)+k,lll)=out((j-1)*length(Y)+k,lll-1)+xdata(lll,:)*ts(1:size(xdata,2),lll);

end

%out(j,k)=sum(diag(xdata*ts(1:size(xdata,2),:)));

end

end;

end;

out=out./(ones(length(X)*length(Y),1)*max(out));

