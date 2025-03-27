%[]=run_all(fname,chan,ping,z,beam,band,interval,duration,fc,num)
%
% Function to select and stack num segments of a swordfish array tseries
% and compute spectra of reverb time series normalized for input waveform
% over all segments of all files in ping vector
%
% Kevin D. LePage
% 25/5/98
% SACLANTCEN
%
% INPUTS
%
% fname		file name ascii string (prefix, without _000 to _119)
% chan		channel vector
% ping	 	ping vector (out of 000-119 usually)
% z		sensor locations (m)
% beam		beam angles (deg, 0 is broadside)
% band		vector of frequencies [f_low f_high] for band-passed 
%		beamformed time series
% interval      inter-shot interval of topas in sec
% duration	duration of stacked time series in msec
% fc		center frequency of ricker
% num		number of pings per file

function[]=run_all(ffname,chan,ping,z,beam,band,interval,duration,fc,num)

num_b=length(beam);

z=z-median(z);

for ll=1:length(ping)

j=ping(ll);

bla='000'

juk=int2str(j);

for k=1:length(juk)

bla(3-length(juk)+k:3)=juk(k);

end

fname=[ffname '_'  bla]

x=x_data(fname,chan(1),[]);

fs=x.sampl_rate;

filt=x_syst_sen(x.filt);

figure(1)

[spec,f,t]=specgram(x.data(1,:),2048,fs,hanning(2048),1024);

sens=interp1(filt.f_axis,dba(filt.syst_sen),f);

wavei(dba(spec)-x.gains(1)-sens(:)*ones(1,size(spec,2)),[t(1) t(length(t))]-1024/fs,[f(1) f(length(f))],110,170,1,1,'dB re uPa')

fprintf('Click on beginning of the first topas in the ping (will select %d more at %f second intervals):',num,interval)

[xlim(ll),ylim]=ginput(1);

fprintf('\nThank you\n')

% end of loop over pings

end

xdata=[];

for ll=1:length(ping)

j=ping(ll);

bla='000'

juk=int2str(j);

for k=1:length(juk)

bla(3-length(juk)+k:3)=juk(k);

end

fname=[ffname '_'  bla]

% reacquire data

for lll=1:num

x=x_data(fname,chan,[xlim(ll) xlim(ll)+duration/1000]+(lll-1)*interval);

xdata=[xdata;zeros(1,size(x.data,2));x.data];

end

% end of loop over pings

end

t_freq=[0:size(xdata,2)-1]/size(xdata,2)*fs;

t_freq=[t_freq(1:floor(length(t_freq)/2)) zeros(1,length(t_freq)-2*floor(length(t_freq)/2)+1) conj(fliplr(t_freq(2:floor(length(t_freq)/2))))];

sens=abs(interp1(filt.f_axis,filt.syst_sen,t_freq).^(-1));

% set parts of sens outside of pass band to zero

mask_low=find(t_freq(1:floor(length(t_freq)/2))<band(1));

mask_high=find(t_freq(1:floor(length(t_freq)/2))>band(2));

mask=boxcar(floor(length(t_freq)/2)-length(mask_high)-length(mask_low));

mask=[zeros(length(mask_low),1); mask; zeros(length(mask_high),1)];

mask=[mask;zeros(length(t_freq)-length(mask)*2+1,1);flipud(mask(2:length(mask)))];

xdata=real(ifft(((sens(:).*mask(:))*ones(1,size(xdata,1))).*(fft(xdata.'))).')/(10^(x.gains(1)/20));

atsplot(xdata',2*length(ping)*8/length(chan),[0:size(xdata,2)-1]/fs,ping(1)+([1:size(xdata,1)]-1)/size(xdata,1)*length(ping),'k');

fprintf('input beginning and end of desired window with mouse click:')

[xtlim,ylim]=ginput(2);

xtlim(1)=floor(xtlim(1)/0.001)*.001;

xtlim(2)=ceil(xtlim(2)/0.001)*.001;

fprintf('\nThank you\n')

axis([xtlim(1) xtlim(2) ping(1) ping(length(ping))+1])

eval(['title(''' x.date '+ ' num2str(xlim) ' sec (' int2str(band(1)) '-' int2str(band(2)) ' Hz)'')'])

xlabel('Time (sec)')

ylabel('Ping')

drawnow

fprintf('click on the data segments for each ping for manual alignment:')

[del,flame]=ginput(num*length(ping));

fprintf('\nThank you\n')

xdata=[];

for ll=1:length(ping)

j=ping(ll);

bla='000'

juk=int2str(j);

for k=1:length(juk)

bla(3-length(juk)+k:3)=juk(k);

end

fname=[ffname '_'  bla]

for lll=1:num

x=x_data(fname,chan,[xlim(ll) xlim(ll)+duration/1000]+(lll-1)*interval);

x=x_data(fname,chan,[xlim(ll) xlim(ll)+duration/1000]+(lll-1)*interval+(del((ll-1)*num+lll)-del(1)));

x_loc=[x_loc;x.east];

y_loc=[y_loc;x.north];

z_loc=[z_loc;x.depth];

head=[head;x.heading];

xdata=[xdata;zeros(1,size(x.data,2));x.data];

end

% end of ping loop

end

% make time series

sigma_sq=(1/(sqrt(2)*pi*fc))^2;

tee=[0:1:size(xdata,2)-1]/fs-100*(sigma_sq)^(.5);

source=(tee.^2/sigma_sq-1).*exp(-tee.^2/2/sigma_sq);

source=source(:);

xdata=real(ifft(((sens(:).*mask(:))*ones(1,size(xdata,1))).*(fft(xdata.'))).')/(10^(x.gains(1)/20));

source=real(ifft((mask(:)+1e-3*ones(size(mask))).*fft(source)));

atsplot(xdata',2*length(ping)*8/length(chan),[0:size(xdata,2)-1]/fs,ping(1)+([1:size(xdata,1)]-1)/size(xdata,1)*length(ping),'k');

axis([xtlim(1) xtlim(2) ping(1) ping(length(ping))+1])

fprintf('input two limits for part of time series which is reverb')

[xtlim,ylim]=ginput(2);

xtlim(1)=floor(xtlim(1)*fs);

xtlim(2)=ceil(xtlim(2)*fs);

eval(['title(''' x.date '+ ' num2str(xlim) ' sec (' int2str(band(1)) '-' int2str(band(2)) ' Hz)'')'])

xlabel('Time (sec)')

ylabel('Ping')

drawnow

orient tall

eval(['print -depsc x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_atsplot_' int2str(band(1)) '-' int2str(band(2)) '_' bla ]) 

eval(['!gzip x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_atsplot_' int2str(band(1)) '-' int2str(band(2)) '_' bla '.eps']) 

% compute beam time series and plot in a similar way

leng=2^(nextpow2(size(xdata(:,xtlim(1):xtlim(2)),2)));

source_spec=abs(fft(source(1+round(50*sigma_sq.^(.5)):leng+round(50*sigma_sq.^(.5)))*ones(1,size(xdata,1))));

fre=([0:leng-1]/leng*fs)'*ones(1,size(xdata,1));

indfre=find(fre<band(1)|fre>band(2));

source_spec(indfre)=NaN*ones(size(indfre));

t_spec=dba(fft([xdata(:,xtlim(1):xtlim(2))';zeros(leng-size(xdata(:,xtlim(1):xtlim(2)),2),size(xdata,1))])./source_spec);

maxx=max(max(t_spec(find(isinf(t_spec)==0))));

t_spec(find(isnan(t_spec)))=(maxx-30)*ones(size(find(isnan(t_spec))));

wavei(t_spec',[0 fs],[ping(1) ping(length(ping))+1],-30+maxx,maxx);

axis([0 fs/2 ping(1) ping(length(ping))+1])

eval(['title(''normalized reverb spectrum ' x.date '+ ' num2str(xlim) ' sec (' int2str(band(1)) '-' int2str(band(2)) ' Hz)'')'])

xlabel('Freq (Hz)')

ylabel('Ping')

grid

orient tall

eval(['print -depsc x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_reverb_spec_win_' int2str(band(1)) '-' int2str(band(2)) ]) 

eval(['!gzip x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_reverb_spec_win_' int2str(band(1)) '-' int2str(band(2)) '.eps']) 

clear t_spec source_spec spec 

for ll=1:length(ping)*num

t_beam(:,(ll-1)*(num_b+1)+2:ll*(num_b+1))=abs(hilbert(beamform(xdata((ll-1)*(length(chan)+1)+2:ll*(length(chan)+1),xtlim(1):xtlim(2))',z,fs,beam)));

end

tim=[xtlim(1):xtlim(2)]/fs;

t_beam=t_beam(1:length(tim),:);

for pp=1:size(t_beam,2)

t_beam(:,pp)=dba(t_beam(:,pp));

end

maxx=max(max(t_beam));

imagesc(tim,[ping(1) ping(length(ping))+1],t_beam',[-40+maxx maxx])

axis('xy')

colorbar

%wavei(t_beam',tim,[ping(1) ping(length(ping))+1],-40+maxx,maxx);

eval(['title(''beamformed time series ' x.date '+ ' num2str(xlim) ' sec (' int2str(band(1)) '-' int2str(band(2)) ' Hz)'')'])

xlabel('Freq (Hz)')

ylabel('Ping')

grid

orient tall

eval(['print -depsc x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_beam_time_' int2str(band(1)) '-' int2str(band(2)) ]) 

eval(['!gzip x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_beam_time_' int2str(band(1)) '-' int2str(band(2)) '.eps']) 

clear t_beam

for pp=1:num*length(ping)

data((pp-1)*length(chan)+1:pp*length(chan),:)=xdata((pp-1)*(length(chan)+1)+2:(pp)*(length(chan)+1),:);

end

x=rmfield(x,'data');

clear fre xdata indfre 

t_start=ones(num,1)*xlim;

t_start=t_start(:);

eval(['save x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_run_all_' int2str(band(1)) '-' int2str(band(2)) ]) 

eval(['!gzip x9814203_'  int2str(ping(1)) '_'  int2str(ping(length(ping))) '_run_all_' int2str(band(1)) '-' int2str(band(2)) '.mat']) 









