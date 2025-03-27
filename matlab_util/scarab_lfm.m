%[amb,snr]=scarab_lfm(filename,chan,win,stat_flag,jump)
%
% Kevin D. LePage
% 17/4/98
% SACLANTCEN
%
% function to compute synthetic lfm reverb products from scarab 98
% mf array data
%
% Inputs:
%
% filename	filename of binary file (string) or 'all' to do 
%		all files in current directory starting with a 'vla' 
%		or 'hla' and ending with a integer  
% chan		vector of desired channels
% win		window for estimating snr
% stat_flag	1 to compute K-S statistic
% jump		option argument for FFTin more than one chan at a time
%
% Outputs:
%
% amb		ambient noise power (num_chan x num_bands)
% snr		signal to noise ratio 
%		(num_chan x (num_windowsxnum_bands))
% bands		center frequencies (Hz)
% times		times for snr's

function[amb,snr,source]=scarab_snr(filename,chan,win,stat_flag,jump)

if nargin<5

jump=1;

else

win=win(:)*ones(1,jump);

end

if filename(1:3)=='all'

a=ls;

ind=find(((a==' ') | (a==setstr(10))));

ind=ind(find(diff(ind)~=1));

ind=[1 ind+1];

else

a=filename;

ind=1;

end

for j=1:length(ind)

if ((a(ind(j):ind(j)+2)=='vla')|(a(ind(j):ind(j)+2)=='lfa') ...
	|(a(ind(j):ind(j)+2)=='mfa'))

if ind==1

back_ind=length(a);

else

back_ind=ind(j)+min(find((a(ind(j):length(a))==' ')|(floor(a(ind(j):length(a)))==10)))-2;

end

fname=a(ind(j):back_ind);

if ((fname(back_ind-2-ind(j):back_ind-ind(j)+1)~='.eps')& ...
    (fname(back_ind-2-ind(j):back_ind-ind(j)+1)~='.mat')& ...
    (fname(back_ind-2-ind(j):back_ind-ind(j)+1)~='.eps'))

fprintf('%s\n',fname)

% acquire one channel only to pick desired length of sequence

fprintf('%s\n',['[aa,fs,gain,h_pass]=read_file_optg(' fname ',''to'',0,''tl'',60,''hs'',1,''F'',''G'',''HP'');'])

[aa,fs,gain,h_pass]=read_file_optg(fname,'to',0,'tl',60,'hs',1,'F','G','HP');

fprintf('fs=%d, gain=%d, h_pass=%d\n',fs,gain,h_pass) 

vecx(1)=100;

vecx(2)=max(200,min(find(abs(untrend(aa))>7*mean(abs(aa))))-5000);

vecx(3)=length(aa)-100;

% plot it

figure(1)

plot([0:length(aa)-1]/fs,dba(untrend(aa))-gain);

hold on

plot(vecx(1)/fs*[1 1],[-180 20],'r')

plot(vecx(2)/fs*[1 1],[-180 20],'g')

plot(vecx(3)/fs*[1 1],[-180 20],'b')

hold off

drawnow

% set acquisition limits

to=vecx(1)/fs;

tl=(vecx(3)-vecx(1)+1)/fs;

num_chan=length(chan);

mean_win=mean(win.^2);

mean_win=mean_win(1);

tocc=0.0;

for jj=1:num_chan/jump

fprintf('jj=%d out of %d, elapsed time = %f min\n',jj,num_chan/jump,tocc/60)

tic

% reacquire channels chan over acquisition limits only

fprintf('%s\n',['[ts,fs,gain,h_pass]=read_file_optg(' fname ',''to'',' num2str(to) ',''tl'',' num2str(tl) ',''hs'',chan((jj-1)*jump+1:(jj*jump),''G'',''G'',''HP'');'])

[ts,fs,gain,h_pass]=read_file_optg(fname,'to',to,'tl',tl,'hs',chan((jj-1)*jump+1:jj*jump),'F','G','HP');

% eliminate processor gain from ts

ts=ts*undb(-gain);

if jj==1

% make the time vector

time=[vecx(1):size(ts,1)+vecx(1)-1]/fs;

% pick nfft for padding sequence

nfft=max(8192,2^nextpow2(vecx(3)-vecx(1)+1));

% make frequency vector (it is double sided)

freq=[[0:fs/nfft:fs*(nfft/2)/nfft] [(nfft/2-1)/nfft*fs:-fs/nfft:fs/nfft]];

% determine the hydrophone sensitivities in linear units uPa/V

if (a(ind(j):ind(j)+2)=='vla')

sen=(undb(vsystsen(0,0,h_pass,freq)).^(-1))';

elseif (a(ind(j):ind(j)+2)=='lfa')

sen=(undb(tsystsen(0,0,h_pass,freq)).^(-1))';

elseif (a(ind(j):ind(j)+2)=='mfa')

sen=(undb(mfsystsen(0,0,h_pass,freq)).^(-1))';

else

fprintf('cannot determine hydrophone sensitivities for file beginning with %s, breaking\n',a(ind(j):ind(j)+2))

break

end

sen=sen(:)*ones(1,jump);

% determine the source time series

[ts_fft_source]=sus(0.817,1,91,fs,nfft,4);

% call the frequency response function

match;

% define the two operational bands

low(1)=450;
high(1)=700;
bands(1)=575;
sl(1)=230;
T(1)=1;

low(2)=2600;
high(2)=3400;
bands(2)=3000;
sl(2)=226;
T(2)=1;

% add the source level to the frequency response function

freq_lfr=[0;lf_resp(:,1);fs/2];
lf_resp=[0;undb(lf_resp(:,2)+sl(1)+3);0];

freq_mfr=[0;mf_resp(:,1);fs/2];
mf_resp=[0;undb(mf_resp(:,2)+sl(2)+3);0];

% generate the frequency representation of the tvds

ts_fft_tvds(:,1)=interp1(freq_lfr,lf_resp,freq);

ts_fft_tvds(:,2)=interp1(freq_mfr,mf_resp,freq);

% multiply by the frequency response of the lfm

ts_fft_tvds(:,1)=ts_fft_tvds(:,1).*fft(real([lfm(low(1),high(1),T(1),1/fs);zeros(nfft-T(1)*fs,1)]));

ts_fft_tvds(:,2)=ts_fft_tvds(:,2).*fft(real([lfm(low(2),high(2),T(2),1/fs);zeros(nfft-T(2)*fs,1)]));

end

% transform the data and apply the hydrophone sensitivity

ts_fft=sen.*fft([untrend(ts);zeros(nfft-length(ts),jump)]);

% formality of determining the number of bands (two)

num_bands=length(bands);

% loop over the number of bands

for kk=1:num_bands

ind_low=min(find((freq-low(kk))>0));

ind_high=max(find((freq(1:nfft/2)-high(kk))<0));

ts=2*real(ifft([zeros(ind_low-1,jump);ts_fft(ind_low:ind_high,:);zeros(nfft-ind_high,jump)]));

if jj==1

% also band pass the source signature

ts_source=fftshift(2*real(ifft([zeros(ind_low-1,1);ts_fft_source(ind_low:ind_high).';zeros(nfft-ind_high,1)])));

end

% estimate the ambient noise in band

amb((jj-1)*jump+1:jj*jump,kk)=mean(ts(vecx(1):vecx(2),:).^2)';

% estimate the signal to noise ratio over the signal portion

len_win=length(win);

num_win=floor((vecx(3)-vecx(1))/len_win)*2;

times=to+(vecx(1)+[0:num_win-1]*len_win/2+len_win/2)/fs;

% loop over the temporal averaging windows

for tt=1:num_win

fprintf('\rjj=%d out of %d, kk=%d out of %d, tt=%d out of %d',jj,num_chan/jump,kk,num_bands,tt,num_win)

snr((jj-1)*jump+1:jj*jump,(kk-1)*num_win+tt)=mean((ts(vecx(1)+(tt-1)*len_win/2:vecx(1)+(tt-1)*len_win/2+len_win-1,:).*win).^2)'./amb((jj-1)*jump+1:jj*jump,kk)/mean_win;

if ((jj==1)&(tt==1))

% also estimate the "power" of the source signature

source(kk)=mean(ts_source(nfft/2-len_win/2:nfft/2+len_win/2-1).^2);

end

% end of window loop

end

% if desired compute the "demodulated" matched filtered result

if stat_flag==1

if ((jj==1)&(kk==1))

path(path,'/usr1/users/lepage/SUS')

end

% define the spectral content of the matched filter

spec=(ts_fft(ind_low:ind_high,:).*(undb(-sl(kk)-3)*abs(ts_fft_tvds(ind_low:ind_high,kk)).^2./ts_fft_source(ind_low:ind_high).')*ones(1,jump));

num_bins=size(spec,1);

% flip the spectrum so that the center frequency is at zero Hz

spec=[spec(ceil(num_bins/2):num_bins,:);zeros(nfft-length(ceil(num_bins/2):num_bins)-length(1:floor(num_bins/2)),jump);spec(1:floor(num_bins/2),:)];

% determine complex envelope of demodulated matched filter result

ts_mf=ifft(spec);

% convolve with a low pass filter

hpf=kevfir1(100,(high(kk)-low(kk))/fs);

ts_mf=conv(hpf,ts_mf);

% decimate by next power of 2 > fs/bandwidth

dec=2^(nextpow2(floor(fs/(high(kk)-low(kk))))-1);

nblock(kk)=min(500,25*512/dec);

ts_mf=untrend(ts_mf(vecx(1):dec:vecx(3),:));

figure(2)

subplot(3,1,1)

plot([vecx(1):dec:vecx(3)]/fs,real(ts_mf),'.')

title(['channel ' int2str((jj-1)*jump+1:jj*jump) ', band ' int2str(bands(kk)) ' nblock=' int2str(nblock(kk)) ' ' fname]) 

drawnow

% send to normalizer

%[Y,V]=osnorm(ts_mf,100,50,150,5);

[Y,V]=osnorm(ts_mf,min(100,nblock(kk)),min(50,floor(nblock(kk)/2)),min(150,ceil(nblock(kk)*3/2)),5);

Y=abs(Y);

subplot(3,1,2)

plot([vecx(1):dec:vecx(3)]/fs,Y)

drawnow

noff=length(win)/2/dec;

temp=ksstat(Y,nblock(kk),noff)';

kolsmir((jj-1)*jump+1:jj*jump,(kk-1)*num_win+1+(num_win-length(temp)):(kk-1)*num_win+num_win)=temp;

kolsmir((jj-1)*jump+1:jj*jump,(kk-1)*num_win+1:(kk-1)*num_win+(num_win-length(temp)))=NaN*ones(jump,num_win-length(temp));

subplot(3,1,3)

plot(times,log10(kolsmir((jj-1)*jump+1:jj*jump,(kk-1)*num_win+1:(kk-1)*num_win+num_win)))

axis([0 10 -10 0])

drawnow

% end of statistics if loop

end

% end of band loop

end

tocc=tocc+toc;

% end of chan loop

end

% save results

eval(['save ' fname '_lfm snr amb times chan bands win mean_win source fs h_pass gain kolsmir nblock'])

clear snr times amb

% end of if loop about the .eps files

end

% end of if loop about filename string starting with vla or hla

end

% end of filename loop

end

