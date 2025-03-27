%[]=scarab_spec(filename,fs,chan)
%
%
% Kevin D. LePage
% 15/4/98
% SACLANTCEN
%
% function to compute the spectrums of vector time series interactively
%
% Inputs:
%
% filename	filename of binary file (string) or 'all' to do all files
%		starting with a 'vla' or 'hla' in the current directory  
% fs		sample frequency (Hz)
% chan		vector of desired channels
%
% Outputs:
%
% 		makes a postscript file filename.eps  

function[]=scarab_spec(filename,fs,chan)

if nargin<3

chan=1:96;

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

if (fname(back_ind-2-ind(j):back_ind-ind(j)+1)~='.eps')

fprintf('%s\n',fname)

% acquire one channel only to pick desired length of sequence

fprintf('%s\n',['[aa]=read_file_optg(' fname ',''to'',0,''tl'',60,''hs'',1);'])

[aa]=read_file_optg(fname,'to',0,'tl',60,'hs',1);

% plot it

figure(1)

plot(dba(untrend(aa)));

% ask for user input on desired range of FFT

fprintf('Please click on the beginning and end of the sequence:')

[vecx,vecy]=ginput(2);

fprintf('\n')

% round input to integers

vecx(1)=floor(vecx(1));

vecx(2)=ceil(vecx(2));

% set acquisition limits

to=vecx(1)/fs;

tl=(vecx(2)-vecx(1)+1)/fs;

% reacquire channels chan over acquisition limits only

fprintf('%s\n',['[ts]=read_file_optg(' fname ',''to'',' num2str(to) ',''tl'',' num2str(tl) ',''hs'',chan);'])

[ts]=read_file_optg(fname,'to',to,'tl',tl,'hs',chan);

% make the time vector

time=[vecx(1):size(ts,1)+vecx(1)-1]/fs;

% pick nfft for padding sequence

nfft=max(8192,2^nextpow2(vecx(2)-vecx(1)+1));

% do the transforms

% predefine the size of the result

ts_fft=ones(nfft,length(chan));%+i*ones(nfft,length(chan));

for jj=1:length(chan)

fprintf('\rFFT loop %d out of %d',jj,length(chan))

%ts_fft=fft([untrend(ts); zeros(nfft-vecx(2)+vecx(1)-1,length(chan))]);

ts_fft(:,jj)=fft([untrend(ts(:,jj)); zeros(nfft-length(ts(:,j)),1)]);

end

eval(['save ' fname ' ts ts_fft to tl fs chan time'])

clear aa ts

for jj=1:length(chan)

fprintf('\r dB loop %d out of %d',jj,length(chan))

% convert to dB

ts_fft(:,jj)=dba(ts_fft(:,jj))+3;

end

% find the max

maxx=ceil(max(max(ts_fft))/10)*10;

% plot zero to Nyquist

% frequency axis

if max(diff(chan))==min(diff(chan))

% this lets wavei call image

freq_ax=[0 nfft/2]*fs/nfft;

else

% this makes wavei call pcolor

freq_ax=[0:nfft/2]*fs/nfft;

end

figure(1)

wavei(ts_fft(1:nfft/2+1,:)',freq_ax,chan,maxx-60,maxx)

xlabel('Frequency (Hz)')

ylabel('Channel number')

title(fname)

fprintf('\n%s\n',['print -depsc ' fname ])

eval(['print -depsc ' fname '_spec'])

fprintf('click on desired channel for spectragram:')

[vecx,vecy]=ginput(1);

[vecy_diff,vecy_ind]=min(abs(vecy-chan));

vecy=find(chan==chan(vecy_ind));

fprintf('\nChannel %d selected\n',chan(vecy))

figure(2)

eval(['load ' fname])

clear ts_fft

[spec_gram,freq,stime]=gspec(untrend(ts(:,vecy)),512,fs,[hamming(128);zeros(256+128,1)],512-16);

spec_gram=dba(spec_gram);

mmax=max(max(spec_gram));

subplot(211)

wavei(spec_gram,to+[stime(1) stime(length(stime))]+64/fs,freq,mmax-70,mmax)

xlabel('Time (s)')

ylabel('Freq (Hz)')

title([fname ' Channel No ' num2str(chan(vecy))])

subplot(212)

plot(time,untrend(ts(:,vecy)),'b')

axis([to+stime(1)+64/fs to+stime(length(stime))+64/fs -2^nextpow2(max(abs(untrend(ts(:,vecy))))/10)*10 2^nextpow2(max(abs(untrend(ts(:,vecy))))/10)*10])

xlabel('Time (s)')

ylabel('Amplitude (V)')

ax=colorbar;

axes(ax)

cla

set(ax,'Visible','off')

drawnow

eval(['print -depsc ' fname '_specgram_ch_' num2str(chan(vecy))])

clear ts ts_fft spec_gram

end

end

end





