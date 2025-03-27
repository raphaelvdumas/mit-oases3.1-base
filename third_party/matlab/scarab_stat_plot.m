%[]=scarab_stat_plot(fname,cchan,bbands,snr_cut,type,num,mmin,mmax)
%
% function to plot results from scarab_snr
%
% Kevin D. LePage
% SACLANTCEN
% 18/4/98
%
% INPUTS:
%
% fname		filename (with or wothout _snr.mat extension)
% cchan		desired channels to plot
% bbands	desired bands to plot
% snr_cut	show time averaged reverb only for snr above this
% type		'band' for plots of all channels at each band,
%		'chan' for plots of all bands for each channel 
% norm		1 to normalize reverb plots by source "power"
% num		number of plots on a page
% mmin		minimum value to plot
% mmax		maximum value to plot
%
% OUTPUTS:
%
% 		generates plots for desired bands, chans

function[]=scarab_stat_plot(fname,cchan,bbands,snr_cut,type,num,mmin,mmax)

if fname(length(fname)-7:length(fname))~='_snr.mat'

fname=[fname '_snr.mat'];

end

eval(['load ' fname]) 

if nargin<8

mmax=5;

end

if nargin<7

mmin=0;

end

if nargin<6

num=1;

end

if nargin<5

type='band';

end

if type=='band'

% if plots of all channels at individual frequencies are desired

for j=1:length(bbands)

figure(1+ceil(j/num))

jj=bbands(j);

a=dbp(snr(cchan,(jj-1)*length(times)+1:jj*length(times)));

b=-log10(kolsmir(cchan,(jj-1)*length(times)+1:jj*length(times)));

b(find(a<snr_cut))=NaN*ones(size(find(a<snr_cut)));

wavei(b,times,chan(cchan),mmin,mmax,num,j-floor((j-1)/num)*num,'log10(K-S)')

xlabel('Time (s)')

ylabel('Channels')

if num==1

eval(['title(''KS statistic for reverb power exceeding ' int2str(snr_cut) ' dB SNR in ' int2str(bands(jj)) ' Hz octave band ' fname(1:length(fname)-9) ''')'])

eval(['print -depsc ' fname(1:length(fname)-9) '_ks_' num2str(jj)])

else

eval(['title(''' int2str(bands(jj)) ' Hz ' fname(1:length(fname)-9) ''')'])

if (j==floor(j/num)*num)|(j==length(bbands))

orient tall

eval(['print -depsc ' fname(1:length(fname)-9) '_ks_multi_' int2str(floor(j/num)+((j==length(bbands))*(j~=floor(j/num)*num)))])

orient portrait

end

end

% end of j loop

end

elseif type=='chan'

% if plots of all frequencies for individual channels are desired

for j=1:length(cchan)

figure(1+ceil(j/num))

jj=cchan(j);

a=dbp(reshape(snr(jj,:),length(times),size(snr,2)/length(times))');

a=a(bbands,:);

b=-log10(reshape(kolsmir(jj,:),length(times),size(snr,2)/length(times))');

b=b(bbands,:);

b(find(a<snr_cut))=NaN*ones(size(find(a<snr_cut)));

wavei(b,times,bands(bbands)*2^(1/6),mmin,mmax,num,j-floor((j-1)/num)*num,'log10(K-S)')

xlabel('Time (s)')

ylabel('Band')

if num==1

eval(['title(''KS statistic for reverb power exceeding ' int2str(snr_cut) ' dB SNR in channel ' int2str(chan(jj)) ' ' fname(1:length(fname)-9) ''')'])

eval(['print -depsc ' fname(1:length(fname)-9) '_ks_vs_f_' num2str(jj)])

else

eval(['title(''channel ' int2str(chan(jj)) ' ' fname(1:length(fname)-9) ''')'])

if (j==floor(j/num)*num)|(j==length(chan))

orient tall

eval(['print -depsc ' fname(1:length(fname)-9) '_ks_vs_f_multi_' int2str(floor(j/num)+(j==length(chan))*(j~=floor(j/num)*num))])

orient portrait

end

end

% end of j loop

end

% end of if loop about type of plot

end

