%[]=scarab_snr_line(fname,cchan,bbands,ttimes,snr_cut,norm,num,mmin,mmax)
%
% function to make line plots from scarab_snr
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
% ttimes        desired times to plot
% snr_cut	show time averaged reverb only for snr above this
% norm		1 to normalize reverb plots by source "power"
% num		number of plots on a page
% mmin		minimum value to plot
% mmax		maximum value to plot
%
% OUTPUTS:
%
% 		generates plots for desired bands, chans

function[]=scarab_snr_line(fname,cchan,bbands,ttimes,snr_cut,norm,num,mmin,mmax)

if fname(length(fname)-7:length(fname))~='_snr.mat'

fname=[fname '_snr.mat'];

end

eval(['load ' fname]) 

if nargin<9

mmax=130;

end

if nargin<8

mmin=70;

end

if nargin<7

num=1;

end

if nargin<6

norm=1;

end

if norm==1&nargin<8

mmax=mmax-230;

mmin=mmin-230;

end

% first plot ambient noise over all bands for desired channels

figure(1)

a=plot(chan(cchan),dbp(amb(cchan,bbands)));

set(a,'LineWidth',2)

axis([min(chan(cchan)) max(chan(cchan)) 60 120])

grid

ylabel('dB re uPa')

xlabel('Channels')

eval(['title(''Ambient noise power in bands before shot ' fname(1:length(fname)-9) ''')'])

leg_string=[];

for ll=1:length(bbands)

leg_string=[leg_string '''' int2str(bands(bbands(ll))) ''','];

end

leg_string=leg_string(1:length(leg_string)-1);

%a=legend(leg_string)

%axes(a)

%refresh

eval(['print -depsc ' fname(1:length(fname)-9) '_amb_line'])

for j=1:length(ttimes)

figure(1+ceil(j/num))

[val,jj]=min(abs(ttimes(j)-times));

aa=dbp(snr(cchan,(bbands-1)*length(times)+jj));

aa(find(aa<snr_cut))=NaN*ones(size(find(aa<snr_cut)));

% no source normalization

if norm~=1

subplot(ceil(num/2),min(2,num),j-floor((j-1)/num)*num)

a=semilogx(bands(bbands),aa+dbp(amb(cchan,bbands)));

set(a,'LineWidth',2)

grid

%a=legend(leg_string)

%axes(a)

%refresh

axis([100 10000 mmin mmax])

xlabel('Third octave center frequency (Hz)')

ylabel('dB re uPa')

else

% dividing by source "power"

subplot(ceil(num/2),min(2,num),j-floor((j-1)/num)*num)

a=semilogx(bands(bbands),aa+dbp(amb(cchan,bbands)./(ones(length(cchan),1)*source(bbands))));

set(a,'LineWidth',2)

grid

%a=legend(leg_string)

%axes(a)

%refresh

axis([100 10000 mmin mmax])

xlabel('Third octave center frequency (Hz)')

ylabel('dB re source power (1 m)')

end

if norm~=1

% no source normalization

if num==1

eval(['title(''Reverb power exceeding ' int2str(snr_cut) ' dB SNR at ' num2str(times(jj)) ' seconds ' fname(1:length(fname)-9) ''')'])

eval(['print -depsc ' fname(1:length(fname)-9) '_reverb_' int2str(times(jj)) '_sec'])

else

eval(['title(''' num2str(times(jj)) ' sec ' fname(1:length(fname)-9) ''')'])

if (j==floor(j/num)*num)|(j==length(bbands))

orient tall

eval(['print -depsc ' fname(1:length(fname)-9) '_reverb_multi_times_' int2str(floor(j/num)+((j==length(bbands))~=(j==floor(j/num)*num)))])

orient portrait

end

end

elseif norm==1

% source normalization

if num==1

eval(['title(''Normalized reverb power exceeding ' int2str(snr_cut) ' dB SNR at ' num2str(times(jj)) ' seconds ' fname(1:length(fname)-9) ''')'])

eval(['print -depsc ' fname(1:length(fname)-9) '_norm_reverb_' int2str(times(jj)) '_sec'])

else

eval(['title(''' num2str(times(jj)) ' sec ' fname(1:length(fname)-9) ''')'])

if (j==floor(j/num)*num)|(j==length(bbands))

orient tall

eval(['print -depsc ' fname(1:length(fname)-9) '_norm_reverb_multi_times_' int2str(floor(j/num)+((j==length(bbands))~=(j==floor(j/num)*num)))])

orient portrait

end

end

end

end
