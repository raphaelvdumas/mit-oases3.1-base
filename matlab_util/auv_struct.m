%[auv_chans,auv_gains,auv_filt,auv_sampl_rate,auv_data,auv_t_axis]=auv_struct(fname, chans_out, t_lim)
%
% Function to read auv data files
%
% Reg Hollett
% SACLANTCEN
%
% INPUTS
%
% fname		 input file name
% chans_out	 desired channels
% t_lim		 beginning and end time desired
%
% OUPUTS
%
% auv_chans	 channel axis
% auv_gains	 channel gains
% auv_filt	 filter cutoff
% auv_sampl_rate sample rate
% auv_data	x data matrix 
% auv_t_axis	 time axis

function[auv_chans,auv_gains,auv_filt,auv_sampl_rate,auv_data,auv_t_axis]= auv_struct(fname, chans_out, t_lim)

[fid, message] = fopen(fname);
if fid == -1
   fprintf(2, '%s', message)
end
if nargin < 2
   chans_out = [];
end
if nargin < 3
   t_lim = [];
end

h = fread(fid, [1, 32768]);
h = h(1:4:length(h));
i = find(h == 0);
h = h(1:i(1)-1);
h = setstr(h);
s = sscanf(h(findstr(h, 'CHANS '):findstr(h, 'GAINS ')-1), '%s');
chans = sscanf(s(length('CHANS '):length(s)), '%d,')';
if isempty(chans_out)
   chans_out = chans;
end
auv_chans = chans_out;
s = sscanf(h(findstr(h, 'GAINS '):findstr(h, 'FILT ')-1), '%s');
gains = sscanf(s(length('GAINS '):length(s)), '$G%1d%1d');
gains = 20*gains;
for i = 1:length(chans_out)
   if auv_chans(i) < 9
      auv_gains(i) = sum(gains);
   else
      auv_gains(i) = gains(1);
   end
end
s = sscanf(h(findstr(h, 'FILT '):findstr(h, 'SAMPL_RATE ')-1), '%s');
auv_filt = sscanf(s(length('FILT '):length(s)), '$%s');
s = sscanf(h(findstr(h, 'SAMPL_RATE '):findstr(h, 'BUFSIZE ')-1), '%s');
sampl_rate = sscanf(s(length('SAMPL_RATE '):length(s)), '%d');
if sampl_rate == 17
   auv_sampl_rate = 100e3;
elseif sampl_rate == 18
   auv_sampl_rate = 50e3;
end
s = sscanf(h(findstr(h, 'BUFSIZE '):findstr(h, 'NUMBUFS ')-1), '%s');
bufsize = sscanf(s(length('BUFSIZE '):length(s)), '%d');
s = sscanf(h(findstr(h, 'NUMBUFS '):findstr(h, 'TIME ')-1), '%s');
numbufs = sscanf(s(length('NUMBUFS '):length(s)), '%d');
if isempty(t_lim)
   lim = [1, 2*numbufs*bufsize/length(chans)];
else
   lim = auv_sampl_rate*t_lim;
   lim(1) = lim(1)+1;
end

fseek(fid, 2*length(chans)*(lim(1)-1), 0);
l = lim(2)-lim(1)+1;
auv_data = zeros(length(chans_out), l);
for i = 1:auv_sampl_rate:l
   siz = [length(chans), min([l-i+1, auv_sampl_rate])];
   data = fread(fid, siz, 'short');
   auv_data(:, i:i+siz(2)-1) = data(chans_out, :);
end
auv_data = 3.75/32768*auv_data;
auv_t_axis = ((lim(1):lim(2))-1)/auv_sampl_rate;

fclose(fid);
