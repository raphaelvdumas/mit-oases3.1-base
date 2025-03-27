%function x = x_data(fname, chans_out, t_lim, head_flag)
%
% Reg Hollett
%
% function to read auv data files
%
% INPUTS
%
% fname		file name string
% chans_out	desired channel vector (default channel 1)
% t_lim		time delimiters ([] for all (default))
% head_flag	1 if want to read header only (default 0)
%
% OUTPUTS
%
% x		data structure with all header info and data itself

function x = x_data(fname, chans_out, t_lim, head_flag)

if nargin<2

chans_out=1;

end

if nargin<3

t_lim=[];

end

if nargin<4

head_flag=0;

end

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
h = h(1:4:end);
i = find(h == 0);
h = h(1:i(1)-1);
h = char(h);
s = sscanf(h(findstr(h, 'CHANS '):end), '%s', 2);
chans = sscanf(s(length('CHANS '):end), '%d,')';
if isempty(chans_out)
   chans_out = chans;
end
x.chans = chans_out;
s = sscanf(h(findstr(h, 'GAINS '):end), '%s', 2);
gains = sscanf(s(length('GAINS '):end), '$G%1d%1d');
gains = 20*gains;
for i = 1:length(chans_out)
   if x.chans(i) < 9
      x.gains(i) = sum(gains);
   else
      x.gains(i) = gains(1);
   end
end
s = sscanf(h(findstr(h, 'FILT '):end), '%s', 2);
x.filt = sscanf(s(length('FILT '):end), '$%s');
s = sscanf(h(findstr(h, 'SAMPL_RATE '):end), '%s', 2);
sampl_rate = sscanf(s(length('SAMPL_RATE '):end), '%d');
if sampl_rate == 17
   x.sampl_rate = 100e3;
elseif sampl_rate == 18
   x.sampl_rate = 50e3;
end
s = sscanf(h(findstr(h, 'BUFSIZE '):end), '%s', 2);
bufsize = sscanf(s(length('BUFSIZE '):end), '%d');
s = sscanf(h(findstr(h, 'NUMBUFS '):end), '%s', 2);
numbufs = sscanf(s(length('NUMBUFS '):end), '%d');
if isempty(t_lim)
   lim = [1, 2*numbufs*bufsize/length(chans)];
else
   lim = round(x.sampl_rate*t_lim);
   lim(1) = lim(1)+1;
end
s = sscanf(h(findstr(h, 'TIME '):end), '%s', 2);
x.t = sscanf(s(length('TIME '):end), '%s');
s = h(findstr(h, 'DATE '):end);
i = find(s == ' ');
date = sscanf(s(i(1):end), '%s', 1);
x.date = [date, ' ', sscanf(s(i(2):end), '%s', 1)];
s = sscanf(h(findstr(h, 'LBL '):end), '%s', 2);
x.t_lbl = sscanf(s(length('LBL '):end), '%s');
s = sscanf(h(findstr(h, 'NORTH '):end), '%s', 2);
x.north = sscanf(s(length('NORTH '):end), '%f');
s = sscanf(h(findstr(h, 'EAST '):end), '%s', 2);
x.east = sscanf(s(length('EAST '):end), '%f');
s = sscanf(h(findstr(h, 'DEPTH '):end), '%s', 2);
x.depth = sscanf(s(length('DEPTH '):end), '%f');
s = sscanf(h(findstr(h, 'ROLL '):end), '%s', 2);
x.roll = sscanf(s(length('ROLL '):end), '%f');
s = sscanf(h(findstr(h, 'PITCH '):end), '%s', 2);
x.pitch = sscanf(s(length('PITCH '):end), '%f');
s = sscanf(h(findstr(h, 'HEADING '):end), '%s', 2);
x.heading = sscanf(s(length('HEADING '):end), '%f');
s = sscanf(h(findstr(h, 'ALTITUDE '):end), '%s', 2);
x.altitude = sscanf(s(length('ALTITUDE '):end), '%f');

if head_flag~=1

fseek(fid, 2*length(chans)*(lim(1)-1), 0);
l = lim(2)-lim(1)+1;
x.data = zeros(length(chans_out), l);
for i = 1:x.sampl_rate:l
   siz = [length(chans), min([l-i+1, x.sampl_rate])];
   data = fread(fid, siz, 'short');
   x.data(:, i:i+siz(2)-1) = data(chans_out, :);
end
x.data = 3.75/32768*x.data;
x.t_axis = ((lim(1):lim(2))-1)/x.sampl_rate;

end

fclose(fid);



