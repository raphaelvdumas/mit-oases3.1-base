%[ts,t,z,r,b,title,parameter,planes,traces,samples,fs]=pp_reader(file)
%
% function to read ascii files output by pp
%
% Kevin D. LePage
% SACLANTCEN
% 14/1/98
%
% INPUTS	
%
% file 		file name (string)
%
% OUTPUTS
%
% ts		time series (length(t) x (planes x traces))
% t		start times (
% z		depths (vector of corresponding depths)
% r		range (vector of corrsponding ranges)
% b		bearing (vector of corresponding bearings)
% title		character string
% parameter	character string (N is normal stress)
% planes	number (for different bearings, for instance)
% traces	number (for depth or range stacks, for instance)
% samples	number (N)
% fs		sample frequency (Hz)

function[ts,t,z,r,b,title,parameter,planes,traces,samples,fs]=pp_reader(file)

% open file

fid=fopen(file)

% read header

title=setstr(fread(fid,72,'uchar')')

fread(fid,1,'uchar');

parameter=setstr(fread(fid,12,'uchar')')

fread(fid,22,'uchar');

planes=str2num(setstr(fread(fid,12,'uchar')'))

fread(fid,29,'uchar');

traces=str2num(setstr(fread(fid,12,'uchar')'))

fread(fid,29,'uchar');

samples=str2num(setstr(fread(fid,12,'uchar')'))

% determine number of rows

nrows=floor(samples/5);

remain=samples-nrows*5;

fread(fid,36,'uchar');

fs=str2num(setstr(fread(fid,12,'uchar')'))

fread(fid,37,'uchar');

% predefine ts

ts=zeros(samples,planes*traces);

for j=1:planes*traces

fprintf('\rj=%d out of %d',j,planes*traces)

fread(fid,1,'uchar');

r(j)=str2num(setstr(fread(fid,15,'uchar')'));

fread(fid,19,'uchar');

z(j)=str2num(setstr(fread(fid,15,'uchar')'));

fread(fid,19,'uchar');

b(j)=str2num(setstr(fread(fid,15,'uchar')'));

fread(fid,23,'uchar');

t(j)=str2num(setstr(fread(fid,15,'uchar')'));

fread(fid,29,'uchar');

for k=1:nrows

junk=str2num(setstr(fread(fid,15,'uchar')'));
ts((k-1)*5+1,j)=junk;
ts((k-1)*5+2,j)=str2num(setstr(fread(fid,15,'uchar')'));
ts((k-1)*5+3,j)=str2num(setstr(fread(fid,15,'uchar')'));
ts((k-1)*5+4,j)=str2num(setstr(fread(fid,15,'uchar')'));
ts(k*5,j)=str2num(setstr(fread(fid,15,'uchar')'));

fread(fid,1,'uchar');

end

for k=1:remain

ts(nrows*5+k,j)=str2num(setstr(fread(fid,15,'uchar')'));

end

fread(fid,1,'uchar');

end

fclose(fid)