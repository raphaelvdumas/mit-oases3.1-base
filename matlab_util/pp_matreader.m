%[ts,t,z,r,b,tit,parameter,planes,traces,samples,fs]=pp_matreader(file)
%
% function to read mat files output by pp
%
% Eddie Scheer
% WHOI
% 8/24/99
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

function[ts,t,z,r,b,tit,parameter,planes,traces,samples,fs]=pp_matreader(file)

ANGLE = [];
TITLE = [];

% open file

eval(['load ',file]);

% read header
 b = double(ANGLE);
 ts = double(DATA);
 fs = 1/double(DELTAT);
 r = double(RANGE);
 t = double(TMSHFT);
 z = double(DEPTH);
 parameter = double(TYPE);
 tit = double(TITLE);
 planes = 1;
 traces = size(ts,2);
 samples = size(ts,1);





