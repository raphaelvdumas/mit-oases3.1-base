%[name,date,time,lat,lon,depth,heading]=nav_reader(fname)
%
% Function to read *.txt nav files
% Kevin D. LePage
% 17/4/98
%
% Inputs
%
% fname		file name (with or without .txt extension)
%
% Outputs
%
% name		shot name vector string
% date		shot date vector string
% time		shot time vector string
% lat		decimal latitude vector
% lon		decimal longitude vector
% depth		water depth vector
% heading	ships heading degrees N (vector)

function[name,date,time,lat,lon,depth,heading]=nav_reader(fname)

len_name=length(fname);

if fname(len_name-3:len_name)~='.txt'

fname=[fname '.txt'];

end

fid=fopen(fname);

out=fread(fid,'uchar');

eol=find(out==10);

num_lines=length(eol);

eol=[0;eol];

for j=1:num_lines-1

blanks=find(out(eol(j)+1:eol(j+1))==9);

blanks=[blanks(find(diff(blanks)~=1));blanks(length(blanks))];

blanks=eol(j)+blanks;

%diff_blanks=diff(blanks);

%starts=blanks(find(diff_blanks~=1)-1)+1;

%ends=blanks(find(diff_blanks~=1))-1;

%end_name=min(blanks);

% first load the name string

name(j,:)=setstr(out(eol(j)+1:blanks(1)-1)');

% second load the date string

date(j,:)=setstr(out(blanks(1):blanks(2)-1)');

% third load the time string

time(j,:)=setstr(out(blanks(2):blanks(3)-1)');

% fourth load the latitude

setstr(out(blanks(3):blanks(4)-1)');

lat(j,:)=str2num(setstr(out(blanks(3):blanks(4)-1)'));

% fifth load the longitude

lon(j,:)=str2num(setstr(out(blanks(4):blanks(5)-1)'));

% sixth load the depth

depth(j,:)=str2num(setstr(out(blanks(5):blanks(6)-1)'));

% seventh load the heading

heading(j,:)=str2num(setstr(out(blanks(6):eol(j+1)-1)'));

end

fclose(fid);
