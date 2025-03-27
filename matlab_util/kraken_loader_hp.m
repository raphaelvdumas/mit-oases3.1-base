%function[kr,freq,mode,z]=kraken_loader(name)
%
% Kevin D. LePage
% SACLANTCEN
% 9/30/97
%
% INPUTS
%
% name	name (withou .mod extension) of modefile
%
% OUTPUTS
% 
% kr	complex modal eigenvalues (rad/m)
% freq	frequency (Hz)
% mode	complex mode shape functions (num depths x phi)
% z	depths for mode shape functions (m)

function[kr,freq,mode,z]=kraken_loader(name)

fid=fopen([name '.mod']);

% get logical record length

lrecl=fread(fid,1,'int');

% read header

title=setstr(fread(fid,80));
freq=fread(fid,1,'float');
nmedia=fread(fid,1,'int');
ntot=fread(fid,1,'int');
nmat=fread(fid,1,'int');

% go to second record

fseek(fid,lrecl*4*1,-1);

for j=1:nmedia
n(j)=fread(fid,1,'int');
eval(['mater_' int2str(j) '=setstr(fread(fid,8));'])
end

% go to third record

fseek(fid,lrecl*4*2,-1);

bctop=setstr(fread(fid,1));
cpt=fread(fid,1,'float32')+i*fread(fid,1,'float32');
cst=fread(fid,1,'float32')+i*fread(fid,1,'float32');
rhot=fread(fid,1,'float32')
deptht=fread(fid,1,'float32')

bcbot=setstr(fread(fid,1));
cpb=fread(fid,1,'float32')+i*fread(fid,1,'float32');
csb=fread(fid,1,'float32')+i*fread(fid,1,'float32');
rhob=fread(fid,1,'float32');
depthb=fread(fid,1,'float32');

% go to fourth record

fseek(fid,lrecl*4*3,-1);

for j=1:nmedia
depth(j)=fread(fid,1,'float32');
rho(j)=fread(fid,1,'float32');
end

% go to fifth record

fseek(fid,lrecl*4*4,-1);

m=fread(fid,1,'int');
lrecl=fread(fid,1,'int');

% go to sixth record

fseek(fid,lrecl*4*5,-1);

z=fread(fid,ntot,'float32');

for j=1:m

% go to next m records for mode shape functions

fseek(fid,lrecl*4*(5+j),-1);

mode(:,j)=fread(fid,ntot*2,'float32');

end

mode=mode(1:2:ntot*2,:)+i*mode(2:2:ntot*2,:);

% go to next m records for eigenvalues

fseek(fid,lrecl*4*(6+m),-1);

for j=1:m

kr(j)=fread(fid,1,'float32')+i*fread(fid,1,'float32');

end

