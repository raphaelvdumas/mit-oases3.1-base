%function[out,sd,z,range,f,fo,omegim]=trf_reader_oases(fname)
%
% function to read oases trf file and return contents
%
% Kevin D. LePage
% SACLANTCEN
% 22/8/00
%
% INPUTS
%
% fname		file name (ascii string, with our without .trf extension)
%
% OUTPUTS
%
% out		complex transfer function (length(z)xlength(r)xlength(f))
% sd		source depth (m)
% z		receiver depths
% range		receiver ranges
% f		receiver frequencies
% fo		receiver center frequency
% omegim        imaginary part of radian frequency

function[out,sd,z,range,f,fc,omegim]=trf_reader(fname)

if fname(length(fname)-3:length(fname))~='.trf'

fname=[fname '.trf']

end

[fid,message]=fopen(fname,'r');

junk=' ';

while junk(1)~='P'

junk=fscanf(fid,'%s',1)

end

fseek(fid,-length(junk),0);

sign=' ';

while ((sign~='+')&(sign~='-'))

sign=setstr(fread(fid,1))

end

fread(fid,2,'float');

fc=fread(fid,1,'float');

fread(fid,2,'float');

sd=fread(fid,1,'float')

fread(fid,2,'float');

z1=fread(fid,1,'float');

z2=fread(fid,1,'float');

num_z=fread(fid,1,'int');

z=[z1:(z2-z1)/(num_z-1):z2]';

fread(fid,2,'float');

r1=fread(fid,1,'float');

dr=fread(fid,1,'float');

nr=fread(fid,1,'int');

fread(fid,2,'float');

nfft=fread(fid,1,'int');

bin_low=fread(fid,1,'int');

bin_high=fread(fid,1,'int');

dt=fread(fid,1,'float');

f=[0:1/dt/(nfft-1):1/dt];

f=f(bin_low:bin_high);

fread(fid,2,'float');

icdr=fread(fid,1,'int');

fread(fid,2,'float');

omegim=fread(fid,1,'float');

range=[r1:dr:r1+dr*(nr-1)];

fread(fid,2,'float');

msuft=fread(fid,1,'int');

fread(fid,2,'float');

isrow=fread(fid,1,'int');

fread(fid,2,'float');

inttyp=fread(fid,1,'int');

fread(fid,2,'float');

idummy(1)=fread(fid,1,'int');

fread(fid,2,'float');

idummy(2)=fread(fid,1,'int');

fread(fid,2,'float');

dummy(1)=fread(fid,1,'int');

fread(fid,2,'float');

dummy(2)=fread(fid,1,'int');

fread(fid,2,'float');

dummy(3)=fread(fid,1,'int');

fread(fid,2,'float');

dummy(4)=fread(fid,1,'int');

fread(fid,2,'float');

dummy(5)=fread(fid,1,'int');

fread(fid,1,'float');

nf=length(f);

out=zeros(num_z,nr,nf);

for j=1:nf
for jj=1:nr
fprintf('\rf=%d out of %d, r=%d out of %d',j,nf,jj,nr)
fread(fid,1,'float');
temp=fread(fid,num_z*4-2,'float');
temp=temp(1:2:length(temp))+i*temp(2:2:length(temp));
temp=temp(1:2:length(temp));
fread(fid,1,'float');
out(:,jj,j)=temp;
end
end

%for j=1:num_z
%for jj=1:nr
%fprintf('\rj=%d out of %d, jj=%d out of %d',j,num_z,jj,nr)
%fread(fid,1,'float');
%temp=fread(fid,nf*2,'float');
%fread(fid,1,'float');
%out(j,jj,:)=(temp(1:2:length(temp))+i*temp(2:2:length(temp)));
%end
%end

%temp=fread(fid,'float');

%nf=length(f)+1;

%out=zeros(num_z,nr,nf-1);

%for j=1:nf-1

%out(1:num_z,1:nr,j)=reshape(temp(j*2:nf*2:length(temp)),nr,num_z)'+i*reshape(temp(j*2+1:nf*2:length(temp)),nr,num_z)';

%end

fclose(fid);







