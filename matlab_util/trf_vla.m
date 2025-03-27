%function[vla,sd,z,r,f,f0,omegim]=trf_vla(fname,r_vla,az_vla)
%
% function to read trf file and return a the complex pressure for a vla
%
% H. Schmidt
% MIT
% 25/1/06
%
% INPUTS
%
% fname		file name (ascii string, with our without .trf extension)
% r_vla         range of vla (km)
% az_vla        azimuth of vla (deg) 

%
% OUTPUTS
%
% vla		complex transfer function (length(z) x length(f))
% sd		source depth (m)
% r             receiver range (km)
% z		receiver depths
% f		receiver frequencies
% fo		source center frequency
% omegim        imaginary part of radian frequency

function[vla,sd,z,r,f,fc,omegim]=trf_vla(fname,r_vla,az_vla)

[out,sd,z,range,f,fc,omegim,msuft]=trf_reader_oases3d(fname);

[dr,indx]=min(abs(range-r_vla));
theta=az_vla*pi/180.;
r=range(indx);

vla=out(:,indx,1,:);
m_max=msuft/2-1;
for m=1:m_max
 vla=vla+out(:,indx,2*m,:)*cos(m*theta)+out(:,indx,2*m+1,:)*sin(m*theta);
end







