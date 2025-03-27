% [image,tim,beam] = tbp(filenm);
% Generates time-beam plot in amplitude
% filenm: Name of Matlab trace file greated by OASES PP
% image:  real matrix(nb x nt) with image amplitudes
% tim:    real vector(nt) with time axis
% beam:   real vector(nb) with beam angles in degrees
%
function[image,tim,beam] = tbp(filenm)
[tsg,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_matreader(filenm);
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:2:90];
[tsg_beam]=beamform(tsg,z,fs,beam,1500); 
image=abs(tsg_beam(1:length(tim),:))';




