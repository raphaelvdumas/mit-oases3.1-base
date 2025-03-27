[tsg,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_matreader('wg_scat.mat');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:90];
[tsg_beam]=beamform(tsg,z,fs,beam,1500); 
[a,b]=wavei(dba(tsg_beam(1:size(tsg,1),:))',tim,beam,-150,-90);
title('Roughness Scattering -  Waveguide');
xlabel('Time (s)');
ylabel('Beam (deg)');
