[tsr,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_reader('rgh_5k.asc');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:90];
[tsr_beam]=beamform(tsr,z,fs,beam,1500); 
[a,b]=wavei(dba(tsr_beam)',tim,beam,-120,-60);
title('Roughness Scattering -  Iso-velocity');
xlabel('Time (s)');
ylabel('Beam (deg)');
