[tsg,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_reader('grad_5k.asc');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:90];
[tsg_beam]=beamform(tsg,z,fs,beam,1500); 
[a,b]=wavei(dba(tsg_beam)',tim,beam,-140,-80);
title('Volume Scattering -  Cradient');
xlabel('Time (s)');
ylabel('Beam (deg)');
 
