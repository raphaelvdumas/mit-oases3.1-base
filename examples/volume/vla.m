[tls,t,zv,rv,bv,tit,param,planes,traces,samples,fs]=pp_matreader('crit_scat.mat');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
tim2=[0.0:1/fs:0.5];
beam = [-90:2:90];
[tls_beam]=beamform(tls,zv,fs,beam,1500); 
[a,b]=wavei(dba(tls_beam(1:length(tim),:))',tim,beam,-120,-60);
title('Mostatic VLA - rgh_scat');
xlabel('Time (s)');
ylabel('Beam (deg)');
