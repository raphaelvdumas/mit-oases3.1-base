[tsn,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_matreader('hla200new.mat');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:2:90];
y=sin(b).*r;
[tsn_beam]=beamform(tsn,y,fs,beam,1500); 
[a,b]=wavei(dba(tsn_beam(1:size(tsn,1),:))',tim,beam,-150,-50);
title('Bistatic HLA new (r=200 m)');
xlabel('Time (s)');
ylabel('Beam (deg)');
