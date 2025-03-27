[ts,t,z,r,b,tittxt,param,planes,traces,samples,fs]=pp_reader('pl_22-mp.asc');
tim=[0.02:1/fs:0.02+2047/fs];
beam = [-90:90];
[ts_beam]=beamform(ts,z,fs,beam,1525); 
[a,b]=wavei(dba(ts_beam)',tim,beam,-110,-50);
xlabel('Time (s)');
ylabel('Beam (deg)');
 
