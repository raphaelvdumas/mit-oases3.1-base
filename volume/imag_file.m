[tsg,t,z,r,b,tit,param,planes,traces,samples,fs]=pp_matreader('test_grad_scat');
tim=[0.0:1/fs:0.0+(samples-1)/fs];
beam = [-90:90];
[tsg_beam]=beamform(tsg,z,fs,beam,1500); 
image=tsg_beam'
t=[0.0:1/fs:0.0+(size(image,2)-1)/fs];
save im_file image t beam fs
[a,b]=wavei(dba(image),t,beam,-120,-60);
title('Volume Scattering -  Waveguide');
xlabel('Time (s)');
ylabel('Beam (deg)');
