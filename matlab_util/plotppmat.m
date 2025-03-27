%plotppmat.m
file = input('E mat file (o/p pp) to plot: ');
[ts,t,z,r,b,title,parameter,planes,traces,samples,fs]=pp_matreader(file);

 clear ANGLE DATA DELTAT RANGE TMSHFT DEPTH TYPE TITLE;
 dt = 1/fs;
% time  = [t(i):dt:t(i)+(-1+size(ts,1))*dt];
 baset = [0:dt:(-1+size(ts,1))*dt];
 plotmatrix2(baset,ts,'PP\_MATFILE');
% imagesc(baset,ZZZZ,DDDD)
 grid on
