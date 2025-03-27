function[a,b] = plimage(image,tim,beam,cmin,cmax,tit)
[a,b]=wavei(dba(image(:,1:length(tim))),tim,beam,cmin,cmax);
title(tit);
xlabel('Time (s)');
ylabel('Beam (deg)');
pause(.2);
