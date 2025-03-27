function radar(beam,tim,tsg_beam,cmin,cmax)
rr=tim*1500/2;
xMat=cos(beam*pi/180.)' * rr;
yMat=sin(beam*pi/180.)' * rr;
zMat=dba(tsg_beam(1:length(tim),:)');

clf reset
pcolor(xMat , yMat , zMat)
caxis([cmin cmax])
shading flat
colorbar
colormap jet
axis image
xlabel('Plan Range from Transmitter (m)')
ylabel('Depth Relative to Transmitter (m)')
grid on
zoom on
