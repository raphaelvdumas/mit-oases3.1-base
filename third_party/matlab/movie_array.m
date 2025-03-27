ang=[0:pi/20:2*pi];

mx=max(max(abs(t_focus)));

for j=1:10:1024,
a=reshape(t_focus(j,:),51,51);
a(find(a>mx))=mx*ones(size(find(a>mx)));
a(find(a<-mx))=-mx*ones(size(find(a<-mx)));
a(1,31)=mx;
a(2,31)=-mx;
imagesc(x(1,:),z(:,1),a);
colormap('hsv');
%hold on
%plot(-(j-150)/fs*1500*cos(ang),-(j-150)/fs*1500*sin(ang),'r');
%plot(-(j-150)/fs*1500*cos(ang),38*2-(j-150)/fs*1500*sin(ang),'r');
%plot((j-150)/fs*1500*cos(ang),-(117.5*2-27.5*2)+(j-150)/fs*1500*sin(ang),'r');
%hold off

colorbar;
drawnow;
A(:,(j-1)/10+1)=getframe;
end

