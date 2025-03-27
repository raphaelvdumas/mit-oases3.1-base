clg
subplot(1,2,1)
atsplot(xdata_norm(1:2:160,:)',5,[0:2047]/fs*1000,1:2:160,'b')
axis([0 40 -10 170]) 
hold on
atsplot(xdata_norm(1:2:80,:)',5,[0:2047]/fs*1000,1:2:80,'r')
xlabel('Time (msec)')
ylabel('Ping')
subplot(2,2,2)
topas_geom(5)
hold on
wavei(reshape(out_98_8_7_old(:,160),26,26),X,Y)
axis([4470 4510 4357.5 4395])
hold off
[M_old,cmap]=capture;
%M_old=moviein(160);
M_old=M_old(:)*ones(1,20);
for j=1:8:160,
clg
subplot(1,2,1)
atsplot(xdata_norm(1:2:160,:)',5,[0:2047]/fs*1000,1:2:160,'b')
axis([0 40 -10 170])
if j>1
hold on
atsplot(xdata_norm(1:2:j+7,:)',5,[0:2047]/fs*1000,1:2:j+7,'r')
end
xlabel('Time (msec)')
ylabel('Ping')
subplot(2,2,2)
topas_geom(5);
hold on;
wavei(reshape(out_98_8_7_old(:,j+7),26,26),X,Y,0,1,1,1,'power/max(power)');
axis([4470 4510 4357.5 4395]);
grid;
if j>1
hold on
a=plot(vec_receiver(1:j+6,1),vec_receiver(1:j+6,2),'b.');
set(a,'MarkerSize',10)
end
hold on
a=plot(vec_receiver(j+7,1),vec_receiver(j+7,2),'r+');
set(a,'MarkerSize',10)
hold off
[junk,cmap]=capture;
M_old(:,(j-1)/8+1)=junk(:);
cmap(:,((j-1)/8+1-1)*3+1:((j-1)/8+1)*3)=cmap;
end