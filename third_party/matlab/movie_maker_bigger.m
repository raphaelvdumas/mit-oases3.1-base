clg
topas_geom(5)
hold on
wavei([reshape(out_bot(:,160),26,51);reshape(out_old_big(:,160),51,51)],X_big,[Y_bot Y_big])
axis([4470 4520 4347.5 4395])
hold off
M_old=moviein(160);
for j=1:160,
clg
topas_geom(5);
hold on;
wavei([reshape(out_bot(:,j),26,51);reshape(out_old_big(:,j),51,51)],X_big,[Y_bot Y_big],0,1,1,1,'power/max(power)');
axis([4470 4520 4347.5 4395]);
grid;
if j>1
hold on
a=plot(vec_receiver(1:j-1,1),vec_receiver(1:j-1,2),'b.');
set(a,'MarkerSize',10)
end
hold on
a=plot(vec_receiver(j,1),vec_receiver(j,2),'r+');
set(a,'MarkerSize',10)
hold off
M_old(:,j)=getframe;
end