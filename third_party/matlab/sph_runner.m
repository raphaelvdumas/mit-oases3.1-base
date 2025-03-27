z_sph=[-.53:.01:.53]';
r_sph=.53*sin(acos(z_sph/.53));
x_sph=r_sph*cos([0:5:360]*pi/180);
y_sph=r_sph*sin([0:5:360]*pi/180);
z_sph=z_sph*ones(1,73);

for j=1:size(ts_out_0,2),
    bla_180(:)=ts_out_180(:,j,2:end);
    bla(:)=ts_out_0(:,j,:);
    figure(1);
    wavei(real(fliplr([fliplr(bla_180) bla])),[-fliplr(r(2:end)) r]*1e3,-z,-1e4,1e4);
    hold on
    a=plot(r_sph,z_sph);
    set(a,'LineWidth',2);
    a=plot(-r_sph,z_sph);
    set(a,'LineWidth',2);
    a=plot([-2 2],[1 1]*0.55)
    set(a,'LineWidth',2)
  % surfl(x_sph,y_sph,z_sph);
  % shading('flat');
  % colormap('bone');
    hold off
    axis('equal');
    drawnow;
    mov(:,j)=getframe;
    close;
end
