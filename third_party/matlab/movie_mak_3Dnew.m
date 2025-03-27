%[M]=movie_mak_3Dnew(out,X_total,Y_total,vec_receiver_3D,x_ax,y_ax)
%
% Kevin D. LePage
% SACLANTCEN
% 15/7/98
%
% function to make movie of auv synthetic aperture images
%
% INPUTS
%
% out			the image file (length(X) x length(Y)) x num traj
% X_total		x axis of image (slowest index on row space of out)
% Y_total    		y axis of image (fast index on row space of out)
% vec_receiver_3D	matrix [x,y,z,head] for auv trajectory (num traj x 4)
% x_ax			x axis pair
% y_ax			y axis pair
%
% OUTPUTS
%
% M	movie file (num_pixels x num traj)

function[M]=movie_mak_3Dnew(out,X,Y,vec_receiver,x_ax,y_ax)

head=vec_receiver(:,4);

clg

topas_geom_3D(5)
view([30,45])

hold on

j=160;

surf(ones(length(Y),1)*X(:)',Y(:)*ones(1,length(X)),-12.5*ones(length(Y),length(X)),reshape(out(:,160),length(Y),length(X)));shading('flat');colormap('jet')
caxis([0 1])
a=gca;
b=colorbar;
axes(b);
ylabel('power/max(power)');
axes(a)

axis([x_ax(1) x_ax(2) y_ax(1) y_ax(2) -14 0])
hold off
M=moviein(160);

for j=1:160,

clg

topas_geom_3D(5);

hold on;

view([30,45])

surf(ones(length(Y),1)*X(:)',Y(:)*ones(1,length(X)),-12.5*ones(length(Y),length(X)),reshape(out(:,j).^2,length(Y),length(X)));
shading('flat');
caxis([0 1])
colormap('jet');
a=gca;
b=colorbar;
axes(b);
ylabel('power/max(power)');
axes(a)

axis([x_ax(1) x_ax(2) y_ax(1) y_ax(2) -14 0]);
grid;
if j>1
hold on
a=plot3(vec_receiver(1:j-1,1),vec_receiver(1:j-1,2),vec_receiver(1:j-1,3),'r.');
set(a,'MarkerSize',10)
end
hold on
a=plot3(vec_receiver(j,1),vec_receiver(j,2),vec_receiver(j,3),'r+');
set(a,'MarkerSize',10)
hold off
M(:,j)=getframe;
end



