%[M]=movie_maker(out,X,Y,vec_receiver,x_ax,y_ax)
%
% Kevin D. LePage
% SACLANTCEN
% 12/7/98
%
% function to make movie of auv synthetic aperture images
%
% INPUTS
%
% out		the image file (length(X) x length(Y)) x num traj
% X		x axis of image (slowest index on row space of out)
% Y     	y axis of image (fast index on row space of out)
% vec_receiver	vector of x-y pairs of auv trajectory (num traj x 2)
% x_ax		x axis pair
% y_ax		y axis pair
%
% OUPUTS
%
% M	movie file (num_pixels x num traj)

function[M]=movie_maker(out,X,Y,vec_receiver,x_ax,y_ax)

clg
topas_geom(5)
hold on
wavei(reshape(out(:,160).^2,length(Y),length(X)),X,Y)
axis([x_ax(1) x_ax(2) y_ax(1) y_ax(2)])
hold off
M=moviein(160);
for j=1:160,
clg
topas_geom(5);
hold on;
wavei(reshape(out(:,j).^2,length(Y),length(X)),X,Y,0,1,1,1,'power/max(power)');
axis([x_ax(1) x_ax(2) y_ax(1) y_ax(2)]);
grid;
if j>1
hold on
a=plot(vec_receiver(1:j-1,1),vec_receiver(1:j-1,2),'y.');
set(a,'MarkerSize',10)
end
hold on
a=plot(vec_receiver(j,1),vec_receiver(j,2),'r+');
set(a,'MarkerSize',10)
hold off
M(:,j)=getframe;
end