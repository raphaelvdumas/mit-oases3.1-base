%[M]=movie_maker_3D(out,X,Y,vec_receiver_3D,x_ax,y_ax)
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
% X			x axis of image (slowest index on row space of out)
% Y     		y axis of image (fast index on row space of out)
% vec_receiver_3D	matrix [x,y,z,head] for auv trajectory (num traj x 4)
% x_ax			x axis pair
% y_ax			y axis pair
%
% OUTPUTS
%
% M	movie file (num_pixels x num traj)

function[M]=movie_maker_3D(out,X,Y,vec_receiver,x_ax,y_ax)

head=vec_receiver(:,4);

colormap('bone')
map_bone=colormap;

map_yellow=[map_bone(:,3) map_bone(:,3) map_bone(:,1)/1e6];

colormap('cool')
map_cool=colormap;

clg

topas_geom_3D(5)
view([30,45])

hold on

j=160;

x=ones(30,1)*[-.7:.05:.7];
y=real(cos([0:29]'/15*pi)*(1-[-.7:.05:.7].^2/.7/.7).^(.5)*.03);
z_nose=real(sin([0:29]'/15*pi)*(1-[-.7:.05:.7].^2/.7/.7).^(.5)*.03);
x_nose=sin(head(j)*pi/180)*(x-.7)+cos(head(j)*pi/180)*y;
y_nose=cos(head(j)*pi/180)*(x-.7)-sin(head(j)*pi/180)*y;

%a=surfl(x_nose'+vec_receiver((ceil(j/8)-1)*8+1,1),y_nose'+vec_receiver((ceil(j/8)-1)*8+1,2),z_nose'+vec_receiver((ceil(j/8)-1)*8+1,3),[30,45],[.3,1,1,1]);shading('flat');view([0,90])

%set(a,'Colormap',map_cool);

x=ones(30,1)*[-.95:.05:.95];
y=real(cos([0:29]'/15*pi)*(1-[-.95:.05:.95].^2/.95/.95).^(.5)*.254);
z_auv=real(sin([0:29]'/15*pi)*(1-[-.95:.05:.95].^2/.95/.95).^(.5)*.254);
x_auv=sin(head(j)*pi/180)*(x-.95-.7)+cos(head(j)*pi/180)*y;
y_auv=cos(head(j)*pi/180)*(x-.95-.7)-sin(head(j)*pi/180)*y;


%a=surfl(x_auv'+vec_receiver((ceil(j/8)-1)*8+1,1),y_auv'+vec_receiver((ceil(j/8)-1)*8+1,2),z_auv'+vec_receiver((ceil(j/8)-1)*8+1,3),[30,45],[.3,1,1,1]);shading('flat');view([0,90])

%set(a,'Colormap',map_yellow)

surf(ones(length(Y),1)*X(:)',Y(:)*ones(1,length(X)),-12.5*ones(length(Y),length(X)),reshape(out(:,160),length(Y),length(X)));shading('flat');colormap('jet')
caxis([0 1])
a=gca;
b=colorbar;
axes(b);
ylabel('power/max(power)');
axes(a)

%wavei(reshape(out(:,160).^2,length(Y),length(X)),X,Y)

axis([x_ax(1) x_ax(2) y_ax(1) y_ax(2) -14 0])
hold off
M=moviein(160);
for j=1:160,
clg
topas_geom_3D(5);
hold on;

x=ones(30,1)*[-.7:.05:.7];
y=real(cos([0:29]'/15*pi)*(1-[-.7:.05:.7].^2/.7/.7).^(.5)*.03);
z_nose=real(sin([0:29]'/15*pi)*(1-[-.7:.05:.7].^2/.7/.7).^(.5)*.03);
x_nose=sin(head(j)*pi/180)*(x-.7)+cos(head(j)*pi/180)*y;
y_nose=cos(head(j)*pi/180)*(x-.7)-sin(head(j)*pi/180)*y;
%colormap('cool');
%surfl(x_nose'+vec_receiver((ceil(j/8)-1)*8+1,1),y_nose'+vec_receiver((ceil(j/8)-1)*8+1,2),z_nose'+vec_receiver((ceil(j/8)-1)*8+1,3),[30,45],[.3,1,1,1]);shading('flat');view([0,90])
x=ones(30,1)*[-.95:.05:.95];
y=real(cos([0:29]'/15*pi)*(1-[-.95:.05:.95].^2/.95/.95).^(.5)*.254);
z_auv=real(sin([0:29]'/15*pi)*(1-[-.95:.05:.95].^2/.95/.95).^(.5)*.254);
x_auv=sin(head(j)*pi/180)*(x-.95-.7)+cos(head(j)*pi/180)*y;
y_auv=cos(head(j)*pi/180)*(x-.95-.7)-sin(head(j)*pi/180)*y;
%colormap('bone')
map=colormap;
colormap([map(:,3) map(:,3) map(:,1)/1e6])
%surfl(x_auv'+vec_receiver((ceil(j/8)-1)*8+1,1),y_auv'+vec_receiver((ceil(j/8)-1)*8+1,2),z_auv'+vec_receiver((ceil(j/8)-1)*8+1,3),[30,45],[.3,1,1,1]);shading('flat');view([0,90])

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

%wavei(reshape(out(:,j).^2,length(Y),length(X)),X,Y,0,1,1,1,'power/max(power)');
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



