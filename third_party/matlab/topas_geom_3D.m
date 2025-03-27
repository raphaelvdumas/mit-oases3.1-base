%[X,Y]=topas_geom_3D(pos)
%
% function to return 3-D topas geometry
%
% Kevin D. LePage
% 15/7/98
% SACLANTCEN
%
% INPUTS
%
% pos		rail position (m)
%
% OUTPUTS
%
% X		X location of topas (m)
% Y		Y location of topas (m)
%
% plots topas geometry from GOATS98 experiment in current figure

function[X,Y]=topas_geom(pos)

hold on

x_rail=4481;

y_rail=4382;

z_rail=-12.5;

head_rail=126;

head_c1=126-180/pi*asin(6/22);

length_c1=22;

head_c2=126+180/pi*asin(7.6/22);

length_c2=22;

head_rail=head_rail/180*pi;

head_c1=head_c1/180*pi;

head_c2=head_c2/180*pi;

length_rail=24;

s1_pos=19.5;

s2_pos=24.5;

s3_pos=29.5;

hold on

a=plot3(x_rail+length_rail*sin(head_rail)*[.5 -.5],y_rail+length_rail*cos(head_rail)*[.5 -.5],z_rail*[1 1],'k')		

set(a,'LineWidth',2)

X=x_rail+(pos-10)*sin(head_rail);

Y=y_rail+(pos-10)*cos(head_rail);

a=plot3(X,Y,-5,'mo')		

set(a,'LineWidth',2)

a=plot3(X*[1 1],Y*[1 1],[-12.5 -5],'k')		

set(a,'LineWidth',2)

a=plot3(x_rail+(s1_pos)*sin(head_rail),y_rail+(s1_pos)*cos(head_rail),-12.5,'mo')		
set(a,'LineWidth',2)

a=plot3(x_rail+(s2_pos)*sin(head_rail),y_rail+(s2_pos)*cos(head_rail),-12.5,'mo')		
set(a,'LineWidth',2)

a=plot3(x_rail+(s3_pos)*sin(head_rail),y_rail+(s3_pos)*cos(head_rail),-12.5,'mo')		
set(a,'LineWidth',2)

a=plot3(x_rail+(length_c1)*sin(head_c1),y_rail+(length_c1)*cos(head_c1),-12.5,'mo')		
set(a,'LineWidth',2)

a=plot3(x_rail+(length_c2)*sin(head_c2),y_rail+(length_c2)*cos(head_c2),-12.5,'mo')		
set(a,'LineWidth',2)

xlabel(' range (m)')

ylabel('vertical range (m)')

zlabel('depth (m)')

hold off

