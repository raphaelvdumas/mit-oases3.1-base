%[]=geom_plotter(fname,num,pos)
%
% function to plot topas-auv geometry for run fname
%
% Kevin D. LePage
% 27/5/98
% SACLANTCEN
%
% INPUTS
%
% fname 	file name (including directory structure, without _000)
% num		vector of files for which gemetry needs to be plotted
% pos		position of topas on rail (m from far end)
%
% plots result and saves to present directory

function[]=geom_plotter(fname,num,pos)

[x_loc,y_loc,head_out,t_out]=head_reader(fname,1,num);

a=plot(x_loc,y_loc,'g')

set(a,'LineWidth',2)

hold on

for j=1:length(num)

text(x_loc(j)+1,y_loc(j),int2str(num(j)));

end

a=plot(x_loc,y_loc,'r+')

set(a,'LineWidth',2)

[X,Y]=topas_geom(pos);

grid 

axis('equal')

fprintf('input limits are desired plot (two clicks:)')

[xlim,ylim]=ginput(2);

fprintf('\nThank you\n')

axis([min(xlim) max(xlim) min(ylim) max(ylim)])

drawnow

[val,ind]=find(47==double(fname));

ind=ind(length(ind));

fname=fname(ind+1:length(fname));

eval(['title(''' fname ''')'])

xlabel('horizontal range (m)')

ylabel('vertical range (m)')

eval(['print -depsc ' fname '_geometry'])

hold off








