%[]=rev_over_map(freq)
%
% function to plot reverb-map overlays
%
% Kevin D. LePage
% 23/4/98
% SACLANTCEN
%
% INPUTS
%
% freq	third octave band
%
% OUTPUTS

function[]=rev_over_map(freq)

load /usr1/users/lepage/image/inverse_500m.mat

[data,times]=image_data_prepare(freq,10);

for jjj=1:6

X1=x_data(jjj)/1000+times(1:90)'*cos((90-head_data(jjj)-angles_data)*pi/180)*4*750/1000;

Y1=y_data(jjj)/1000+times(1:90)'*sin((90-head_data(jjj)-angles_data)*pi/180)*4*750/1000;

X2=x_data(jjj)/1000+times(1:90)'*cos((90-head_data(jjj)+angles_data)*pi/180)*4*750/1000;

Y2=y_data(jjj)/1000+times(1:90)'*sin((90-head_data(jjj)+angles_data)*pi/180)*4*750/1000;

aa=reshape(data((jjj-1)*length(times)*length(angles_data)+1:jjj*length(times)*length(angles_data)),length(times),length(angles_data));

wavei(dbp(aa(1:90,:)),X1,Y1,-160,-100,1,1,'dB re source power')

hold on

wavei(dbp(aa(1:90,:)),X2,Y2,-160,-100,1,1,'dB re source power')

axis([min(min(x_data))/1000-30 max(max(x_data))/1000+30 min(min(y_data))/1000-30 max(max(y_data))/1000+30])

hold on

load /usr1/users/lepage/nav/sus_geometry.mat

load /usr1/users/lepage/bathy_formica_v4/new/high_res_data.mat

zdata=[zdata(1:770);zdata(1540:length(zdata))];

xdata=[xdata(1:770);xdata(1540:length(xdata))];

ydata=[ydata(1:770);ydata(1540:length(ydata))];

depths=[0 10 20 30 40 50 60 70 80 90 100 120 140 160 180 200 400];

%col=['w.';'c.';'c.';'c.';'c.';'c.';'c.';'c.';'c.';'c.';'w.';'b.';'b.';'b.';'b.';'w.';'w.'];

for l=1:length(depths)

ind=find(zdata==depths(l));

%eval(['plot((xdata(ind)-lon(13))*60*6080/3.28084*cos(36.5*pi/180)/1000,(ydata(ind)-lat(13))*60*6080/3.28084/1000,''' col(l,:) ''')'])

plot((xdata(ind)-lon(13))*60*6080/3.28084*cos(36.5*pi/180)/1000,(ydata(ind)-lat(13))*60*6080/3.28084/1000,'w.')

end

%axis([min(min(x_data))-30 max(max(x_data))+30 min(min(y_data))-30 max(max(y_data))+30])

plot((lon-lon(13))*60*6080/3.28084*cos(36.5*pi/180)/1000,(lat-lat(13))*60*6080/3.28084/1000,'r+')

hold off

eval(['title(''MF HLA reverb in ' int2str(freq) ' Hz third octave band'')'])

xlabel('longitudinal range from vla (km)')

ylabel('latitudinal range from vla (km)')

['print -depsc rose_' int2str(jjj) '_' int2str(freq)]

eval(['print -depsc rose_' int2str(jjj) '_' int2str(freq)])

end

%print -depsc map_overlay

