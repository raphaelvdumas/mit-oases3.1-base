%[ss]=image_basin(x,y,data,x_data,y_data,head_data,times_data,angles_data,alpha)
% 
% Function to do least square estimation of scattering strength 
% on checkerboard defined by x and y.  Data is in intensity
% Assumes cylindrical spreading loss plus some attenuation term
%
% Kevin D. LePage
% 19/4/98
% SACLANTCEN
%
% INPUTS:
% 
% x		x locations of hypothesized scattering strengths (m)
% y		y locations of hypothesized scattering strengths (m)
% data		intensity data in band (snapshots x (times x angles))
%		to give true ss source strength should be removed
% x_data	x locations of snapshots (m) (snapshots)		
% y_data	y locations of snapshots (m) (snapshots)
% head_data	heading of snapshots (zero is pointing towards positive y)
% times_data	times at which intensities are given (times)
% angles_data	angles at which intensities are coming from (angles deg)
% alpha		good estimate of attenuation dB/m
%
% OUTPUTS
%
% ss		estimated scattering strength ss

function[SS]=image_basin(x,y,data,x_data,y_data,head_data,times_data,angles_data,alpha)

% size things as desired

size_x=length(x);

x=ones(length(y),1)*x(:)';

x=x(:);

y=y(:)*ones(1,size_x);

y=y(:);

angles_data=angles_data(:)';

num_times=length(times_data);

num_scat=length(x);

num_snapshots=length(x_data);

num_angles=length(angles_data);

% get the ranges between the hypothesized source locations and the various
% snapshot positions

for j=1:num_snapshots

fprintf('\rj=%d out of %d',j,num_snapshots)

r=((x-x_data(j)).^2+(y-y_data(j)).^(2)).^(.5);

angle=atan2(y-y_data(j),x-x_data(j));

angles_data_global_stbd=atan2(sin((90-angles_data-head_data(j))/180*2*pi),cos((90-angles_data-head_data(j))/180*2*pi));

angles_data_global_port=atan2(sin((90+angles_data-head_data(j))/180*2*pi),cos((90+angles_data-head_data(j))/180*2*pi));

% find which scatterers show up in which beams

[mmin,ind_angle]=min(abs((angle*ones(1,2*num_angles)-ones(num_scat,1)*[angles_data_global_stbd angles_data_global_port])'));

% put in ambiguity

ind_angle(find(ind_angle>num_angles))=ind_angle(find(ind_angle>num_angles))-num_angles;

% find which scatterers show up at which times

slow=2/1500;

[mmin,ind_time]=min(abs((r*ones(1,num_times)*slow-ones(num_scat,1)*times_data)'));

% Green function for cylindrical spreading plus attenuation

G(num_scat*(j-1)+1:num_scat*j)=r.^(-1).*exp(-alpha*r);

I(num_scat*(j-1)+1:num_scat*j)=num_angles*num_times*(j-1)+(ind_angle-1)*num_times+ind_time;

J(num_scat*(j-1)+1:num_scat*j)=1:num_scat;

% end of loop over different observation points

end

% find the data which is not used

ind_data=[];

for kk=1:length(data)

fprintf('\rkk=%d out of %d, length ind=%d',kk,length(data),length(ind_data))

temp=find(I==kk);

if (temp)

ind_data=[ind_data;kk];

I(temp)=length(ind_data)*ones(size(temp));

end

end

% build sparse A matrix of propagators

A=sparse(I,J,G);

keyboard

% solve for scatterer distributions

SS=A\data(ind_data);

