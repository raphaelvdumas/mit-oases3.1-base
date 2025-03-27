%[x_loc,y_loc,z_loc,head_out,t_out,t_lbl_out]=head_reader(fname,chan,ping)
%
% function to loop through file names (fname without _000-_119 extension)
% and get good info
%
% Kevin LePage
% 26/5/98
% SACLANTCEN
%
% INPUTS
%
% fname		filename without _000-_119 extension
% chan		desired channel vector
% ping		desired ping vector ( through 119 typically)
%
% OUTPUTS
%
% x_out		x coordinate (m)
% y_out		y coordinate (m)
% z_out		z coordinate (m)
% head_out	heading (deg N)
% t_out		time (since Y0)
% t_lbl_out	long base line time (since Y0)

function[x_out,y_out,z_out,head_out,t_out,t_lbl_out]=head_reader(ffname,chan,ping)

for ll=1:length(ping)

j=ping(ll);

bla='000'

juk=int2str(j);

for k=1:length(juk)

bla(3-length(juk)+k:3)=juk(k);

end

fname=[ffname '_'  bla]

x=x_data(fname,chan(1),[],1);

x_out=[x_out;x.east];

y_out=[y_out;x.north];

z_out=[z_out;x.depth];

head_out=[head_out;x.heading];

t_out=[t_out;x.t];

t_lbl_out=[t_out;x.t_lbl];

end


