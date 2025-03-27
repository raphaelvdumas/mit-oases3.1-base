%[tt,angle]=overlay(D,Zs,Zr,Rr,r,c,num)
%
% Kevin D. LePage
% SACLANTCEN
% 22/1/98
%
% INPUTS:
%
% D	waterdepth
% Zs	source depth
% Zr	receiver depth
% Rr	receiver range (m)
% t	bottom ranges (m)
% c	sound speed (default 1500 m/s)
% num	number of multiples (default 2)
%
% OUTPUTS
%
% tt	time of arrival for each angle (times x arrival)
% angle	angles corresponding to time vector (size(t))

function[tt,angle]=overlay(D,Zs,Zr,Rr,r,c,num)

if nargin<5

num=2;

end

if nargin<4

c=1500;

end

% arrange r as desired

r=r(:);

R_in=r.^2;

R_out=(r-Rr).^2;

angle=zeros(length(r),j^2*2);

tt=zeros(length(r),j^2*2);

% j is index over incident bounces

for j=0:num

if j==0

in_even=1;

else

in_even=floor(j/(ceil(j/2)*2));

end

in_odd=1-in_even;

% k is index over back scattered bounces

for k=j:-1:0

if k==0

out_even=1;

else

out_even=floor(k/(ceil(k/2)*2));

end

out_odd=1-out_even;

Z_in=in_odd*(j*D+Zs)+in_even*(j*D+(D-Zs));

Z_out=out_odd*(k*D+Zr)+out_even*(k*D+(D-Zr));

angle(:,(j)^2*2+k*2+1)=(out_odd*(-1)+out_even)*asin((Z_out)*(R_out+Z_out.^2).^(-.5))*180/pi;

tt(:,(j)^2*2+k*2+1)=((Z_in^2+R_in).^(.5)+(Z_out^2+R_out).^(.5))/c;

%if((in_even*out_even<0)|(in_odd*out_odd>0))

if(j>0)

angle(:,(j)^2*2+k*2+2)=(in_odd*(-1)+in_even)*asin((Z_in)*(R_in+Z_in.^2).^(-.5))*180/pi;

tt(:,(j)^2*2+k*2+2)=tt(:,(j)^2*2+k*2+1);

end

end

end


