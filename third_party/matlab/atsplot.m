% function[amp]=atsplot(x,scale,t)
%
% function to plot staked time series
%
% Kevin D. LePage
% SACLANT ASW Research Centre
% 10/30/97
%
% INPUTS
%
% x	vector time series (time x number channels)
% scale	magnification factor (default 1)
% t	time axis (default vector row index)
%
% OUTPUTS
%
% amp   absolute value of largest number in x

function[amp]=atsplot(x,scale,t,z,string)

if nargin<2

plot((real(x)/abs(max(max(x)))+ones(size(x,1),1)*[0:1:size(x,2)-1]))

elseif nargin<3

plot((real(x)/abs(max(max(x)))*scale+ones(size(x,1),1)*[0:1:size(x,2)-1]))

elseif nargin<4

if(size(x,1)~=length(t))

x=x.';

end

plot(t,(real(x)/abs(max(max(x)))*scale+ones(size(x,1),1)*[0:1:size(x,2)-1]))

elseif nargin<5

if(size(x,1)~=length(t))

x=x.';

end

plot(t,(real(x)*max(diff(z))/abs(max(max(x)))*scale+ones(size(x,1),1)*z(:)'))

else

if(size(x,1)~=length(t))

x=x.';

end


if size(string,1)==1

plot(t,(real(x)*max(diff(z))/abs(max(max(x)))*scale+ones(size(x,1),1)*z(:)'),string)

elseif size(string,1)==length(z)

for j=1:length(z)

plot(t,(real(x(:,j))*max(diff(z))/abs(max(max(x)))*scale+ones(size(x,1),1)*z(j)'),string(j))

hold on

end

hold off

end

end

amp=max(max(abs(x)));
