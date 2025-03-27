% function[amp]=atsplot_surf(x,scale,t,z)
%
% function to plot color picture of stacked time series
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
% y	vertical axis (default column index)
%
% OUTPUTS
%
% amp   absolute value of largest number in x

function[amp]=atsplot_surf(x,scale,t,z)

amp=max(max(abs(x)));

if nargin<2

surfl(real(x'));shading('flat');colormap('bone');view([0 90])

elseif nargin<3

surfl(real(x'));shading('flat');colormap('bone');view([0 90-scale])

elseif nargin<4

surfl(([1:size(x,2)]'*ones(1,size(x,1)))',(ones(size(x,2),1)*t(:)')',real(flipud(x)),[-115 45]);shading('flat');colormap('bone');view([-90 90-scale])

else

surfl((z(:)*ones(1,size(x,1)))',(ones(size(x,2),1)*t(:)')',real(flipud(x)),[-115 45]);shading('flat');colormap('bone');view([-90 90-scale])


end

