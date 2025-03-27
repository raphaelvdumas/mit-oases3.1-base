function plotmatrix(x,name)
% Script to plot columns of matrix offset by overall MAX value
nargin;
name;
[m,n] = size(x);
% generate temp matrix with just means removed...
for i=1:n
	mmmean = mean(x(:,i));
	temporary(:,i) = x(:,i)-mmmean;
end
mmmax = max(max(temporary));
for i=1:n
	temporary(:,i) = temporary(:,i) + (i-1)*mmmax;
end
plot(temporary);
if (nargin > 1)
	title(name);
end
grid on
clear temporary;
