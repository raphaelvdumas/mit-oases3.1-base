function y = untrend(x,o)
if nargin == 1
	o = 1;
end
[m,n] = size(x);
if m == 1	% If a row, turn into column vector
	x = x(:);
end
[mp,np] = size(x);
if o == 0	% Remove just mean from each column
	y = x - ones(mp,1)*mean(x);
else		% Remove straight-line fit from each column
	a = [(1:mp)'/mp ones(mp,1)];
	y = x - a*(a\x);
end
if m == 1
	y = y.';
end

