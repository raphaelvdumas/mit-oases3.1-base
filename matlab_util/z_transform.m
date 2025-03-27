function g = z_transform(x, k, w, a)

[m, n] = size(x); oldm = m;
if m == 1, x = x(:); [m, n] = size(x); end

if nargin < 2, k = length(x); end
if nargin < 3, w = exp(-i .* 2 .* pi ./ k); end
if nargin < 4, a = 1; end

if any([size(k) size(w) size(a)]~=1),
	error('Inputs M, W and A must be scalars.')
end

%------- Length for power-of-two fft.

nfft = 1;
while nfft < (m + k - 1), nfft = 2 .* nfft; end

%------- Premultiply data.

kk = ( (-m+1):max(k-1,m-1) ).';
kk2 = (kk .^ 2) ./ 2;
ww = w .^ (kk2);   % <----- Chirp filter is 1./ww
nn = (0:(m-1))';
aa = a .^ ( -nn );
aa = aa.*ww(m+nn);
y = x .* aa(:,ones(1,n));

%------- Fast convolution via FFT.

fy = fft(  y, nfft );
fv = fft( 1 ./ ww(1:(k-1+m)), nfft );   % <----- Chirp filter.
fy = fy .* fv(:,ones(1, n));
g  = ifft( fy );

%------- Final multiply.

g = g( m:(m+k-1), : ) .* ww( m:(m+k-1),ones(1, n) );

if oldm == 1, g = g.'; end


