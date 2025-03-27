function [c,lags] = kevcorr(a, b, maxlag, option)
error(nargchk(1,4,nargin))
if  length(a)<=1,
    error('1st input argument must be a vector or matrix.')
end
onearray = 0;
if  nargin == 1
    b = [];  maxlag = [];  option = 'none';
elseif  nargin == 2
    maxlag = [];  option = 'none';
    if  isstr(b)
        option = b;  b=[];
    elseif  length(b)==1
        maxlag = b; b = [];
    end
elseif  nargin == 3
    option = 'none';
    if  isstr(b)
       error('argument list not in correct order')
    end
    if  length(b)>1
       if isstr(maxlag), option = maxlag; maxlag = []; end
    elseif length(b)<=1
       if isstr(maxlag)
          option = maxlag;  maxlag = b;  b = [];
       end
    end
end

if  length(b)==1 & length(maxlag)==1
    error('3rd arg is maxlag, 2nd arg cannot be scalar')
end
if  length(maxlag)>1
    error('maxlag must be a scalar')
end
if  isempty(option)
    option = 'none';
end
option = lower(option);

[ar,ac] = size(a);
La = ar;
if  ar==1,  La = ac;  end
Lb = length(b);
if  Lb>1
    if La ~= Lb & ~strcmp(option,'none')
       error('OPTION must be ''none'' for different length vectors A and B')
    end
    if min(size(a))==1 & min(size(b))==1
	onearray = 2;   %-- do just one cross-correlation
	if La > Lb
	   b(La) = 0;
	elseif La < Lb
	   a(Lb) = 0;
	end
	a = [a(:) b(:)];
    elseif min(size(b))>1
       error('B must be a vector (min(size(B))==1).')
    else
       error('When B is a vector, A must be a vector.')
    end
end
if Lb==1
   error('something is messed up with b')
end
if size(a,1)==1  &  Lb==0   % a is a row vector
   a = a(:);
end
if  isempty(maxlag)
   maxlag = size(a,1)-1;
end

% check validity of option
nopt = nan;
if  strcmp(option, 'none')
	nopt = 0;
elseif  strcmp(option, 'coeff')
	nopt = 1;
elseif  strcmp(option, 'biased')
	nopt = 2;
elseif  strcmp(option, 'unbiased')
	nopt = 3;
end
if isnan(nopt)
	error('Unknown OPTION')
end
[nr, nc] = size(a);
nsq  = nc^2;
mr = 2 * maxlag + 1;
nfft = 2^nextpow2(mr);
nsects = ceil(2*nr/nfft);
if nsects>4 & nfft<64
   nfft = min(4096,max(64,2^nextpow2(nr/4)));
end

pp = 1:nc;
n1 = pp(ones(nc,1),:);  n2 = n1';
aindx = n1(:)';   bindx = n2(:)';

c = zeros(nfft,nsq);
minus1 = (-1).^(0:nfft-1)' * ones(1,nc);
af_old = zeros(nfft,nc);
n1 = 1;
nfft2 = nfft/2;
while( n1 < nr )
   n2 = min( n1+nfft2-1, nr );
   af = fft(a(n1:n2,:), nfft);
   c = c + af(:,aindx).* conj( af(:,bindx) + af_old(:,bindx) );
   af_old = minus1.*af;
   n1 = n1 + nfft2;
end;
if  n1==nr
   af = ones(nfft,1)*a(nr,:);
   c = c + af(:,aindx).* conj( af(:,bindx) + af_old(:,bindx) );
end
c = ifft(c);

jkl = reshape(1:nsq,nc,nc)';
mxlp1 = maxlag+1;
c = [ conj(c(mxlp1:-1:2,:)); c(1:mxlp1,jkl(:))];

if nopt == 1	% return normalized by sqrt of each autocorrelation at 0 lag
% do column arithmetic to get correct autocorrelations
	tmp = sqrt(c(mxlp1,diag(jkl)));
	tmp = tmp(:)*tmp;
	cdiv = ones(mr,1)*tmp(:).';
	c = c ./ cdiv;
elseif nopt == 2	% biased result, i.e. divide by nr for each element
	c = c / nr;
elseif nopt == 3	% unbiased result, i.e. divide by nr-abs(lag)
	c = c ./ ([nr-maxlag:nr (nr-1):-1:nr-maxlag]' * ones(1,nsq));
end

if onearray==2
	c = c(:,2);
end
if ar == 1              % make output a row, same as input
   c = c.';
end
if ~any(any(imag(a)))
   c = real(c);
else
   c = conj(c);
end
lags = -maxlag:maxlag;


