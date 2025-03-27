function w = hamming(n)
w = .5*(1 - cos(2*pi*(1:n)'/(n+1)));

