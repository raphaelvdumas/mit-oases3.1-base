function[h]= hanning(n)
     a=[0:2*pi/(n-1):2*pi]';
     h=0.5*(1.0-cos(a));
