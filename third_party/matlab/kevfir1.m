function [b,a] = kevfir1(N,Wn,varargin)
%KEVFIR1   FIR filter design using the window method.
%   B = KEVFIR1(N,Wn) designs an N'th order lowpass FIR digital filter
%   and returns the filter coefficients in length N+1 vector B.
%   The cut-off frequency Wn must be between 0 < Wn < 1.0, with 1.0 
%   corresponding to half the sample rate.  The filter B is real and
%   has linear phase, i.e., even symmetric coefficients obeying B(k) =
%   B(N+2-k), k = 1,2,...,N+1.
%
%   If Wn is a two-element vector, Wn = [W1 W2], KEVFIR1 returns an
%   order N bandpass filter with passband  W1 < W < W2.
%   B = KEVFIR1(N,Wn,'high') designs a highpass filter.
%   B = KEVFIR1(N,Wn,'stop') is a bandstop filter if Wn = [W1 W2].
%
%   If Wn is a multi-element vector, 
%          Wn = [W1 W2 W3 W4 W5 ... WN],
%   KEVFIR1 returns an order N multiband filter with bands
%    0 < W < W1, W1 < W < W2, ..., WN < W < 1.
%   B = KEVFIR1(N,Wn,'DC-1') makes the first band a passband.
%   B = KEVFIR1(N,Wn,'DC-0') makes the first band a stopband.
%
%   For filters with a passband near Fs/2, e.g., highpass
%   and bandstop filters, N must be even.
%   
%   By default KEVFIR1 uses a Hamming window.  Other available windows,
%   including Boxcar, Hanning, Bartlett, Blackman, Kaiser and Chebwin
%   can be specified with an optional trailing argument.  For example,
%   B = KEVFIR1(N,Wn,kaiser(N+1,4)) uses a Kaiser window with beta=4.
%   B = KEVFIR1(N,Wn,'high',chebwin(N+1,R)) uses a Chebyshev window.
%
%   By default, the filter is scaled so the center of the first pass band 
%   has magnitude exactly one after windowing. Use a trailing 'noscale' 
%   argument to prevent this scaling, e.g. B = KEVFIR1(N,Wn,'noscale'), 
%   B = KEVFIR1(N,Wn,'high','noscale'), B = KEVFIR1(N,Wn,wind,'noscale').
%

%   KEVFIR1 is an M-file implementation of program 5.2 in the IEEE
%   Programs for Digital Signal Processing tape. 

%   Reference(s):
%     [1] "Programs for Digital Signal Processing", IEEE Press
%         John Wiley & Sons, 1979, pg. 5.2-1.

nargchk(2,5,nargin);

% Up to 3 optional input arguments, always in this order:
%   1 - Filter type flag, can be 'high', 'stop', '', 'DC-0', or 'DC-1'
%   2 - Window vector
%   3 - 'noscale' flag

% default optional parameter values:
Ftype = '';
Wind = [];
Scale = 'scale';

SCALING = 1;

if isempty(N) | ~isreal(N) | N~=round(N) | N<=0
    error('N must be a real, positive integer.')
end

nw = length(Wind);

nbands = length(Wn) + 1;
if (nbands > 2) & isempty(Ftype)
    Ftype = 'DC-0';  % make sure default 3 band filter is bandpass
end
First_Band = isempty(findstr('DC-0',Ftype)) & isempty(findstr('HIGH',Ftype));
mags = rem( First_Band + (0:nbands-1), 2);

L = N + 1;
odd = rem(L, 2);
if (mags(nbands) & ~odd)
      disp('For highpass and bandstop filters, order must be even.')
      disp('Order is being increased by 1.')
      N = N + 1;  L = L + 1;
      odd = 1;
end
if nw ~= 0 & nw ~= L
   error('The window length must be the same as the filter length.')
end
if nw == 0   % replace the following with the default window of your choice.
   Wind = hamming(L);
end
%
% to use Kaiser window, beta must be supplied
% att = 60; % dB of attenuation desired in sidelobe
% beta = 0.1102*(att-8.7);
% wind = kaiser(L,beta);

if  any( Wn<0 | Wn>1 )
   error('Frequencies must fall in range between 0 and 1.')
end
if  any(diff(Wn)<0)
   error('Frequencies must be increasing')
end

Wn = Wn(:)';
ff = [0,Wn(1:nbands-1); Wn(1:nbands-1),1];
mags = [mags(:)'; mags(:)'];
hh = kevfirls(L-1,ff(:),mags(:));

b = hh.*Wind(:)'; 
a = 1;

if SCALING
    if First_Band
        b = b / sum(b);  % unity gain at DC
    else
        if ff(4)==1
            % unity gain at Fs/2
            f0 = 1;
        else
            % unity gain at center of first passband
            f0 = mean(ff(3:4));
        end
        b = b / abs( exp(-j*2*pi*(0:L-1)*(f0/2))*(b.') );
    end
end

