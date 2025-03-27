function [msg,x,nfft,Fs,window,noverlap] = gspecchk(P)

msg = [];

x = P{1}; 
if (length(P) > 1) & ~isempty(P{2})
    nfft = P{2};
else
    nfft = min(length(x),256);
end
if (length(P) > 2) & ~isempty(P{3})
    Fs = P{3};
else
    Fs = 2;
end
if length(P) > 3 & ~isempty(P{4})
    window = P{4};
else
    if length(nfft) == 1
        window = hanning(nfft);
    else
        msg = 'You must specify a window function.';
    end
end
if length(window) == 1, window = hanning(window); end
if (length(P) > 4) & ~isempty(P{5})
    noverlap = P{5};
else
    noverlap = ceil(length(window)/2);
end

% NOW do error checking
if (length(nfft)==1) & (nfft<length(window)), 
    msg = 'Requires window''s length to be no greater than the FFT length.';
end
if (noverlap >= length(window)),
    msg = 'Requires NOVERLAP to be strictly less than the window length.';
end
if (length(nfft)==1) & (nfft ~= abs(round(nfft)))
    msg = 'Requires positive integer values for NFFT.';
end
if (noverlap ~= abs(round(noverlap))),
    msg = 'Requires positive integer value for NOVERLAP.';
end
if min(size(x))~=1,
    msg = 'Requires vector (either row or column) input.';
end






