
% Fourier synthesis to make a time series from the transfer function
% mbp 9/96

% Need to set
%   Tstart = starting time
%   specfile (file containing spectrum)
%   root of field files
%   N = size of transform

clear all

Tstart   = 19.0;
specfile = [ 'munkts.fft' ];
shdfile  = [ 'work/munkts' ];
N   = 5000;

% load source spectrum

fid = fopen( specfile );
temp = fscanf( fid, '%f', [ 3, inf ] );
shat = temp( 2, : ) + i * temp( 3, : );
nfreq = size( shat, 2 );

nsd = 1;

for isd = 1:nsd

% read in the model transfer function

clear rmodhat

for ifreq = 1:nfreq
  % read

  filename = [ shdfile int2str( ifreq ) ]
  fid = fopen( [ filename '.asc' ], 'r' );

  pltitl = fgetl( fid );
  junk   = fgetl( fid );
  freq( ifreq ) = fscanf( fid, '%f', 1 );
  nsd    = fscanf( fid, '%i', 1 );
  nrd    = fscanf( fid, '%i', 1 );
  nrr    = fscanf( fid, '%i', 1 );

  sd     = fscanf( fid, '%f', nsd );
  rd     = fscanf( fid, '%f', nrd );
  rr     = fscanf( fid, '%f', nrr );

  % advance to the right record or that sd and read the field
  for ii = 1:isd
    temp1   = fscanf( fid, '%f', [ 2 * nrr, nrd ] );
  end
%  rmodhat( ifreq, :, : ) = temp1;
  rmodhat( ifreq, 1:size( temp1, 1), 1:size( temp1, 2 ) ) = temp1;
  fclose( fid );
end   % next frequency

rmodhatc = rmodhat( :, 1:2:2*nrr-1, : ) + i * rmodhat( :, 2:2:2*nrr, : );

nrr = 1;
foo = rmodhatc( :, 1, : );
rmodhatc = foo;

% ********************************
% compute the received time series
% ********************************

% remove the travel time delay

for ird = 1:nrd
  for ir = 1: nrr
    rmodhatc( :, ir, ird ) = rmodhatc( :, ir, ird ) ...
            .* exp( -i * 2 * pi * Tstart * freq )';
  end
end

% weight transfer function by source spectrum

for ifreq = 1: nfreq
  rmodhatc( ifreq, :, : ) = rmodhatc( ifreq, :, : ) * shat( ifreq );
end

% inverse FFT to compute received time-series

rmod = ifft( rmodhatc, N );

% set up time vector based on usual FFT sampling rules

deltaf = freq( 2 ) - freq( 1 );
Tmax = 1 / deltaf;
deltat = Tmax / N;
time   = linspace( 0.0, Tmax - deltat, N );

% heterodyne with the base frequency
% Be careful that rmod has adequate time-sampling ...

for ird = 1:nrd
  for ir = 1: nrr
    rmod( :, ir, ird ) = rmod( :, ir, ird ) .* ...
                        exp(  i * 2 * pi * time' * freq( 1 ) );
  end
end

% spectrum is conjugate symmetric so:

rmod = 2 * real( rmod );

irr = 1;
rmod = squeeze( rmod( :, irr, : ) );	% can only plot 2D matrix

figure; plot( rmod( :, 26 ) )

figure
peak = max( max( rmod ) );
% imagesc( time, rd, squeeze( rmod )' );
nint = 1000;
imagesc( time, rd, rmod' ); ...
caxis( [ -peak/5, peak/5 ] ); colorbar
xlabel( 'Time (s)' )
ylabel( 'Depth (m)' )
title( 'KRAKEN impulse response (dB)' )
	 
%rmod = abs( hilbert( rmod( :, : ) ) );

% normalize

%for ir = 1:nrr
%  temp = 20 * log10( rmod( :, ir ) / max( rmod( :, ir ) ) ) + 30;
%  I = find( temp < 0 );
%  temp( I ) = zeros( size( I ) );
%  rmod( :, ir ) = temp / norm( temp );
%end
%rmod = rmod';

% save for future use ...

%fname = [ 'model' int2str( isd ) ]
%eval( [ 'save ' fname ' nrr rr rmod' ] );

end;   % next source depth

