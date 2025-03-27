

% plots a single TL surface in dB

% open the file
fid = fopen('wedge.asc','r');

% read

pltitl = fgetl( fid );
junk   = fgetl( fid );
junk   = fscanf( fid, '%f', 1 );
nsd    = fscanf( fid, '%i', 1 );
nrd    = fscanf( fid, '%i', 1 );
nrr    = fscanf( fid, '%i', 1 );

sd     = fscanf( fid, '%f', nsd );
rd     = fscanf( fid, '%f', nrd );
rr     = fscanf( fid, '%f', nrr );

isd = 1;
for i = 1:isd
   temp1   = fscanf( fid, '%f', [ 2 * nrr, nrd ] );
   i, size(temp1)
end

zt = rd;
taker = 1:nrr;
rt = rr( taker );

rkm = rt / 1000.0;

figure

tlt = 20.0 * log10( abs( temp1( 1:2:2*nrr, : )' + sqrt( -1 ) * temp1( 2:2:2*nrr, : )') );
pcolor( rkm, zt, tlt );  caxis( [ -100 -60 ] ); ...
shading flat; colormap( jet ); colorbar; view( 0, -90 );
xlabel( 'Range (km)' ); ylabel( 'Depth (m)' );
title( pltitl )
%set(1,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tl.ps

%figure
%plot( rkm, tlt( 5, : ) ); axis( [ 0 100 -100 -60 ] );
%xlabel( 'Range (km)' ); ylabel( 'TL (dB)' );
%title( pltitl )
%set(2,'PaperPosition', [ 0.25 0.25 5.0 3.5 ] )
%print -deps tlslice.ps
