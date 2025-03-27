
% plot a BELLHOP ray file

fid = fopen( 'drefdJKPS.ray', 'r' );

% read header stuff

TITLE  = fgetl(  fid );
FREQ   = fscanf( fid, '%f', 1 );
NBEAMS = fscanf( fid, '%i', 1 );
DEPTHT = fscanf( fid, '%f', 1 );
DEPTHB = fscanf( fid, '%f', 1 );

% read rays

figure
view( 0, -90 ); % flip plot so that z-axis is pointing down
xlabel( 'Range (m)' )
ylabel( 'Depth (m)' )
title( TITLE )
%axis( [ DEPTHT DEPTHB 0.0 1000.0 ] ) 
hold on

for ibeam = 1:NBEAMS
   nsteps = fscanf( fid, '%i', 1 )
   ray = fscanf( fid, '%f', [2 nsteps] );
   plot( ray( 1, : ), ray( 2, : ) )
   % drawnow   % flush graphics buffer
end

axis( [ 0 7000 DEPTHT DEPTHB ] )


