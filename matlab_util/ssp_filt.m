A = [  0.0  1520.0
  13.0  1520.0
  20.0  1518.0
  30.0  1516.0
  40.0  1514.0
  50.0  1512.0
  60.0  1510.0
  70.0  1509.0
 130.0  1507.0
 135.0  1507.0 ]

z = A( :, 1 );
c = A( :, 2 );

D = 135.0   % water depth
ptsperm = 10;
N = ptsperm * D + 1 % points to use in sampling c(z)

zint = linspace( 0.0, D, N );
cint = interp1( z, c, zint, 'spline' );
cint = interp1( z, c, zint );

figure
plot( z, c )
hold on
plot( zint, cint )

idec = 10
zdec = zint( 1:idec:N );
cdec = decimate( cint, idec );
cdec = cdec + mean( cint ) - mean( cdec );

plot( zdec, cdec )

c_resamp = interp( cdec, idec );

plot( zint, c_resamp )

% remove linear term

ctop = cint( 1 );
cbot = cint( N );
trend = linspace( ctop, cbot, N )';
cflat = cint - trend;
idec = 30
zdec = zint( 1:idec:N );
cdec = decimate( cflat, idec );
c_resamp = interp( cdec, idec );

% add back linear term

N2 = size( c_resamp, 1 );
trend2 = linspace( ctop, cbot, N2 )';
c_resamp = c_resamp + trend2;
z_resamp = linspace( 0.0, D, N2 );

figure
plot( z, c )
hold on
plot( z_resamp, c_resamp )



