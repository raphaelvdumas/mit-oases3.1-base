
% Loads the arrival time/amplitude data computed by BELLHOP
% mbp 9/96

% need to set ARRFIL before calling

% open the file
fid = fopen( ARRFIL, 'r');

% read

freq = fscanf( fid, '%f',  1  );
nsd  = fscanf( fid, '%i',  1  );
nrd  = fscanf( fid, '%i',  1  );
nr   = fscanf( fid, '%i',  1  );
sd   = fscanf( fid, '%f', nsd );
rd   = fscanf( fid, '%f', nrd );
rr   = fscanf( fid, '%f', nr  );

amp   = zeros( nr, 100, nrd );
delay = zeros( nr, 100, nrd );

for isd = 1:nsd
  for ird = 1:nrd
    for ir = 1:nr
      narr = fscanf( fid, '%f', 1 );
      narrmat( ir, ird, isd ) = narr;

      if narr > 0   % do we have any arrivals?
        da = fscanf( fid, '%f', [ 3, narr ] );

%        if ird == iphone   % take data for one phone only
          amp(   ir, 1:narr, ird, isd ) = da( 1, : ) + i * da( 2, : );
          delay( ir, 1:narr, ird, isd ) = da( 3, : );
%        end
      end
    end		% next receiver range
  end		% next receiver depth
end		% next source depth

fclose( fid );
