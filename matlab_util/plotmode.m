% plotmode.m

% plot the modes produced by KRAKEN
% need to set ntot, modes, filename below

fid = fopen( 'foo.modA', 'r' )

lrecl   = fscanf( fid, '%i' );
pltitl = fgetl( fid );

temp = fscanf( fid, '%f' );
freq   = temp( 1 );
nmedia = temp( 2 );
ntot   = temp( 3 );
nmat   = temp( 4 );
m      = temp( 5 );

temp   = fscanf( fid, '%f', [ nmedia ] );
junk   = fgetl( fid );
junk   = fgetl( fid );
junk   = fgetl( fid );
junk   = fgetl( fid );

junk   = fgetl( fid );
z      = fscanf( fid, '%f', [ 1, ntot ] );
junk   = fgetl( fid );
ckt     = fscanf( fid, '%f', [ 2, m ] );
ck = ckt( 1, : )' + i * ckt( 2, : )';

for mode = 1:m
   junk = fgetl( fid );
   phit = fscanf( fid, '%f', [ 2, ntot] );
   phi( :, mode )  = phit( 1, : )' + i * phit( 2, : )';
end

figure; imagesc( 1:m, z, real( phi ) ); colorbar
figure; surf( 1:m, z, real( phi ) ); colorbar
