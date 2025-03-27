function varargout = plotcon( filename,m, n, p )

% plot a single TL surface in dB
% usage:
% plotcon( filename, m, n, p, units )
% (m, n, p) optional subplot spec
% '.shd' is the fault file extension if not specified
%
% mbp

global units jkpsflag

% read

%disp( 'PlotShd uses the first bearing and source depth in the shade file; check OK' )
itheta = 1;
isd    = 1;

eval(['load ' filename]);
whos
pressure=10.^(-double(DATA')/20);
zt = linspace(double(YMIN),double(YMAX),double(NY));
rt = 1000*linspace(double(XMIN),double(XMAX),double(NX));
PlotTitle=TITLE;
SubTitle = SUBTITLE;
if (exist('p'))
    PlotTitle = ' ';
    if (p==1)
        figlabel='(a)';
        SubTitle = 'Azimuth = 0^o';
    elseif (p==2)
        figlabel = '(b)';
        SubTitle = 'Azimuth = 45^o';
    else
        figlabel = '(c)';
        SubTitle = 'Azimuth = 90^o';
    end
end
  
        
%[ PlotTitle, PlotType, freq, atten, Pos, pressure ] = read_shd( filename );

%pressure = squeeze( pressure( itheta, isd, :, : ) );
%zt       = Pos.r.depth;
%rt       = Pos.r.range;

% set labels in m or km
xlab     = 'Range (m)';
if ( strcmp( units, 'km' ) )
    rt      = rt / 1000.0;
    xlab    = 'Range (km)';
end

if ( nargin == 1 )
    %figure
else
    if ( p == 1 )
        %figure( 'units', 'normalized', 'outerposition', [ 0 0 1 1 ] ); % first subplot
        figure
    else
        hold on   % not first subplot
    end
    subplot( m, n, p )
end
%%

tlt = abs( pressure );

tlt( isnan( tlt ) ) = 1e-6;   % remove NaNs
tlt( isinf( tlt ) ) = 1e-6;   % remove infinities

icount = find( tlt > 1e-7 );         % for stats, only these values count
tlt( tlt < 1e-7 ) = 1e-7;            % remove zeros
tlt = -20.0 * log10( tlt );          % so there's no error when we take the log

% compute some statistics to automatically set the color bar

tlmed = median( tlt( icount ) );    % median value
tlstd = std( tlt( icount ) );       % standard deviation
tlmax = tlmed + 0.75 * tlstd;       % max for colorbar
tlmax = 10 * round( tlmax / 10 );   % make sure the limits are round numbers
tlmin = tlmax - 50;                 % min for colorbar

% optionally remove cylindrical spreading:
% tlt = tlt + ones( nrd, 1 ) * 10.0 * log10( rt )';
%%

tej = flipud( jet( 256 ) );  % 'jet' colormap reversed

if ( size( tlt, 1 ) > 1 && size( tlt, 2 ) > 1 )
    h = imagesc( rt, zt, tlt );   % imagesc produces a better PostScript file, using PostScript fonts
    %pcolor( rt, zt, tlt );  ...
    shading interp; colormap( tej );
    caxisrev( [ tlmin, tlmax ] )
    set( gca, 'YDir', 'Reverse' )
    xlabel( xlab )
    ylabel( 'Depth (m)' );
%    title( { deblank( PlotTitle ); [ 'Freq = ' num2str( freq ) ' Hz    Sd = ' num2str( double(SD) ) ' m' ] } )
    title( { PlotTitle ; SubTitle } )
else   % line plots
    if ( length(rt) > 1 )   % TL vs. range
        h = plot( rt, tlt );
        xlabel( xlab );
        ylabel( 'TL (dB)' )
        set( gca, 'YDir', 'Reverse' )
        title( deblank( PlotTitle ) )
    else
        % TL vs. depth
        h = plot( tlt', zt );
        set( gca, 'YDir', 'Reverse' )
        set( gca, 'Xdir', 'Reverse' )
        xlabel( 'TL (dB)' )
        ylabel( 'Depth (m)' );
        title( deblank( PlotTitle ) )
    end
end

if (exist('p'))
     h=text( min(rt)+0.96 * (max( rt )-min(rt)), min( zt )-(max(zt)-min(zt))/10, figlabel );
     set(h,'FontSize',16);
end
     drawnow

if ( nargout == 1 )
    varargout( 1 ) = { h };   % return a handle to the figure
end

% fixed size for publications
if ( jkpsflag )
    set( gca, 'ActivePositionProperty', 'Position', 'Units', 'centimeters' )
    set( gcf, 'Units', 'centimeters' )
    set( gcf, 'PaperPositionMode', 'auto');   % this is important; default is 6x8 inch page

    if ( exist( 'm' ) )
        set( gca, 'Position', [ 2    2 + ( m - p ) * 9.0     14.0     7.0 ] )
        set( gcf, 'Position', [ 3 15 19.0 m * 10 ] )
    else
        set( gca, 'Position', [ 2    2                       14.0     7.0 ] )
        set( gcf, 'Units', 'centimeters' )
        set( gcf, 'Position', [ 3 15 19.0 10.0 ] )
    end
    
    %     set( gcf, 'Units', 'centimeters' )
    %     set( gcf, 'PaperPositionMode', 'manual' );
    %     set( gcf, 'PaperPosition', [ 3 3 15.0 10.0 ] )
    
end
