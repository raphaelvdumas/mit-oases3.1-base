function handle=colorbah(loc,font)
%COLORBAR Display color bar (color scale).
%	COLORBAR('vert') appends a vertical color scale to 
%	the current axis. COLORBAR('horiz') appends a 
%	horizontal color scale.
%
%	COLORBAR(H) places the colorbar in the axes H. The
%	colorbar will be horizontal if the axes H width > height
%	(in normalized coordinates).
%
%	COLORBAR without arguments either adds a new vertical
%	color scale or updates an existing colorbar.
%
%	H = COLORBAR(...) returns a handle to the colorbar axis.

%	Clay M. Thompson 10-9-92
%	Copyright (c) 1984-94 by The MathWorks, Inc.
%	$Revision: 1.4 $  $Date: 1994/03/25 14:06:26 $

if nargin<1, loc = 'vert'; end
ax = [];
if nargin==1,
  if ~isstr(loc), 
    ax = loc; 
    if ~strcmp(get(ax,'type'),'axes'),
      error('Requires axes handle.');
    end
    rect = get(ax,'position');
    if rect(3) > rect(4), loc = 'horiz'; else loc = 'vert'; end
  end
end

% Determine color limits by context.  If any axes child is an image
% use scale based on size of colormap, otherwise use current CAXIS.

ch = get(gca,'children');
hasimage = 0; t = [];
for i=1:length(ch),
  if strcmp(get(ch(i),'type'),'image'),
    hasimage = 1;
    t = get(ch(i),'UserData'); % Info stored by imshow or imagesc
  elseif strcmp(get(ch(i),'Type'),'surface'), % Texturemapped surf?
    if strcmp(get(ch(i),'FaceColor'),'texturemap')
      hasimage = 2;
    t = get(ch(i),'UserData'); % Info stored by imshow or imagesc
    end
  end
end
if hasimage,
  if isempty(t), t = [0.5 size(colormap,1)+0.5]; end
else
  t = caxis;
end

h = gca;

if nargin==0,
  % Search for existing colorbar
  ch = get(gcf,'children'); ax = [];
  for i=1:length(ch),
    d = get(ch(i),'userdata');
    if prod(size(d))==1, if d==h, 
      ax = ch(i); 
      pos = get(ch(i),'Position');
      if pos(3)<pos(4), loc = 'vert'; else loc = 'horiz'; end
      break; 
    end, end
  end
end

if strcmp(get(gcf,'NextPlot'),'replace'),
  set(gcf,'NextPlot','add')
end

if loc(1)=='v', % Append vertical scale to right of current plot
  stripe = 0.075; edge = 0.02; 

  if isempty(ax),
    pos = get(h,'Position');
    [az,el] = view;
    if all([az,el]==[0 90]), space = 0.05; else space = .1; end
    set(h,'Position',[pos(1) pos(2) pos(3)*(1-stripe-edge-space) pos(4)])
    rect = [pos(1)+(1-stripe-edge)*pos(3) pos(2) stripe*pos(3) pos(4)];

    % Create axes for stripe
    ax = axes('Position', rect);
  else
    axes(ax);
  end
  
  % Create color stripe
  n = size(colormap,1);
  image([0 1],t,[1:n]'); set(ax,'Ydir','normal')

	if nargin==2
	set(ax,'FontSize',font)
	end

  % Create color axis
  ylim = get(ax,'ylim');
  units = get(ax,'Units'); set(ax,'Units','pixels');
  pos = get(ax,'Position');
  set(ax,'Units',units);
  yspace = get(ax,'FontSize')*(ylim(2)-ylim(1))/pos(4)/2;
  xspace = .25*get(ax,'FontSize')/pos(3);
  yticks = get(ax,'ytick');
  ylabels = get(ax,'yticklabels');
  labels = []; width = [];
  for i=1:length(yticks),
    labels = [labels;text(xspace,yticks(i),deblank(ylabels(i,:)), ...
         'HorizontalAlignment','right', ...
         'VerticalAlignment','middle', ...
         'FontName',get(ax,'FontName'), ...
         'FontSize',get(ax,'FontSize'), ...
         'FontAngle',get(ax,'FontAngle'), ...
         'FontStrikeThrough',get(ax,'FontStrikeThrough'), ...
         'FontUnderline',get(ax,'FontUnderline'), ...
         'FontWeight',get(ax,'FontWeight'))];
    width = [width;get(labels(i),'Extent')];
  end

  % Shift labels over so that they line up
  [dum,k] = max(width(:,3)); width = width(k,3);
  for i=1:length(labels),
    pos = get(labels(i),'Position');
    set(labels(i),'Position',[pos(1)+width/3*2 pos(2:3)])
  end

  % If we need an exponent then draw one
  [ymax,k] = max(abs(yticks));
  if abs(abs(str2num(ylabels(k,:)))-ymax)>sqrt(eps),
    ex = log10(max(abs(yticks)));
    ex = sign(ex)*ceil(abs(ex));
    l = text(0,ylim(2)+2*yspace,'x 10', ...
         'FontName',get(ax,'FontName'), ...
         'FontSize',get(ax,'FontSize'), ...
         'FontAngle',get(ax,'FontAngle'), ...
         'FontStrikeThrough',get(ax,'FontStrikeThrough'), ...
         'FontUnderline',get(ax,'FontUnderline'), ...
         'FontWeight',get(ax,'FontWeight'));
    width = get(l,'Extent');
    text(width(3)-xspace,ylim(2)+3.2*yspace,num2str(ex), ...
         'FontName',get(ax,'ExpFontName'), ...
         'FontSize',get(ax,'ExpFontSize'), ...
         'FontAngle',get(ax,'ExpFontAngle'), ...
         'FontStrikeThrough',get(ax,'ExpFontStrikeThrough'), ...
         'FontUnderline',get(ax,'ExpFontUnderline'), ...
         'FontWeight',get(ax,'ExpFontWeight'));
  end

  set(ax,'yticklabelmode','manual','yticklabels','')
  set(ax,'xticklabelmode','manual','xticklabels','')

else, % Append horizontal scale to top of current plot

  if isempty(ax),
    pos = get(h,'Position');
    stripe = 0.075; space = 0.1;
    set(h,'Position',...
      [pos(1) pos(2)+(stripe+space)*pos(4) pos(3) (1-stripe-space)*pos(4)])
    rect = [pos(1) pos(2) pos(3) stripe*pos(4)];

    % Create axes for stripe
    ax = axes('Position', rect);
  else
    axes(ax);
  end

  % Create color stripe
  n = size(colormap,1);
  image(t,[0 1],[1:n]); set(ax,'Ydir','normal')
  set(ax,'yticklabelmode','manual')
  set(ax,'yticklabels','')
end
set(ax,'userdata',h)
set(gcf,'CurrentAxes',h)
set(gcf,'Nextplot','Replace')

if nargout>0, handle = ax; end


