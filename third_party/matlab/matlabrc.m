%MATLABRC Master startup M-file.
%
%   MATLABRC is automatically executed by MATLAB during startup.
%   It establishes the MATLAB path, sets the default figure size,
%   and sets a few uicontrol defaults.
%
%   MATLABRC also invokes a STARTUP command if the file 'startup.m'
%   exists on the MATLAB path.

%   Copyright (c) 1984-93 by the MathWorks, Inc.

% Setup the MATLAB search path.

dos('net use lpt2:\\pcserv\prerdcol')

matlabpath([...
'C:\MATLAB;',...
'C:\MATLAB\toolbox\matlab\general;',...
'C:\MATLAB\toolbox\matlab\ops;',...
'C:\MATLAB\toolbox\matlab\lang;',...
'C:\MATLAB\toolbox\matlab\elmat;',...
'C:\MATLAB\toolbox\matlab\specmat;',...
'C:\MATLAB\toolbox\matlab\elfun;',...
'C:\MATLAB\toolbox\matlab\specfun;',...
'C:\MATLAB\toolbox\matlab\matfun;',...
'C:\MATLAB\toolbox\matlab\datafun;',...
'C:\MATLAB\toolbox\matlab\polyfun;',...
'C:\MATLAB\toolbox\matlab\funfun;',...
'C:\MATLAB\toolbox\matlab\sparfun;',...
'C:\MATLAB\toolbox\matlab\plotxy;',...
'C:\MATLAB\toolbox\matlab\plotxyz;',...
'C:\MATLAB\toolbox\matlab\graphics;',...
'C:\MATLAB\toolbox\matlab\color;',...
'C:\MATLAB\toolbox\matlab\sounds;',...
'C:\MATLAB\toolbox\matlab\strfun;',...
'C:\MATLAB\toolbox\matlab\iofun;',...
'C:\MATLAB\toolbox\matlab\demos;',...
'C:\MATLAB\se\nav',...
]);

% Set the default uicontrol color.

gray = [128/255 128/255 128/255];
set(0,'defaultuicontrolbackgroundcolor',gray);

% Set the default figure position, in pixels.
% On small screens, make figure smaller, with same aspect ratio.

screen = get(0, 'ScreenSize');
width = screen(3);
height = screen(4);
if height >= 500
   mwwidth = 560; mwheight = 420;
else
   mwwidth = 500; mwheight = 375;
end
left = (width-mwwidth)/2;
bottom = height-mwheight-60;
rect = [ left bottom mwwidth mwheight ];
set(0, 'defaultfigureposition',rect);

% Clean up work space.

clear gray screen width height mwwidth mwheight left bottom rect
flops(0);

% Display helpful hints.

disp(' ')
disp('Commands to get started: intro, demo, help help')
disp('Commands for more information: help, whatsnew, info, subscribe')
disp(' ')

% Now execute startup M-file, if it exists.

if (exist('startup.m')==2)
    startup
end
