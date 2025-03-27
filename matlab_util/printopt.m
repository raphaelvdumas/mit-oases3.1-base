function [pcmd,dev] = printopt
%PRINTOPT Configure local printer defaults.
%	PRINTOPT is an M-file that you or your system manager can
%	edit to indicate your default printer type and destination.
%
%	[PCMD,DEV] = PRINTOPT returns two strings, PCMD and DEV.
%	PCMD is a string containing the print command that PRINT
%	uses to spool a file to the printer. Its default is:
%
%	   Unix:      lpr -r
%	   Windows:   PRINT
%	   Macintosh: unused
%	   VMS:       PRINT/DELETE
%	   SGI:       lp
%
%	DEV is a string containing the default device option for 
%	the PRINT command. Its default is:
%
%	   Unix & VMS: -dps
%	   Windows:    -dwin
%	   Macintosh:  -dmac
%
%	See also PRINT.

%	Copyright (c) 1984-93 by the MathWorks, Inc.

% Indicate default device option here. See PRINT.M for a 
% complete list of available devices. Uncomment and/or change
% one of the following, depending upon your computer type.
% dev = '-dps';       % Unix & VAX/VMS
dev = '-dwinc';      % Windows
% dev = '-dmac';      % Macintosh

% Indicate default print command string here. Uncomment and/or
% change one of the following, depending upon your computer type.
% pcmd = 'lpr -r';       % Unix
% pcmd = 'lpr -r -Pfred' % Unix, to a printer named fred
pcmd = 'COPY /B %s LPT2:';        % Windows, parallel printer
% pcmd = 'SPR';          % Windows, serial printer 
% pcmd = 'unused';       % Macintosh
% pcmd = 'PRINT/DELETE'; % VAX/VMS
% pcmd = 'lp';           % SGI


% ----------- Do not modify anything below this line ---------------
% Try to do something reasonable for people that have not yet
% configured their default print command string.

if ~exist('pcmd')
	cname = computer;

	% For Unix
	pcmd = 'lpr -r';

	% For Windows
	if strcmp(cname(1:2),'PC'),  pcmd = 'PRINT'; return; end

	% For Macintosh
	if strcmp(cname(1:3),'MAC'), pcmd = 'prtsc'; end

	% For SGI
	if strcmp(cname(1:3),'SGI'), pcmd = 'lp'; end

	% For VAX/VMS
	if length (cname) >= 6 
	   if strcmp(cname(1:6),'VAXVMS')
		pcmd = 'PRINT/DELETE';
	   end
	end

end

if ~exist('dev')
	cname = computer;

	% For Unix and VAX/VMS
	dev = '-dps';

	% For Windows
	if strcmp(cname(1:2),'PC'),  dev = '-dwin'; return; end

	% For Macintosh
	if strcmp(cname(1:3),'MAC'), pcmd = '-dmac'; end
end
