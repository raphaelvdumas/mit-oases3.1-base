%function[output]=dba(input)
%
% function to take dB of an amplitude
%
% Kevin D. LePage
% SACLANTCEN
% 2/2/98
%
% INPUTS
% 
%	input	amplitude term
%
% OUTPUTS
%
%	output	20*log10(abs(input))
%
function[output]=dba(input)

output=20*log10(abs(input));
