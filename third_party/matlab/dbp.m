%function[output]=dbp(input)
%
% function to take dB of a power term
%
% Kevin D. LePage
% SACLANTCEN
% 2/2/98
%
% INPUTS
% 
%	input	power term
%
% OUTPUTS
%
%	output	10*log10(input)
%
function[output]=dbp(input)

if (isreal(input)&isempty(find(input<0)))

output=10*log10(input);

else

fprintf('argument is not real and positive, must not be a power term!\nbreaking\n')

return

end
