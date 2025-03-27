%[linear]=undb(db)
%
% function to raise 10 to the power of db/20
%
% Kevin D. LePage
% SACLANTCEN
% 17/4/98
%
% INPUTS:
%
% db		some power expressed in dB 
%
% OUPUTS:
%
% linear	the same power in linear land

function[linear]=undb(db)

if isreal(db)

linear=10.^(db/20);

else

fprintf('Supposed dB quantity is not real, breaking\n')

break

end
