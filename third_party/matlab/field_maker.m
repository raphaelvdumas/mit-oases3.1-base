%function[field]=field_maker(freq,kr,mode,z,r_field,z_s,z_field)
%
% Kevin D. LePage
% SACLANTCEN
% 11/17/97
% 
% INPUTS
%
% freq		frequency in Hz
% kr		modal eigenvalues (complex, rad/m)	
% mode		modal eigenfunctions (length z x length kr)
% z		corresponding depths
% r_field	ranges desired for field (km)
% z_s		source depth (m)
% z_field	depths desired for field (optional)
%
% OUTPUTS
%
% field		complex pressure (length z (or z_field) x length r)

function[field]=field_maker(freq,kr,mode,z,r,z_s,z_field)

if size(mode,2)~=length(kr)

mode=mode.';

end

if size(mode,2)~=length(kr)

error('inconsistency between size of mode and length of kr, breaking:  ')

break

end

if nargin>6

for j=1:length(kr)

mode1(:,j)=interp1(z,mode(:,j),z_field);

end

else

mode1=mode;

end

for j=1:length(kr)

mode_s(j,1)=interp1(z,mode(:,j),z_s);

end

kr=real(kr)+i*abs(imag(kr));

kr=kr(:);

r=r(:)'*1e3;

field=(mode_s*ones(1,length(r))).*(exp(i*kr*r).*(kr*r).^(-.5));

field=mode1*field;















