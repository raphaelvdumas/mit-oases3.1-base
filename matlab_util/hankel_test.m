%function[exact,approx]=hankel_test(k,q,rough,r)
%
% Kevin D. LePage
%
% SACLANTCEN
% 2/2/98
%
% INPUTS
%
% k	incident wavenumber
% q	scattered wavenumber
% rough	roughness profile
% r	accompanying range vector
%
% OUTPUTS
%
% exact		int r' Ho(kr')Ho(qr')rough(r') dr'
% approx	rough_tilde(k+q)

function[exact,approx]=hankel_test(k,q,rough,r)

% arrange vectors as desired

q=q(:);

k=k(:)';

r=r(:)';

rough=rough(:);

% compute propagators for k+q

k_plus_q=q*ones(1,length(k))+ones(length(q),1)*k;

k_plus_q=k_plus_q(:);

k_times_q=q*k;

k_times_q=k_times_q(:);

steer=2/pi*exp(i*k_plus_q*r-i*pi/2).*((k_times_q).^(-.5)*ones(1,length(r)));

approx=steer*rough;

approx=reshape(approx,length(q),length(k));

% now do exact integral

for j=1:length(q)

fprintf('\rdoing exact, %d out of %d',j,length(q))

steer=hankel(0,k(:)*r).*(ones(length(q),1)*(r.*hankel(0,q(j)*r)));

exact(j,:)=(steer*rough).';

end

