function[y]=kevsineint(x)
% KEVSINEINT (a.k.a. SININT)   Numerical Sine Integral
%   Used by KEVFIRLS in the Signal Processing Toolbox.
%   Untested for complex or imaginary inputs.
%

    i1 = find(real(x)<0);   % this equation is not valid if x is in the
                            % left-hand plane of the complex plane.
            % use relation Si(-z) = -Si(z) in this case (Eq 5.2.19, Abramowitz
            %  & Stegun).
    x(i1) = -x(i1);
    y = zeros(size(x));
    ind = find(x);
    % equation 5.2.21 Abramowitz & Stegun
    %  y(ind) = (1/(2*i))*(expint(i*x(ind)) - expint(-i*x(ind))) + pi/2;
    y(ind) = imag(expint(i*x(ind))) + pi/2;
    y(i1) = -y(i1);
