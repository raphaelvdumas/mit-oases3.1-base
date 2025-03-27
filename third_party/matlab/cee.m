%function[c]=cee(t,zt,s,zs,Z)
%
% 3/23/97
% Kevin D. LePage
% BBN Systems and Technologies
%
% Inputs:
%
% t(zt)	temperature (deg Celsius)
% zt    corresponding depths (m)
% s(zs)	salinity (ppt)
% zs 	corresponding depths (m)
% Z	desired depths (m)
%
% Outputs
%
% c	sound speed at depths Z

function[c]=cee(t,zt,s,zs,Z)

t=interp1(zt,t,Z);

s=interp1(zs,s,Z);

% pressure term

Vp=1.60272e-1*(Z/10)+1.0268e-5*(Z/10).^2+3.5216e-9*(Z/10).^3-3.3603e-12*(Z/10).^4;

% salinity term

Vs=1.39799*(s-35)+1.69202e-3*(s-35).^2;

% temperature term

Vt=4.5721*t-4.4532e-2*t.^2-2.6045e-4*t.^3+7.9851e-6*t.^4;

% salinity, temperature and pressure term

Vstp=(s-35).*(-1.1244e-2*t+7.7711e-7*t.^2+7.7016e-5*(Z/10)-1.2943e-7*(Z/10).^2+3.1580e-8*(Z/10).*t+1.5790e-9*(Z/10).*t.^2);

Vstp=Vstp+(Z/10).*(-1.8607e-4*t+7.4812e-6*t.^2+4.5283e-8*t.^3);

Vstp=Vstp+((Z/10).^2).*(-2.5294e-7*t+1.8563e-9*t.^2);

Vstp=Vstp-((Z/10).^3*1.9646e-10).*t;

c=Vs+Vt+Vp+Vstp+1449.14;

plot(Vs+1449.14,Z,'r')

hold on

plot(Vt+1449.14,Z,'g')

plot(Vp+1449.14,Z,'m')

plot(Vstp+1449.14,Z,'b')

plot(c,Z)

hold off

legend('salinity','temperature','pressure','stp','total')

