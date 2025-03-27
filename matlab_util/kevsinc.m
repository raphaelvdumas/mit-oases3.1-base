function y=kevsinc(x)
%KEVSINC Sin(pi*x)/(pi*x) function.
%   KEVSINC(X) returns a matrix whose elements are the sinc of the elements 
%   of X, i.e.
%        y = sin(pi*x)/(pi*x)    if x ~= 0
%          = 1                   if x == 0
%   where x is an element of the input matrix and y is the resultant
%   output element.  

y=ones(size(x));
i=find(x);
y(i)=sin(pi*x(i))./(pi*x(i));
