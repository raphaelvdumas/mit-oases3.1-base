function[hank]=hankel(alpha,arg)

hank=besselj(alpha,arg)+i*bessely(alpha,arg);
