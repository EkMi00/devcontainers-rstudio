function [f,df,d2f] = newtonfun(x,funparms)

f   =  exp(x) + x^2 - 2*x;
df  =  exp(x) + 2*x - 2;
d2f =  exp(x) + 2;

end