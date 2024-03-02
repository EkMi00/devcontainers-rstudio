function [f,df,d2f] = T5new(x,funparms)

f   =  x^4 - 4*x;
df  =   4*x^3- 4;
d2f =  12*x^2;

end