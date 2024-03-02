function [f,df,d2f] = newtonfun(x,funparms)

f   =  exp(x) - x;
df  =  exp(x) - 1;
d2f =  exp(x);

end