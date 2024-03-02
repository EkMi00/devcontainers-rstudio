function [f,df,d2f] = newtonfun2(x,funparms)

f   =  sin(x) - 0.8*x + 0.05*x^2;
df  =  cos(x) - 0.8 + 0.1*x;
d2f = -sin(x) + 0.1;

end
