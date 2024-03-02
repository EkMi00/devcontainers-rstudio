function [f,df] = steepestfunc(x,funparms)
f   =  exp(norm(x).^2)-norm(x).^2/2;
df  =  [2*x(1)*exp(norm(x).^2)-x(1);2*x(2)*exp(norm(x).^2)-x(2)];

end