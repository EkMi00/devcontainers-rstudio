function [f,df,d2f] = newton3fun(x,funparms)

f   =  exp(norm(x).^2)-norm(x).^2/2;
df  =  [2*x(1)*exp(norm(x).^2)-x(1);2*x(2)*exp(norm(x).^2)-x(2)];
d2f = [(4*x(1).^2+2)*exp(norm(x).^2)-1, 4*x(1)*x(2)*exp(norm(x).^2);
    4*x(1)*x(2)*exp(norm(x).^2),(4*x(2).^2+2)*exp(norm(x).^2)-1];

end
