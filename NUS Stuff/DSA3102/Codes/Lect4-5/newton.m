function  [x,iter] = newton(fun,x0,tol,funparms,printyes)

if (nargin < 5); printyes = 0; end
if (nargin < 4); funparms = []; end

if (printyes)
   fprintf('\n iter      xk        df(xk)       ddf(xk)\n')
   fprintf('------------------------------------------\n')
end
x = x0;
maxit = 50;
for iter = 0:maxit
   [f,df,d2f] = feval(fun,x,funparms);
   if (printyes)
      fprintf('  %2i   %8.4f   %9.4f    %- 5.2e\n',iter,x,df,d2f);
   end
   if (abs(df) < tol); break; end;
   x = x - df/d2f;
end

