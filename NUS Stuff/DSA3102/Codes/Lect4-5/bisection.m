function [x,iter,a,b,m] = bisection(fun,a,b,tol,funparms,printyes)
%-----------------------------------------------------------------%
% Matlab implementation of the bisection search method            %
%-----------------------------------------------------------------%

if (nargin <= 5); printyes = 1; end
if (nargin <= 4); funparms = []; end

maxit = ceil(log((b-a)/(tol+eps))/log(2));

% dfa   = feval(fun,a,funparms);
% dfb   = feval(fun,b,funparms);

dfa   = feval(fun,a);
dfb   = feval(fun,b);

if (sign(dfa)*sign(dfb) > 0)
   error(' dfa, dfb must have opposite sign');
end

if (printyes)
   fprintf('   k      ak       bk        xk       df(xk)\n')
   fprintf('---------------------------------------------\n')
end


for iter = 1:maxit
   x = (a+b)/2;
 
   %dfx = feval(fun,x,funparms);
   dfx = feval(fun,x);
   if (printyes)
      fprintf('  %2.0f   %- 5.4f   %- 5.4f   %- 5.4f   %- 5.4f\n',...
         iter,a,b,x,dfx);
   end
   
   if (sign(dfx)*sign(dfb) <= 0)
      a = x; dfa = dfx;
   elseif (sign(dfx)*sign(dfa) <= 0)
      b = x; dfb = dfx;
   end
   
   if (b-a) < tol
      break;
   end
   
end
fprintf('---------------------------------------------\n')

m = 0.5*(a+b);

end
%-----------------------------------------------------------------%
