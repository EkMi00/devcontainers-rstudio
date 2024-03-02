function [lhs,rhs,iter] = golden(fun,a,b,tol,funparms,printyes)
%-----------------------------------------------------------------%
% Matlab implementation of the Golden Section search method       %
%-----------------------------------------------------------------%

if (nargin <= 5); printyes = 0; end
if (nargin <= 4); funparms = []; end

maxit = 1000; alpha  = (sqrt(5)-1)/2;

lhs   = a; rhs = b;
lam   = b-alpha*(b-a);  
mu    = a + alpha*(b-a);
flam  = feval(fun,lam,funparms);
fmu   = feval(fun,mu,funparms);

if printyes
   fprintf('----------------------------------------------------------------\n')
   fprintf('iter    ak      bk       lam      mu         f(lam)     f(mu)   \n')
   fprintf('----------------------------------------------------------------\n')
   fprintf('%3i  %7.4f %7.4f   %7.4f  %7.4f    %7.6f  %7.6f\n',...
      0,lhs,rhs,lam,mu,flam,fmu);
end;

for iter = 1:maxit
   if flam > fmu
      lhs   = lam;
      lam   = mu;  
      flam  = fmu;
      mu    = lhs + alpha*(rhs-lhs);
      fmu   = feval(fun,mu,funparms);
   else
      rhs   = mu;
      mu    = lam;  
      fmu   = flam;
      lam   = rhs - alpha*(rhs-lhs);
      flam  = feval(fun,lam,funparms);
   end
   
   if (printyes)
      fprintf('%3i  %7.4f %7.4f   %7.4f  %7.4f    %7.6f  %7.6f\n',...
         iter,lhs,rhs,lam,mu,flam,fmu);
   end;
   
   if (rhs-lhs) < tol;  break; end;
   
end
%-----------------------------------------------------------------%
