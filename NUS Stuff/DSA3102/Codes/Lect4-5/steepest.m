function [sol] = steepest(ini,func,Tol,printyes)
%NEWTON input: ini: initial value, func,the function interested, Jini, initial
%Jacobian inverse
%Output: a solution of f(x)=0 with within a tolerance level
N=1000; %Maximum iteration count
sol=ini;
n=1;
Flag=1;
val2=Inf;
val1=Inf;
[val1, Grad]=func(sol(:,1));
h=0.1;

if (printyes)
   fprintf('-------------------------------------------\n')
   fprintf(' iter      x1          x2       norm-grad  \n')
   fprintf('-------------------------------------------\n')
end


while Flag && (n<=N) 
val2=val1;
sol(:,n+1)=sol(:,n)-h*Grad;
[val1, Grad]=func(sol(:,n+1));
if abs(val2-val1)<Tol Flag=0;
end
   if (printyes)
      fprintf('  %2.0f  %10.3e  %10.3e    %3.2e\n',n,sol(1,n),sol(2,n),norm(Grad));
   end
n=n+1;
end
if Flag disp('Does not converge') 
end
end


