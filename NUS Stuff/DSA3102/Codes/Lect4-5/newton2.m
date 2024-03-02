%-----------------------------------------------------------------%
% Matlab implementation of the Newton search method               %
%-----------------------------------------------------------------%
function [x,iter] = newton2(fun,x0,tol,maxit,funparms,printyes)

if (nargin <= 5); printyes = 1; end
if (nargin <= 4); funparms = []; end
if (nargin <= 3); maxit = 100; end

x  = x0(:);
hist=x;
histy=[];

if (printyes)
   fprintf('-------------------------------------------\n')
   fprintf(' iter      x1          x2       norm-grad  \n')
   fprintf('-------------------------------------------\n')
end

for iter = 0:maxit
   [f,df,d2f] = feval(fun,x,funparms);
   normgrad = norm(df);
   if (printyes)
      fprintf('  %2.0f  %10.3e  %10.3e    %3.2e\n',iter,x(1),x(2),normgrad);
   end
   
   if normgrad < tol; break; end;
   x = x - (d2f \ df);
   hist=[hist,x];
   histy=[histy,feval(fun,x,funparms)];
end
histd=histy;
for i=1:iter
    histd(i)=norm(hist(:,i)-hist(:,iter));
end


figure 
plot(hist(1,:),hist(2,:),'-*')
xlim([-1,2])
ylim([-1,2])
title('Trajectory')
figure
plot(1:iter,histd)
title('Distance to solution')

figure
plot(1:iter,histy)
title('Function value')



end
%-----------------------------------------------------------------%
