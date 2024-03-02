%Golden ratio example 
% [lhs,rhs,iter] = golden('T4Q1gold',-1,2,0.001,[],1);
% 


% %Bisection example 
%[x,iter] = bisection('T4Q1bisec',-1,2,0.001,[],1);
% 
% 
% 
% %Newton's method example 
 [x,iter] = newton('T4Q1new',0.5,1e-5,[],1);
% [x,iter] = newton2('newton3fun',[1;1],1e-6,100,[],1);
% 
% %Steepest descent
% [iter]=steepest([1;1],@steepestfunc,1e-6,1);