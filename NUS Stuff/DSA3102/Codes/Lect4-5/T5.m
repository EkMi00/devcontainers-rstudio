%Golden ratio example 
[lhs,rhs,iter] = golden('T5gold',0,5,0.001,[],1);
% 


% %Bisection example 
[x,iter] = bisection('T5bisec',0,5,0.001,[],1);
% 
% 
% 
% %Newton's method example 
 [x,iter] = newton('T5new',2,1e-5,[],1);
% [x,iter] = newton2('newton3fun',[1;1],1e-6,100,[],1);
% 
% %Steepest descent
% [iter]=steepest([1;1],@steepestfunc,1e-6,1);