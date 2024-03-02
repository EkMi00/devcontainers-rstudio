%Golden ratio example 
%[lhs,rhs,iter] = golden('goldenfun2',0,1,0.02,[],1);
[lhs,rhs,iter] = golden('midtermtemp',-1,2,0.001,1,1);


%Bisection example 
%[x,iter] = bisection('bisectionfun',-1,1,0.02,[],1);



%Newton's method example 
%[x,iter] = newton('newtonfun',0,1e-5,[],1);
%[x,iter] = newton2('newton3fun',[1;1],1e-6,100,[],1);

%Steepest descent
%[iter]=steepest([1;1],@steepestfunc,1e-6,1);