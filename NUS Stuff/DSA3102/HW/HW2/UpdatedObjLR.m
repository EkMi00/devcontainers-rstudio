%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Objective Function for LR 
% (vectorized) 
% 
% Inputs: 
% X(i,:)    -- ith data point 
% y         -- vector of classification results 
% w         -- normal vector to hyperplane 
% 
% Outputs: 
% f         -- function value 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
function f = UpdatedObjLR(X, y, w) 

[m,n] = size(X); 
f = 1/m*(sum(log(1+exp(-y.*(X*w))))); 
end
