 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 % Gradient of Objective Function for LR 
 % (vectorized) 
 % 
 % Inputs: 
 % X(i,:)    -- ith data point 
 % y         -- vector of classification results 
 % w         -- normal vector to hyperplane 
 % 
 % Outputs: 
 % gradf    -- gradient of function evaluated at w and b 
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
function gradf = UpdatedGradLR(X, y, w) 

[m,n] = size(X); 
gradf = zeros(n,1); 
gradf(1:n,1) = 1/m*sum((-y.*X./(exp(y.*(X*w))+1)))'; 
 
end