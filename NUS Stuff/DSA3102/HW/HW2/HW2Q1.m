load('HW2data.mat')

[m,n] = size(Xtrain); 
% order=randperm(m);
order=1:m;
X=Xtrain(order,:);
y=ytrain(order);
Xt=Xtest;
yt=ytest;

w=zeros(n,1);    

rho = 0.9;
c = 0.6;
tol = 1e-2;

hist_obj = []; 
hist_objt= [];

iter = 0;
% for iter=1:1:m   
while true
    % Loop for alpha using backtracking 
    grad_fk = UpdatedGradLR(X, y, w);
    vk = -grad_fk;
    
    alpha = 1;
    while UpdatedObjLR(X, y, w + alpha*vk) > UpdatedObjLR(X, y, w) + c*alpha*grad_fk'*vk
        alpha = rho*alpha;
    end
    
    % Update w  
    w = w + alpha*vk;
    iter = iter + 1;

    % Update objective 
    obj = UpdatedObjLR(X, y, w); 
    objt= UpdatedObjLR(Xt, yt, w);
    hist_obj = [hist_obj; obj];
    hist_objt= [hist_objt; objt];
    if norm(grad_fk) < tol
        break;
    end
end 
w ;
plot(1:iter,hist_obj, 1:iter, hist_objt)
legend('Training','Test')
