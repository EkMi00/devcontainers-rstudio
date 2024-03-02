H_f = @(x,y) [1200*x.^2-400*y+2 -400*x;-400*x 200];
minus_H_f = @(x,y) [-(1200*x.^2-400*y+2) 400*x;400*x -200];

if chol(H_f)
    disp('Matrix is symmetric positive definite.')
elseif chol(minus_H_f)
    disp('Matrix is symmetric negative definite.')
else
    disp('Matrix is not symmetric positive definite')
end