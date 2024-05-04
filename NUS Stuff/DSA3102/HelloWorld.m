% syms x y z a b c
% f(x, y) = 100*(y-x^2)^2 + (1-x)^2
% grad_f = gradient(f, [x,y])
% H_f = hessian(f, [x,y])
% 
% f(x, y, z) = x*y*z
% gradient(f, [x,y,z])
% hessian(f, [x,y,z])
% a = hessian(f, [x,y,z])
% det([0 z; z 0])
% 
% syms X w y
% 
% loss = log(exp(-y*w*X) + 1)
% 
% gradient(loss, [w])
% 
% hessian(loss,[w])
% beep off
% syms f(X,y,w)
syms X y w
% w = sym('w', [1, 3]);
% X = sym('X', [1, 3]);
% f(w, X) = w.*X;
% log_loss(X, y, w) = log(exp(-y*dot(w, X)) + 1); 
log_loss(X, y, w) = log(exp(-y*w*X) + 1)
grad_f = gradient(log_loss, [w])
H_f = hessian(log_loss, w)
syms x y z a b c
f(x,y,z) = x*y*z
H_v = -hessian(f, [x,y,z])
eig(-hessian(f, [x,y,z]))
% v = [a;b;c]
% v.'*hessian(f, [x,y,z])*v
g2(x, y, z) = x*y + x*z+ y*z - 8
H_g = hessian(g2, [x,y,z])

eig(H_v(1,2,2) + H_g)
eig(H_v(2,1,2) + H_g)
eig(H_v(2,2,1) + H_g)