syms x y
f(x, y) = 100*(y-x^2)^2 + (1-x)^2
grad_f = gradient(f, [x,y])
H_f = hessian(f, [x,y])