syms x y z
f(x,y,z) = log(x^2+1) + x^2 - 2*x*y + 4*y^2 + z^4 - 8*z^3 + 16*z^2;
v = [x,y,z];
gradient(f,v)
soln = struct2table(solve(gradient(f,v)==0))

H_f = hessian(f,v)

s1 = H_f(0, 0, 0)
eig(s1)

s2 = H_f(0, 0, 2)
eig(s2)

s3 = H_f(0, 0, 4)
eig(s3)

f(0,0,0)
f(0,0,2)
f(0,0,4)
