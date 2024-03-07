% syms x y;
% f(x, y) = 100*(y - x^2)^2 + (1 - x)^2;
% grad_f = gradient(f, [x,y]);
% H_f = hessian(f, [x,y]);

f = @(x,y) 100*(y - x.^2).^2 + (1 - x).^2;
grad_f = @(x,y) [400*x.^3 - 400*x.*y + 2*x - 2; 200*(y - x.^2)];
H_f = @(x,y) [1200*x.^2-400*y+2 -400*x;-400*x 200];

%################################################################%

% For x0 = [1.2;1.2]
x_path = newton_line(f, grad_f, H_f, [1.2; 1.2]);
figure
x = linspace(-2,2,100);
y = linspace(-1,3,100);
[X,Y] = meshgrid(x,y);
Z = f(X,Y);
contour(X,Y,Z,logspace(-1,3,20));
hold on;
plot(x_path(1,1:end-1), x_path(2,1:end-1), '-o');
hold on;
plot(x_path(1,end), x_path(2,end), '-x','MarkerSize', 10,'LineWidth', 2.5);
hold off;

title(['Contour, Path of Iterates, (Newton+Line Search), x0 = [1.2;1.2]']);
xlabel('x');
ylabel('y');
legend('Contour', 'Path of Iterates');


% For x0 = [1.2;1.2]
x_path = descent_line(f, grad_f, [1.2; 1.2]);
figure
contour(X,Y,Z,logspace(-1,3,20));
hold on;
plot(x_path(1,1:end-1), x_path(2,1:end-1), '-o');
hold on;
plot(x_path(1,end), x_path(2,end), '-x','MarkerSize', 10,'LineWidth', 2.5);
hold off;

title('Contour, Path of Iterates, (Gradient Descent+Line Search), x0 = [1.2;1.2]');
xlabel('x');
ylabel('y');
legend('Contour', 'Path of Iterates');




% For x0 = [-1.2; 1.0]
x_path = newton_line(f, grad_f, H_f, [-1.2; 1.0]);
figure
x = linspace(-2,2,100);
y = linspace(-1,3,100);
[X,Y] = meshgrid(x,y);
Z = f(X,Y);
contour(X,Y,Z,logspace(-1,3,20));
hold on;
plot(x_path(1,1:end-1), x_path(2,1:end-1), '-o');
hold on;
plot(x_path(1,end), x_path(2,end), '-x','MarkerSize', 10,'LineWidth', 2.5);
hold off;

title('Contour, Path of Iterates, (Newton+Line Search), x0 = [-1.2;1.0]');
xlabel('x');
ylabel('y');
legend('Contour', 'Path of Iterates');

% For x0 = [-1.2; 1.0]
x_path = descent_line(f, grad_f, [-1.2; 1.0]);
figure
contour(X,Y,Z,logspace(-1,3,20));
hold on;
plot(x_path(1,1:end-1), x_path(2,1:end-1), '-o');
hold on;
plot(x_path(1,end), x_path(2,end), '-x','MarkerSize', 10,'LineWidth', 2.5);
hold off;

title('Contour, Path of Iterates, (Gradient Descent+Line Search), x0 = [-1.2;1.0]');
xlabel('x');
ylabel('y');
legend('Contour', 'Path of Iterates');


%################################################################%
% Newton+Line Search
function x_path = newton_line(f, grad_f, H_f, x0) 
    rho = 0.9;
    c = 0.6;
    k=0;
    tol = 1e-6;
    x_path = x0;
    while true
        % Step 1: Find vk
        grad_fk = grad_f(x0(1), x0(2));
        H_fk_inv = inv(H_f(x0(1), x0(2)));
        vk = -H_fk_inv*grad_fk;
        % Step 2: Find t_k = alpha
        alpha = 1;
        while f(x0(1) + alpha*vk(1), x0(2) + alpha*vk(2)) > f(x0(1), x0(2)) + c*alpha*grad_fk'*vk
            alpha = rho*alpha;
        end
        % Step 3: Find x^{k+1}
        x0 = x0 + alpha*vk;
        k = k + 1;
        x_path = [x_path x0];
        if norm(grad_fk) < tol
            break;
        end
    end
end


% Gradient Descent+Line Search
function x_path = descent_line(f, grad_f, x0) 
    rho = 0.9;
    c = 0.6;
    k=0;
    tol = 1e-6;
    x_path = x0;
    while true
        % Step 1: Find vk
        grad_fk = grad_f(x0(1), x0(2));
        vk = -grad_fk;
        % Step 2: Find t_k = alpha
        alpha = 1;
        while f(x0(1) + alpha*vk(1), x0(2) + alpha*vk(2)) > f(x0(1), x0(2)) + c*alpha*grad_fk'*vk
            alpha = rho*alpha;
        end
        % Step 3: Find x^{k+1}
        x0 = x0 + alpha*vk;
        k = k + 1;
        x_path = [x_path x0];
        if norm(grad_fk) < tol
            break;
        end
    end
end