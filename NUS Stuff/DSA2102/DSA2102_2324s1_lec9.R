# Single variable linear regression

x <- rnorm(1000)
y <- x + rnorm(1000)
plot(y ~ x)
linreg <- lm(y ~ x)
abline(linreg, col = "red", lwd = 5)

z <- rep(1,1000)
A <- cbind(z,x)
M <- t(A)%*%A
b <- t(A)%*%y
s <- solve(M,b)
abline(s, col = "blue", lwd = 2)

# Multiple regression

x1 <- rnorm(1000)
x2 <- rnorm(1000)
y <- x1 + x2 + rnorm(1000)
lm(y ~ x1 + x2)

A <- cbind(z,x1,x2)
M <- t(A)%*%A
b <- t(A)%*%y
solve(M,b)

# Polynomial regression

y <- 4 - x - 3*x^2 + x^3 + rnorm(1000)
plot(y ~ x)
x1 <- x
x2 <- x^2
x3 <- x^3
lm(y ~ x1 + x2 + x3)


A <- cbind(z,x1,x2,x3)
M <- t(A)%*%A
b <- t(A)%*%y
solve(M,b)

# Multivariable polynomial regression

x1 <- rnorm(1000)
x2 <- rnorm(1000)
y <- 3 - x1 - x2 + x1*x2 + x2^2 + x1^2 + rnorm(1000)
x3 <- x1*x2
x4 <- x2^2
x5 <- x1^2
lm(y ~ x1+x2+x3+x4+x5)

A <- cbind(z,x1,x2,x3,x4,x5)
M <- t(A)%*%A
b <- t(A)%*%y
solve(M,b)

# Linear in the coefficients

y <- 3 + 4*x  + pi*exp(x) - exp(0)*sin(x) + rnorm(1000)
x1 <- x
x2 <- exp(x)
x3 <- sin(x)
lm(y~x1+x2+x3)

A <- cbind(z,x1,x2,x3)
M <- t(A)%*%A
b <- t(A)%*%y
solve(M,b)


#########

u <- c(1,1,1)
x <- c(160,170,180)
y <- c(60,70,75)

X <- cbind(u,x)
A <- t(X)%*%X
b <- t(X)%*%y

solve(A,b)
lm(y ~ x)
