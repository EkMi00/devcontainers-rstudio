t <- 1:6
f <- c(1.9,2.7,4.8,5.3,7.1,9.4)

plot(f ~ t, pch = 19)
x <- lm(f ~ t)
abline(x, col = "red")

wtm.polyreg <- function(x,y,d){
  n <- length(x)
  A <- rep(1,n)
  for (i in 1:d){
    A <- cbind(A,x^i)
  }
  solve(t(A)%*%A,t(A)%*%y)
}

z <- wtm.polyreg(t,f,5)

p2 <- function(x){z[1] + z[2]*x + z[3]*x^2}
curve(p2, add = T)

p3 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3}
curve(p3, add = T)

p4 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4}
curve(p4, add = T)

p5 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4 + z[6]*x^5}
curve(p5, add = T)

x <- 1:6
y <- x + rnorm(6,0,1)

plot(y ~ x)

z <- wtm.polyreg(x,y,5)
curve(p5, add = T)
z <- lm(y ~ x)
abline(z, col = "red")
points(cbind(x,x), pch = 19)
abline(0,1, col = "blue")

#######

t <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24)
d <- c(22, 20.5, 19, 18.5, 18, 18, 18.5, 19, 21, 23, 24, 24.5, 25, 26, 27, 28, 28, 26, 24.5, 23, 22, 22, 21.5, 21, 22)

plot(d ~ t, pch = 19)

z <- lm(d ~ t)
abline(z, col = "red")

z <- wtm.polyreg(t,d,5)
curve(p5, add = T)

plot(d ~ t, pch = 19, xlim = c(-12,36), ylim = c(0,40))
curve(p5, add = T)

wtm.genreg <- function(x,y){
  n <- length(x)
  A <- rep(1,n)
  A <- cbind(A,-sin((pi*x)/10))
  solve(t(A)%*%A,t(A)%*%y)
}

z <- wtm.genreg(t,d)

plot(d ~ t, pch = 19, ylim = c(0,28))
cx <- function(x){z[1] - z[2]*sin((pi*x)/10)}
curve(cx, add = T)

#######

t <- 1:7
p <- c(1.7,2.11,2.53,3.14,4.14,5.18,5.45)
plot(p ~ t, pch = 19)

z <- lm(p ~ t)
abline(z, col = "red")

z <- wtm.polyreg(t,p,3)
curve(p3, add = T)

######

m2 <- function(x){x}
m3 <- function(x){x^2}
m4 <- function(x){x^3}
m5 <- function(x){x^4}

plot(m5, xlim = c(0,1))
curve(m4, add = T)
curve(m3, add = T)
curve(m2, add = T)
curve(m1, add = T)
abline(1,0)

l1 <- function(x){((x-.25)/(0-.25))*((x-.5)/(0-.5))*((x-.75)/(0-.75))*((x-1)/(0-1))}
l2 <- function(x){((x-0)/(.25-0))*((x-.5)/(.25-.5))*((x-.75)/(.25-.75))*((x-1)/(.25-1))}
l3 <- function(x){((x-0)/(.5-0))*((x-.25)/(.5-.25))*((x-.75)/(.5-.75))*((x-1)/(.5-1))}
l4 <- function(x){((x-0)/(.75-0))*((x-.25)/(.75-.25))*((x-.5)/(.75-.5))*((x-1)/(.75-1))}
l5 <- function(x){((x-0)/(1-0))*((x-.25)/(1-.25))*((x-.5)/(1-.5))*((x-.75)/(1-.75))}

plot(l1, xlim = c(0,1), ylim = c(-.75,1.25))
curve(l2, add = T)
curve(l3, add = T)
curve(l4, add = T)
curve(l5, add = T)

n1 <- function(x){1}
n2 <- function(x){(x-0)}
n3 <- function(x){(x-0)*(x-.5)}
n4 <- function(x){(x-0)*(x-.5)*(x-1)}
n5 <- function(x){(x-0)*(x-.5)*(x-1)*(x-1.5)}

plot(n5, xlim = c(0,2))
curve(n4, add = T)
curve(n3, add = T)
curve(n2, add = T)
abline(1,0)

#######
t <- c(-2,0,1)
y <- c(-27,-1,0)

plot(y ~ t, pch = 19)
p <- function(x){-1 + 5*x - 4*x^2}
curve(p, add = T)

