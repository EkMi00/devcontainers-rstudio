wtm.polyreg <- function(x,y,d){
  n <- length(x)
  A <- rep(1,n)
  for (i in 1:d){
    A <- cbind(A,x^i)
  }
  solve(t(A)%*%A,t(A)%*%y)
}

t <- 1:6
f <- exp(t)
plot(f ~ t, pch = 19)

z <- wtm.polyreg(t,f,5)

p5 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4 + z[6]*x^5}

curve(p5, add = T)
curve(exp, col = "red", add = T)

plot(f ~ t, pch = 19, xlim = c(1,10), ylim = c(0,exp(1)^10))
curve(p5, add = T)
curve(exp, col = "red", add = T)

plot(f ~ t, pch = 19, xlim = c(-2,6), ylim = c(-100,400))
curve(p5, add = T)
curve(exp, col = "red", add = T)

t <- 1:10
f <- exp(t)
plot(f ~ t, pch = 19)

z <- wtm.polyreg(t,f,9)

######

sc <- function(x){sqrt(4 - x^2)}
t <- seq(-2,2,1)
f <- sc(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,2.5))

z <- wtm.polyreg(t,f,4)
p4 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4}
curve(sc, add = T)
curve(p4, add = T, col = "red")

t <- seq(-2,2,0.5)
f <- sc(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,2.5))

z <- wtm.polyreg(t,f,8)
p8 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4}
curve(sc, add = T)
curve(p4, add = T, col = "red")

install.packages("PolynomF")
library(PolynomF)

t <- seq(-2,2,0.25)
f <- sc(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,2.5))
p <- poly_calc(t,f)
curve(sc, add = T)
curve(p, add = T, col = "red")

t <- seq(-2,2,0.1)
f <- sc(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,2.5))
p <- poly_calc(t,f)
curve(sc, add = T)
curve(p, add = T, col = "red")

#####

fh <- function(x){1/(1+25*x^2)}
t <- seq(-2,2,1)
f <- fh(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,1.5))

z <- wtm.polyreg(t,f,4)
p4 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4}
curve(fh, add = T)
curve(p4, add = T, col = "red")

t <- seq(-2,2,0.5)
f <- fh(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,1.5))

z <- wtm.polyreg(t,f,8)
p8 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3 + z[5]*x^4 + z[6]*x^5 + 
    z[7]*x^6 + z[8]*x^7 + z[9]*x^8}
curve(fh, add = T)
curve(p8, add = T, col = "red")

t <- seq(-2,2,0.25)
f <- fh(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,1.5))
p <- poly_calc(t,f)
curve(fh, add = T)
curve(p, add = T, col = "red")

t <- seq(-2,2,0.1)
f <- fh(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,1.5))
p <- poly_calc(t,f)
curve(fh, add = T)
curve(p, add = T, col = "red")


