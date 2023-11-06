library(PolynomF)
library(pracma)

sc <- function(x){sqrt(4 - x^2)}
t <- seq(-2,2,0.25)
f <- sc(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,2.5))
p <- poly_calc(t,f)
curve(sc, add = T)
curve(p, add = T, col = "red")

n <- length(t)
cp <- rep(0,n)
for(i in 1:n){
  cp[i] <- cos(((2*i-1)/(2*n))*pi)
}

cp <- 2*cp
fc <- sc(cp)
points(cp,fc,col = "blue")
p <- poly_calc(cp,fc)
#curve(sc, add = T)
curve(p, add = T, col = "blue")


fh <- function(x){1/(1+25*x^2)}
t <- seq(-2,2,0.25)
f <- fh(t)

plot(f ~ t, pch = 19, ylim = c(-0.5,1.5))
p <- poly_calc(t,f)
curve(fh, add = T)
curve(p, add = T, col = "red")

fc <- fh(cp)
points(cp,fc,col = "blue")
p <- poly_calc(cp,fc)
#curve(sc, add = T)
curve(p, add = T, col = "blue")


f <- sc(t)
curve(sc, xlim = c(-2,2))
z <- taylor(sc,0,4)
p4 <- function(x){z[5] + z[4]*x + z[3]*x^2 + z[2]*x^3 + z[1]*x^4}
curve(p4,add = T, col = "red")

f <- fh(t)
curve(fh, xlim = c(-2,2))
z <- taylor(fh,0,8)
p8 <- function(x){z[9] + z[8]*x + z[7]*x^2 + z[6]*x^3 + z[5]*x^4 + z[4]*x^5 + 
    z[3]*x^6 + z[2]*x^7 + z[1]*x^8}
curve(p8, add = T, col = "red")

##########

library(graphics)

t <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24)
d <- c(22, 20.5, 19, 18.5, 18, 18, 18.5, 19, 21, 23, 24, 24.5, 25, 26, 27, 28, 28, 26, 24.5, 23, 22, 22, 21.5, 21, 22)

p <- poly_calc(t,d)
curve(p, add = T)

n <- length(d)

plot(d ~ t, pch = 19)
for (i in 1:(n)){
  segments(t[i]-.5,d[i],t[i]+.5,d[i])
}

#######

plot(d ~ t, pch = 19)
for (i in 1:(n-1)){
  segments(t[i],d[i],t[i+1],d[i+1])
}

######

plot(d ~ t, pch = 19)
n <- length(t)

wtm.polyreg <- function(x,y,d){
  n <- length(x)
  A <- rep(1,n)
  for (i in 1:d){
    A <- cbind(A,x^i)
  }
  solve(t(A)%*%A,t(A)%*%y)
}

for (i in 1:(n-2)){
  z <- wtm.polyreg(t[i:(i+2)],d[i:(i+2)],2)
  p2 <- function(x){z[1] + z[2]*x + z[3]*x^2}
  curve(p2, add = T, xlim=c(t[i],t[i+2]))
}


k <- round(n/2,0)
j <- n%%2

plot(d ~ t, pch = 19)
for (i in 0:(k-j)){
  z <- wtm.polyreg(t[(2*i+1):(2*i+3)],d[(2*i+1):(2*i+3)],2)
  p2 <- function(x){z[1] + z[2]*x + z[3]*x^2}
  curve(p2, add = T, xlim = c(t[2*i+1],t[2*i+3]))
}

wtm.qsm <- function(x,y){
  A <- matrix(c(1,x[1],x[1]^2,0,0,0,
         1,x[2],x[2]^2,0,0,0,
         0,0,0,1,x[2],x[2]^2,
         0,0,0,1,x[3],x[3]^2,
         0,1,2*x[2],0,-1,-2*x[2],
         0,1,2*x[1],0,0,0),
         nrow = 6, ncol = 6, byrow = T)
  b <- c(y[1],y[2],y[2],y[3],0,0)
  solve(A,b)
}

plot(d[1:3] ~ t[1:3], pch = 19)
z <- wtm.qsm(t[1:3],d[1:3])
p2 <- function(x){z[1] + z[2]*x + z[3]*x^2}
curve(p2, add = T)
p2 <- function(x){z[4] + z[5]*x + z[6]*x^2}
curve(p2, add = T)

wtm.qsmn <- function(x,y){
  n <- length(x)
  A <- matrix(0, nrow = 3*(n-1), ncol = 3*(n-1))
  for (i in 1:(n-1)){
    A[2*i-1,(3*i-2):(3*i)] <- c(1,x[i],x[i]^2)
    A[2*i,(3*i-2):(3*i)] <- c(1,x[i+1],x[i+1]^2)
  }
  for (i in 1:(n-2)){
    A[2*n-2+i,(3*i-2):(3*i+3)] <- c(0,1,2*x[i+1],0,-1,-2*x[i+1])
  }
  A[3*(n-1),2:3] <- c(1,2*x[1])
  b <- c()
  for (i in 1:(n-1)){
    b <- c(b,y[i:(i+1)])
  }
  b <- c(b,rep(0,n-1))
  solve(A,b)
}

plot(d ~ t, pch = 19)
z <- wtm.qsmn(t,d)
for (i in 1:(n-1)){
  p2 <- function(x){z[3*i-2] + z[3*i-1]*x + z[3*i]*x^2}
  curve(p2, add = T, xlim = c(t[i],t[i+1]))
}

########

k <- round(n/3,0)
j <- n%%3

#install.packages("PolynomF")
library(PolynomF)

plot(d ~ t, pch = 19)
for (i in 0:(k-j)){
  p3 <- poly_calc(t[(3*i+1):(3*i+4)],d[(3*i+1):(3*i+4)])
  curve(p3, add = T, xlim = c(t[3*i+1],t[3*i+4]))
}

wtm.csm <- function(x,y){
  A <- matrix(c(1,x[1],x[1]^2,x[1]^3,0,0,0,0,
                1,x[2],x[2]^2,x[2]^3,0,0,0,0,
                0,0,0,0,1,x[2],x[2]^2,x[2]^3,
                0,0,0,0,1,x[3],x[3]^2,x[3]^3,
                0,1,2*x[2],2*x[2]^2,0,-1,-2*x[2],-3*x[2]^2,
                0,0,2,6*x[2],0,0,-2,-6*x[2],
                0,0,2,6*x[1],0,0,0,0,
                0,0,0,0,0,0,2,6*x[3]),
              nrow = 8, byrow = T)
  b <- c(y[1],y[2],y[2],y[3],0,0,0,0)
  solve(A,b)
}

plot(d[1:3] ~ t[1:3], pch = 19)
z <- wtm.csm(t[1:3],d[1:3])
p3 <- function(x){z[1] + z[2]*x + z[3]*x^2 + z[4]*x^3}
curve(p3, add = T, xlim = c(0,1))
p3 <- function(x){z[5] + z[6]*x + z[7]*x^2 + z[8]*x^3}
curve(p3, add = T, xlim = c(1,2))

wtm.csmn <- function(x,y){
  n <- length(x)
  A <- matrix(0, nrow = 4*(n-1), ncol = 4*(n-1))
  for (i in 1:(n-1)){
    A[2*i-1,(4*i-3):(4*i)] <- c(1,x[i],x[i]^2,x[i]^3)
    A[2*i,(4*i-3):(4*i)] <- c(1,x[i+1],x[i+1]^2,x[i+1]^3)
  }
  for (i in 1:(n-2)){
    A[2*n-2+(2*i-1),(4*i-3):(4*i+4)] <- c(0,1,2*x[i+1],3*x[i+1]^2,0,-1,-2*x[i+1],-3*x[i+1]^2)
    A[2*n-2+(2*i),(4*i-3):(4*i+4)] <- c(0,0,2,6*x[i+1],0,0,-2,-6*x[i+1])
  }
  A[4*n-5,3:4] <- c(2,6*x[1])
  A[4*n-4,(4*n-5):(4*n-4)] <- c(2,6*x[n])
  b <- c()
  for (i in 1:(n-1)){
    b <- c(b,y[i:(i+1)])
  }
  b <- c(b,rep(0,2*n-2))
  solve(A,b)
}

z <- wtm.csmn(t[1:4],d[1:4])

plot(d ~ t, pch = 19)
z <- wtm.csmn(t,d)
for (i in 1:(n-1)){
  p3 <- function(x){z[4*i-3] + z[4*i-2]*x + z[4*i-1]*x^2 + z[4*i]*x^3}
  curve(p3, add = T, xlim = c(t[i],t[i+1]))
}
lines(spline(t,d, method = "natural"), col = "red")

x <- c(0,2,4,6)
y <- c(1,3,-2,0)
z <- wtm.csmn(x,y)
plot(y ~ x, pch = 19, ylim = c(-3,4))
lines(spline(x,y,method="natural"), col = "red")
for (i in 1:3){
  p3 <- function(x){z[4*i-3] + z[4*i-2]*x + z[4*i-1]*x^2 + z[4*i]*x^3}
  curve(p3, add = T, xlim = c(x[i],x[i+1]))
}

########

plot(d ~ t, pch = 19)
lines(spline(t,d))
lines(spline(t,d, method = "natural"), col = "red")
lines(spline(t,d, method = "periodic"), col = "blue")

x <- c(0,1,3,4,6,7,9,10)
y <- c(7,5,4.5,1,.9,.8,.7,.6)
plot(y ~ x, pch = 19, ylim = c(-1,8))
lines(spline(x,y, method = "natural"))
lines(spline(x,y, method = "hyman"), col = "red")

#######

x <- c(1 , 0.9 , 0.81 , 0.729 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
       1 , 1.3 , 1.69 , 2.197 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
       0 , 0 , 0 , 0 , 1 , 1.3 , 1.69 , 2.197 , 0 , 0 , 0 , 0  ,
       0 , 0 , 0 , 0 , 1 , 1.9 , 3.61 , 6.859 , 0 , 0 , 0 , 0  ,
       0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 1.9 , 3.61 , 6.859 ,
       0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 2.1 , 4.41 , 9.261 ,
       0 , 1 , 2.6 , 5.07 , 0 , -1 , -2.6 , -5.07 , 0 , 0 , 0 , 0 ,
       0 , 0 , 0 , 0 , 0 , 1 , 3.8 , 10.83 , 0 , -1 , -3.8 , -10.83 ,
       0 , 0 , 2 , 7.8 , 0 , 0 , -2 , -7.8 , 0 , 0 , 0 , 0 ,
       0 , 0 , 0 , 0 , 0 , 0 , 2 , 11.4 , 0 , 0 , -2 , -11.4 ,
       0 , 0 , 2 , 5.4 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
       0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 2 , 12.6)

M <- matrix(x,nrow = 12, byrow = T)
y <- c(1.3,1.5,1.5,1.85,1.85,2.1,0,0,0,0,0,0)

solve(M,y)


x <- c(0.9,1.3,1.9,2.1)
y <- c(1.3,1.5,1.85,2.1)
plot(y ~ x, pch = 19)
lines(spline(x,y))

p0 <- function(x){ -0.2347*(x - 0.9)^3 + 3.788*(x-0.9) + 3.25*(1.3-x) }
p0(0.9)
p0(1.3)
curve(p0, col = "red", add = T)

p1 <- function(x){ a*(x - 1.3)^3 + b*(1.9 - x)^3 + c*(x-1.3) + d*(1.9-x)}
p1(1.3)
p1(1.9)
curve(p1, col = "red", add = T)

p2 <- function(x){ a*(x - 1.9)^3 + b*(x-1.9) + c*(2.1-x) }
p2(1.9)
p2(2.1)
curve(p2, col = "red", add = T)

