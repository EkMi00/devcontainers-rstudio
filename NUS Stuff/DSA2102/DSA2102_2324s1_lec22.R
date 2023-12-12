wtm.mp <- function(f,a,b){
  m <- (a+b)/2
  (b-a)*f(m)
}

wtm.trap <- function(f,a,b){
  ((b-a)/2)*(f(a) + f(b))
}

wtm.simp <- function(f,a,b){
  m <- (a+b)/2
  ((b-a)/6)*(f(a) + 4*f(m) + f(b))
}

wtm.gquad <- function(f){
  f(-1/sqrt(3)) + f(1/sqrt(3))
}

wtm.mp(cos,-1,1)
wtm.trap(cos,-1,1)
wtm.simp(cos,-1,1)
wtm.gquad(cos)
integrate(cos,-1,1)

f <- function(x){exp(-(x^2))}
integrate(f,0,1)

######
f <- function(x){sin(1/x)}
plot(f)
plot(f,xlim = c(0.01,0.2))
plot(f, xlim = c(0.1,1.1))

wtm.mp(f,0.1,1.1)
wtm.trap(f,0.1,1.1)

###

wtm.mp(f,0.1,0.6)
wtm.trap(f,0.1,0.6)

wtm.mp(f,0.6,1.1)
wtm.trap(f,0.6,1.1)

###

wtm.mp(f,0.1,0.35)
wtm.trap(f,0.1,0.35)

wtm.mp(f,0.35,0.6)
wtm.trap(f,0.35,0.6)

###

wtm.mp(f,0.1,0.225)
wtm.trap(f,0.1,0.225)

wtm.mp(f,0.225,0.35)
wtm.trap(f,0.225,0.35)

wtm.mp(f,0.35,0.475)
wtm.trap(f,0.35,0.475)

wtm.mp(f,0.475,0.6)
wtm.trap(f,0.475,0.6)

#############

wtm.mci <- function(f,a,b,n){
  x <- runif(n,a,b)
  y <- f(x)
  I <- (1/n)*sum(y)
  return(I)
}

wtm.mci(sin,0,1,20)
integrate(sin,0,1)


############

install.packages("deSolve")
library(deSolve)
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}
Pars <- c(alpha = 1.1, beta = .4, gamma = .4, delta = 0.1)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Prey", "Predator"), lty = c(1,2), col = c(1,2), box.lwd = 0)

#########

x <- seq(-pi,pi,0.5)
f <- sin(x)
plot(f ~ x, pch = 19)

wtm.fordif <- function(x,y){
  n <- length(x)
  df <- c()
  for (i in 1:(n-1)){
    d <- (y[i+1] - y[i])/(x[i+1]-x[i])
    df <- c(df,d)
  }
  return(df)
}

df <- wtm.fordif(x,f)

points(x[-(length(x))],df, col = "red")
curve(cos,col = "red", add = T)

#######

fh <- function(x){1/(1+25*x^2)}
fp <- function(x){(-50*x)/((1+25*x^2)^2)}
t <- seq(-2,2,0.5)
f <- fh(t)
plot(f ~ t, pch = 19, ylim = c(-3,3))

df <- wtm.fordif(t,f)
points(t[-(length(t))],df, col = "red", pch=19)
curve(fp, add = T, col = "red")

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

n <- length(t)
z <- wtm.csmn(t,f)
for (i in 1:(n-1)){
  p3 <- function(x){z[4*i-3] + z[4*i-2]*x + z[4*i-1]*x^2 + z[4*i]*x^3}
  curve(p3, add = T, xlim = c(t[i],t[i+1]))
}

for (i in 1:(n-1)){
  p3 <- function(x){z[4*i-2] + 2*z[4*i-1]*x + 3*z[4*i]*x^2}
  curve(p3, add = T, xlim = c(t[i],t[i+1]), col = "blue")
}

#####

exp(0.5)-1
x <- 0.5
e <- exp(1)
(e-1)*exp(x) - 2*x - 1
(1-(1/e))*exp(x) - 2*x + 1
0.5*(e-(1/e))*exp(x)-2*x

wtm.fd <- function(f,x,h){
  (f(x+h) - f(x))/h
}

wtm.bd <- function(f,x,h){
  (f(x) - f(x-h))/h
}

wtm.cd <- function(f,x,h){
  (f(x+h) - f(x-h))/(2*h)
}

fx <- function(x){exp(x) - x^2}

wtm.fd(fx,0.5,0.001)
wtm.bd(fx,0.5,0.001)
wtm.cd(fx,0.5,0.001)

gx <- function(x){log(x)*exp(sin(x))}
(gx(0.501)-2*gx(0.5)+gx(0.499))/(0.000001)

gppx <- function(x){(exp(sin(x))*(-x^2*sin(x)*log(x) + x^2*(cos(x))^2*log(x) + 2*x*cos(x)-1))/(x^2)}
gppx(0.5)
