wtm.lsum <- function(f,a,b,n) {
  h <- (b-a)/n
  x <- seq(a,b,h)
  y <- h*f(x)
  sum(y[1:n])
}

wtm.rsum <- function(f,a,b,n) {
  h <- (b-a)/n
  x <- seq(a,b,h)
  y <- h*f(x)
  sum(y[2:n+1])
}

wtm.lsum(cos,0,1,10)
wtm.rsum(cos,0,1,10)
integrate(cos,0,1)

f <- function(x){exp(-x^2)}
wtm.lsum(f,0,1,10)
wtm.rsum(f,0,1,10)
integrate(f,0,1)

g <- function(x){sqrt(1+x^2)}
wtm.lsum(g,-1,1,10)
wtm.rsum(g,-1,1,10)
integrate(g,-1,1)

#####

wtm.mid <- function(f,a,b){
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

wtm.simp38 <- function(f,a,b){
  s <- (2*a+b)/3
  t <- (a+2*b)/3
  ((b-a)/8)*(f(a) + 3*f(s) + 3*f(t) + f(b))
}

