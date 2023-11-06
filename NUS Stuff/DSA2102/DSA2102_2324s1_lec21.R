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

wtm.comp.mp <- function(f,a,b,k){
  h <- (b-a)/k
  x <- seq(a,b,h)
  I <- 0
  for (i in 1:k){
    I <- I + f((x[i] + x[i+1])/2)
  }
  h*I
}

wtm.comp.trap <- function(f,a,b,k){
  h <- (b-a)/k
  x <- seq(a,b,h)
  I <- f(a)+f(b) + 2*sum(f(x[-c(1,length(x))])) 
  (h/2)*I
}

wtm.comp.simp <- function(f,a,b,k){
  h <- (b-a)/(2*k)
  x <- seq(a,b,h)
  y <- f(x)
  w <- c(1,rep(c(4,2),k-1),4,1)
  (h/3)*sum(y*w)
}
f <- sin
g <- cos

wtm.mp(f,0,pi)
wtm.mp(g,0,pi)

wtm.comp.mp(f,0,pi,2)
wtm.comp.mp(g,0,pi,2)

wtm.trap(f,0,pi)
wtm.trap(g,0,pi)

wtm.comp.trap(f,0,pi,2)
wtm.comp.trap(g,0,pi,2)

wtm.simp(f,0,pi)
wtm.simp(g,0,pi)

wtm.comp.simp(f,0,pi,2)
wtm.comp.simp(g,0,pi,2)
