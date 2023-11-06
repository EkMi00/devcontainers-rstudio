####### Question 2
x <- rnorm(1000)
y <- x + rnorm(1000)
plot(y~x)
model <- lm(y~x)
abline(model, col = "red")

wtm.lmnormal <- function(x,y){
  n <- length(x)
  u <- rep(1,n)
  A <- cbind(u,x)
  M <- t(A)%*%A
  b <- t(A)%*%y
  solve(M,b)
}

z <- wtm.lmnormal(x,y)
abline(z, col = "blue")

wtm.lmqr <- function(x,y){
  n <- length(x)
  u <- rep(1,n)
  A <- cbind(u,x)
  Q <- qr.Q(qr(A))
  R <- qr.R(qr(A))
  solve(R,t(Q)%*%y)
}

z <- wtm.lmqr(x,y)
abline(z, col = "green")

wtm.lmsvd <- function(x,y){
  n <- length(x)
  u <- rep(1,n)
  A <- cbind(u,x)
  X <- svd(A)
  U <- X$u
  S <- diag(X$d)
  V <- X$v
  PA <- V%*%(solve(S)%*%t(U))
  return(PA%*%y)
}

z <- wtm.lmsvd(x,y)
abline(z, col = "violet")

####### Question 6

wtm.linint <- function(x,y){
  n <- length(x)
  plot(y~x)
  for (i in 1:(n-1)){
    lines(x[i:i+1],y[i:i+1])
  }
}

wtm.linint(c(1,2,3,4),c(-1,2,-2,3))
