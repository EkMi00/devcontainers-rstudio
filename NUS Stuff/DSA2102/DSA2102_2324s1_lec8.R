wtm.cholesky <- function(A){
  n <- nrow(A)
  for (i in 1:(n-1)){
    A[i,i] <- sqrt(A[i,i])
    for (j in (i+1):n){
      A[j,i] <- A[j,i]/A[i,i]
    }
    for (j in (i+1):n){
      for (k in j:n){
        A[k,j] <- A[k,j] - (A[k,i]*A[j,i])
      }
    }
  }
  A[n,n] <- sqrt(A[n,n])
  return(A)
}

A <- matrix(runif(25, 1,2), nrow = 5)
M <- A%*%t(A)
B <- wtm.cholesky(M)
B[upper.tri(B)] <- 0
B
t(chol(M))

#########

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
