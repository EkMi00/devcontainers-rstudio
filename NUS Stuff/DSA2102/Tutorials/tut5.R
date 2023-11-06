### Householder QR

wtm.hqr <- function(A){
  n <- ncol(A)
  m <- nrow(A)
  I <- diag(1,m)
  for (i in 1:n){
    a <- sign(A[i,i])*sqrt(sum(A[i:m,i]*A[i:m,i]))
    z <- rep(0,i-1)
    x <- c(z,A[i:m,i])
    e <- rep(0,m)
    e[i] <- 1
    v <- x - (a*e)
    b <- sum(v*v)
    if (b == 0){i <- i+1}
    else{
      for(j in i:n){
        g <- sum(v*A[,j])
        A[,j] <- A[,j] - (2*g/b)*v
        h <- sum(v*I[,j])
        I[,j] <- I[,j] - (2*h/b)*v
      }
    }
  }
  return(list(I,A))
}

wtm.hqr(A)
qr.Q(qr(A))
qr.R(qr(A))
