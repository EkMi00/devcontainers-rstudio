wtm.cgs <- function(A){
  n <- ncol(A)
  m <- nrow(A)
  R <- matrix(0, nrow = n, ncol = n)
  R[1,1] <- sqrt(sum(A[,1]*A[,1]))
  A[,1] <- A[,1]/R[1,1]
  for (i in 2:n){
    for (j in 1:(i-1)){
      R[j,i] <- sum(A[,i]*A[,j])
    }
    for (j in 1:(i-1)){
      A[,i] <- A[,i] - R[j,i]*A[,j]
    }
    R[i,i] <- sqrt(sum(A[,i]*A[,i]))
    A[,i] <- A[,i]/R[i,i]
  }
  return(list(A,R))
}

A <- matrix(rnorm(12), nrow = 4)

qrA <- qr(A)
qr.Q(qrA)
qr.R(qrA)

wtmqr <- wtm.cgs(A)
wtmqr[[1]]
wtmqr[[2]]

a <- 5

H <- matrix(0,nrow = a, ncol = a)
for (i in 1:a){
  for (j in 1:a){
    H[i,j] <- 1/(i+j-1)
  }
}
I <- diag(1,a)
H <- H + (0.00001*I)

qrH <- qr(H)
Q <- qr.Q(qrH)
max(abs(I - t(Q)%*%Q))

wtmqr <- wtm.cgs(H)
Q <- wtmqr[[1]]
max(abs(I - t(Q)%*%Q))

wtm.mgs <- function(A){
  n <- ncol(A)
  m <- nrow(A)
  R <- matrix(0, nrow = n, ncol = n)
  R[1,1] <- sqrt(sum(A[,1]*A[,1]))
  A[,1] <- A[,1]/R[1,1]
  for (i in 2:n){
    for (j in 1:(i-1)){
      R[j,i] <- sum(A[,i]*A[,j])
      A[,i] <- A[,i] - R[j,i]*A[,j]
    }
    R[i,i] <- sqrt(sum(A[,i]*A[,i]))
    A[,i] <- A[,i]/R[i,i]
  }
  return(list(A,R))
}


A <- matrix(rnorm(12), nrow = 4)

qrA <- qr(A)
qr.Q(qrA)
qr.R(qrA)

wtmqr <- wtm.mgs(A)
wtmqr[[1]]
wtmqr[[2]]

qrH <- qr(H)
Q <- qr.Q(qrH)
max(abs(I - t(Q)%*%Q))

wtmqr <- wtm.mgs(H)
Q <- wtmqr[[1]]
max(abs(I - t(Q)%*%Q))
