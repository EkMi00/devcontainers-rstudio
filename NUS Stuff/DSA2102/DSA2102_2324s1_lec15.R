wtm.qrit <- function(A,k){
  n <- nrow(A)
  Q <- diag(1,n)
  for (i in 1:k){
    Qi <- qr.Q(qr(A))
    Ri <- qr.R(qr(A))
    A <- Ri%*%Qi
    Q <- Q%*%Qi
    #print(A)
  }
  return(list(diag(A),Q))
}

A <- matrix(rnorm(25), nrow = 5)
S <- t(A)%*%A

wtm.qrit(S,5)
eigen(S)

H <- S
H[abs(row(H) - col(H)) > 1] <- 0
H
eigen(H)
wtm.qrit(H,5)

B <- matrix(c(2,1,1,3,2,5,4,4,1), nrow = 3, byrow = T)
B
H <- matrix(c(1,0,0,0,3/5,4/5,0,4/5,-3/5), nrow = 3)
H
M <- H%*%(B%*%H)
round(M,10)
round(qr.Q(qr(M)),10)

wtm.hess <- function(A){
  n <- nrow(A)
  for (i in 1:(n-2)){
    x <- A[(i+1):n,i]
    y <- sqrt(sum(x*x))
    v <- x - c(y,rep(0,n-(i+1)))
    #print(v)
    P <- (v%*%t(v))/(sum(v*v))
    H <- diag(1,n-i) - 2*P
    HH <- diag(1,n)
    HH[row(HH) > i & col(HH) > i] <- c(H)
    A <- HH%*%(A%*%HH)
  }
  return(A)
}

G <- matrix(rnorm(36), nrow = 6)
round(wtm.hess(G),10)
J <- round(wtm.hess(G),10)
qr.Q(qr(J))

wtm.givens <- function(A){
  n <- nrow(A)
  for (i in 1:(n-1)){
    x <- A[i,i]/sqrt(A[i,i]^2+A[i+1,i]^2)
    y <- A[i+1,i]/sqrt(A[i,i]^2+A[i+1,i]^2)
    G <- diag(1,n)
    G[i,i] <- x
    G[i,i+1] <- y
    G[i+1,i] <- -y
    G[i+1,i+1] <- x
    A <- G%*%A
  }
  return(A)
}

round(wtm.givens(J),10)

########

A <- matrix(rnorm(25), nrow = 5)

A
B <- A + t(A)
B
eigen(B)
Q <- eigen(B)$vectors
round(Q%*%t(Q),10)
round(t(Q)%*%Q,10)

C <- A%*%t(A)
C
eigen(C)
Q <- eigen(C)$vectors
round(Q%*%t(Q),10)
round(t(Q)%*%Q,10)

#####

M <- matrix(rnorm(15), nrow = 3)
svd(M)

M <- matrix(rnorm(15), nrow = 5)
svd(M)

#####

A <- matrix(c(2,1,0,-1,0,-1,1,1), nrow = 2, byrow = T)
B <- A%*%t(A)
S <- diag(sqrt(eigen(B)$values))
U <- eigen(B)$vectors
VT <- solve(S)%*%(t(U)%*%A)
svd(A)
t(VT)


