wtm.elimination <- function(A,b){
  A <- cbind(A,b)
  n <- nrow(A)
  for (i in 1:(n-1)){
    m <- i
    for (k in (i+1):n){
      if (abs(A[k,i]) > abs(A[m,i])){m <- k}
    }
    if (abs(A[m,i]) < 0.00000001){return("Error: matrix is computationally singular")}
    else if (m != i) {
      x <- A[i,]
      A[i,] <- A[m,]
      A[m,] <- x
    }
    for (j in (i+1):n){
      a <- A[j,i]/A[i,i]
      for (k in (i+1):(n+1)){
        A[j,k] <- A[j,k] - (a*A[i,k])
      }
    }
  }
  return(A)
}

wtm.backsub <- function(U,b){
  n <- nrow(U)
  for (j in n:2){
    if (U[j,j] == 0) {return("Error: singular")}
    else{
      b[j] <- b[j]/U[j,j]
      for (i in 1:(j-1)){
        b[i] <- b[i]-U[i,j]*b[j]
      }
    }
  }
  b[1] <- b[1]/U[1,1]
  return(b)
}

wtm.solve <- function(A,b){
  n <- nrow(A)
  U <- (wtm.elimination(A,b))[,1:n]
  u <- (wtm.elimination(A,b))[,n+1]
  wtm.backsub(U,u)
}

A <- matrix(rnorm(25), nrow = 5)
b <- 1:5
solve(A,b)
wtm.solve(A,b)

wtm.lu <- function(A){
  n <- nrow(A)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      A[j,i] <- A[j,i]/A[i,i]
      for (k in (i+1):n){
        A[j,k] <- A[j,k] - A[j,i]*A[i,k]
      }
    }
  }
  L <- diag(1,n)
  L[lower.tri(L)]<-A[lower.tri(A)]
  U <- A
  U[lower.tri(U)] <- 0
  return(list(L,U))
}

A <- matrix(rnorm(25), nrow = 5)
wtm.lu(A)
L <- wtm.lu(A)[[1]]
U <- wtm.lu(A)[[2]]
A - L%*%U

# WARNING! the built-in lu function will automatically perform pivoting.
# You should get the same entries, but they will likely be in different
# positions.

library(Matrix)
luA <- lu(A)
elu <- expand(luA)
elu$L
elu$U
