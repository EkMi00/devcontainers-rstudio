#### Solve symmetric pos def systems

wtm.cholesky <- function(A){
  n <- ncol(A)
  if (isFALSE(all.equal(t(A),A))) {return("error: not symmetric")} else {
  A[upper.tri(A)] <- 0
  for (j in 1:(n-1)){
    if (A[j,j] <= 0) {return("error: not pos def")} else {
    A[j,j] <- sqrt(A[j,j])
    for (i in (j+1):n){
      A[i,j] <- A[i,j]/A[j,j]
    }
    for (k in (j+1):n){
      for(i in k:n){
        A[i,k] <- A[i,k] - A[i,j]*A[k,j]
      }
    }
    }
  }
  }
  A[n,n] <- sqrt(A[n,n])
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

wtm.forsub <- function(L,b){
  n <- nrow(L)
  for (j in 1:(n-1)){
    if (L[j,j] == 0) {return("Error: singular")}
    else{
      b[j] <- b[j]/L[j,j]
      for (i in (j+1):n){
        b[i] <- b[i] - L[i,j]*b[j]
      }
    }
  }
  b[n] <- b[n]/L[n,n]
  return(b)
}

wtm.spdsolve <- function(A,b) {
  L <- wtm.cholesky(A)
  y <- wtm.forsub(L,b)
  x <- wtm.backsub(t(L),y)
  return(x)
}

A <- matrix(rnorm(16), nrow = 4)
B <- t(A)%*%A
b <- 1:4

wtm.spdsolve(B,b)
solve(B,b)
