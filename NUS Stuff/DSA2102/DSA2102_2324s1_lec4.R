wtm.matmult <- function(A,B){
  m = nrow(A)
  n = ncol(A)
  p = ncol(B)
  if (n == nrow(B)){
    C <- matrix(0, nrow = m, ncol = p)
    for (i in 1:m){
      for (j in 1:p){
        for (k in 1:n){
          C[i,j] <- C[i,j] + (A[i,k]*B[k,j])
        }
      } 
    }
    return(C)
  }
  else (return("error"))
}

A <- matrix(rnorm(10), ncol = 2)
B <- matrix(rnorm(8), nrow = 2)
A%*%B
wtm.matmult(A,B)
A%*%B - wtm.matmult(A,B)
