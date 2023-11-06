# Code for question 1
wtm.LUmult <- function(L,U){
  n <- ncol(L)
  A <- matrix(0,nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      m <- min(i,j)
      r <- 0
      for (k in 1:m){
        r <- r + (L[i,k]*U[k,j])
      }
      A[i,j] <- r
    }
  }
  return(A)
}




