set.seed(123)

getBanded <- function(n, w) {
    A <- matrix(sample(-99:99, size = n*n), nrow=n)
    A[abs(row(A) - col(A)) > w] = 0
    return(A)
} 
w = 3
A <- getBanded(10, w)
# print(A)

getLU<- function(A, w) {
    n <- nrow(A)
    L <- replicate(n, numeric(n))
    U <- replicate(n, numeric(n))
    U[row(U) == col(U)] = 1
    
    for (i in 1:w) { # Diagonal for i=1,...,w
        L[1,1] <- A[1,1]
        for (j in 1:i) {
            L[i,j] <- A[i,j]
            for (k in 1:j) {
                print(c(j, k))
                L[i,i] <- A[i,i] - (L[i,k] * U[k,i])
            }
        }
    }
    for (i in 1:w) { # Upper Triangular for i=1,...,w and j=i+1,...,w+1
        for (j in (i+1):(w+1)) {
            U[1, j] <- A[1,j]/L[1,1]

            U[i,j] <- (A[i,j] - (L[i,1] * U[1, j]))/L[i,i]
            for (k in (j+1):(w+1)) {  
                U[i,j] <- (U[i,j] - (L[i,k] * U[k,j]))/L[i,i]
            }
        }
    }

    for (i in (1+w):(n-w)) {
        for (j in 1:i)
        L[i,j] <- A[i,j]
        L[i,i] <- A[i,i] - (L[i,i-1] * U[i-1,i])
        # U[i,i+1] <- A[i,i+1]/L[i,i]
    }
    for (i in (n-w+1):n) {
        for (j in 1:i)
            L[i,j] <- A[i,j]
            for (k in 1:j)
            L[i,i] <- A[i,i] - (L[i,i-1] * U[i-1,i])
    }
    # print(L)
    # print(U)
    # print(A)
    # print(L %*% U)
    
    return(list(L,U))
}
factors <- getLU(A, w)
L <- matrix(factors[1])
U <- matrix(factors[2])
# product <- L %*% U
# print(product)
