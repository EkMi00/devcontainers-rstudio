set.seed(123)

lu.decomposition <- function(A, w) {
    n <- nrow(A)
    L <- matrix(0,n,n)
    U <- matrix(0,n,n)
    diag(L) <- rep(1,n)
    for (i in 1:n) {
        for (j in 1:min(i+w, n)) {
            U[i,j] <- A[i,j]
            if ((i-1)>0) {
                for ( k in 1:(i-1) ) {
                    U[i,j] <- U[i,j] - L[i,k] * U[k,j]
                }
            }
        }
        if ((i+1)<=n) {
            for (j in (i+1):min(i+w, n)) {
                L[j,i] <- A[j,i]
                if (U[i,i] == 0 | is.na(U[j,j]) | is.nan(U[j,j]))
                    return("Error: singular")
                if ((i-1) > 0 ) {
                    for (k in 1:(i-1) ) {
                        L[j,i] <- L[j,i] - L[j,k] * U[k,i]
                    }
                }
                L[j,i] <- L[j,i] / U[i,i]
            }    
        }
    }
    result <- list(L=L,U=U)
    return( result )
}

getBanded <- function(n, w) {
    M <- matrix(sample(-9:9, size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 

testCases <- function(n) {
    w <- 1:(n-1)
    for (i in w) {
        M <- getBanded(n,i)
        case <- sprintf("Case: %.0f x %.0f matrix, Bandwidth, w = %.0f",n,n,i)
        LU <- lu.decomposition(M, i)
        L <- matrix(unlist(LU[1]), n, n)
        U <- matrix(unlist(LU[2]), n, n)
        if (!is.character(LU)) {
            print(case)
            print("L")
            print(L)
            print("U")
            print(U)
            print("L%*%U")
            print(L%*%U)
            print("M")
            print(M)
        }
    }
}

print(testCases(5))