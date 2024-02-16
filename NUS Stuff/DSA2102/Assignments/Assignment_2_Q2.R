set.seed(1234)

getBanded <- function(n, w) {
    M <- matrix(sample((-9:9)[-10], size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 

reduce <- function(A, i, upper) {
    n <- nrow(A)
    for (j in (i+1):(upper)) {
        A[j,i] <- A[j,i]/A[i,i]
        for (k in (i+1):(upper)) {
            A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
        }
    }
    return(A[,-(n+1)])
}

my.LUfactorisation <- function(A,w) {
    n <- nrow(A)
    for (i in 1:(n-w)) {
        A <- reduce(A, i, i+w)
    }
    if (w != 1) {
        for (i in (n-w+1):(n-1)) {
            A <- reduce(A, i, n)
        }
    }
    return(A)
}

testCases <- function(n) {
    w <- 1:(n-1)
    for (i in w) {
        A <- getBanded(n,i)
		b <- 1:n
        
        L <- my.LUfactorisation(A,i)
        L[upper.tri(A)] <- 0
        diag(L) <- 1
        U <- my.LUfactorisation(A,i)
        U[lower.tri(A)] <- 0

        case <- sprintf("Case: %.0f x %.0f matrix, Bandwidth w = %.0f;",n,n,i)
        print(case)
        print("LU:")
        print(L%*%U)
        print("A:")
        print(A)
    }  
}

# sink(file="./output.txt", append=T)
n <- 7
# print(testCases(n))
testCases(n)
# sink()
