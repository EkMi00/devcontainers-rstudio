qr.householder <- function(A) {
    require(Matrix)
    R <- as.matrix(A)
    n <- ncol(A)
    m <- nrow(A)
    H <- list()
    
    if (m > n) {
        c <- n
    } else {
        c <- m
    }
    
    for (k in 1:c) {
        x <- R[k:m,k]
        e <- as.matrix(c(1, rep(0, length(x)-1)))
        vk <- sign(x[1]) * sqrt(sum(x^2)) * e + x
        
        suppressWarnings(
            h_k <- diag(length(x)) - 2 * as.vector(vk %*% t(vk)) / (t(vk) %*% vk)
        )

        if (k > 1) {
            h_k <- bdiag(diag(k-1), h_k) # Can use?
        }
        
        H[[k]] <- h_k
        
        R <- h_k %*% R
    }

    Q <- Reduce("%*%", H) 
    res <- list('Q'=Q,'R'=R)
    return(res)
}

A <- matrix(c(1,-4,2, 3,2,2), 3, 2, byrow=TRUE)
S <- qr.householder(A)
print(S)
# $Q
# 3 x 3 Matrix of class "dgeMatrix"
#            [,1]       [,2]       [,3]
# [1,] -0.3333333  0.9333333 -0.1333333
# [2,] -0.6666667 -0.3333333 -0.6666667
# [3,] -0.6666667 -0.1333333  0.7333333

# $R
# 3 x 2 Matrix of class "dgeMatrix"
#               [,1]          [,2]
# [1,] -3.000000e+00 -2.000000e+00
# [2,] -3.108624e-16 -5.000000e+00
# [3,]  4.440892e-17  8.881784e-16