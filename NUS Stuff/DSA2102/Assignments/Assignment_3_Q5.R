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
        v_k <- as.matrix(c(sqrt(sum(x^2)), rep(0, length(x)-1))) - x
        suppressWarnings(
            h_k <- diag(length(x)) - 2 * as.vector(v_k %*% t(v_k)) / (t(v_k) %*% v_k)
        )
        if (k > 1) {
            h_k <- bdiag(diag(k-1), h_k)
        }
        H[[k]] <- h_k
        R <- h_k %*% R
    }

    Q <- Reduce("%*%", H) 
    QR <- Q%*%R
    I <- t(Q) %*% Q
    place <- 14
    res <- list('Q'=round(Q, place),'R'=round(R, place), 
        'a) t(Q) %*% Q' = round(I, place),
        'b) A' = A, 
        'Q %*% R'=round(QR, place))
    return(res)
}

set.seed(1234)
n <- 4
m <- 3
A <- matrix(sample((-9:9), size = n*m, replace=TRUE), n, m, byrow=TRUE)

S <- qr.householder(A)
print(S)