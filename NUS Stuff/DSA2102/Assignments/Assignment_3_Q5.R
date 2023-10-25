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
            h_k <- bdiag(diag(k-1), h_k)
        }
        H[[k]] <- h_k
        R <- h_k %*% R
    }

    Q <- Reduce("%*%", H) 
    QR <- Q%*%R
    I <- t(Q) %*% Q
    deci <- 14
    res <- list('A' = A,'Q'=round(Q, deci),'R'=round(R, deci), 
        'a) t(Q) %*% R' = round(I, deci),
        'b) Q%*%R'=round(QR, deci))
    return(res)
}

set.seed(1234)
n <- 4
m <- 3
A <- matrix(sample((-9:9), size = n*m, replace=TRUE), n, m, byrow=TRUE)

S <- qr.householder(A)
print(S)