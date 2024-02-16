div_diff <- function(x, y) {    
    n <- length(x)
    b <- numeric(n) 
    for (k in 1:n) {
        b[k] <- y[k]
    }
    for (j in 2:n) {
        for (k in n:j) {
            b[k] <- (b[k] - b[k-1])/(x[k] - x[k-j+1])
        }
    }
    return(b)
}

x <- c(-1, 0, 1, 2)
y <- c(-10, -4 , 0, 14)

my.basis_coeff <- div_diff(x,y)
my.basis_coeff

