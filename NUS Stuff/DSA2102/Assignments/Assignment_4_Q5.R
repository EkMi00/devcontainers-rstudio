# install.packages("PolynomF")
library(PolynomF)


chebychev_nodes <- function(a,b,n) {
    nodes <- sapply(0:(n-1), function(k,a, b, n) {
            (b+a)/2 + (b-a)*(cos( ((2*k + 1) * pi) / (2*n) ))/2
        }, n=n, a=a, b=b)
    return(nodes)
}

lagrange_basis <- function(x_list, k) {
    product = 1
    x <- polynomial()
    n <- length(x_list)
    k <- k+1
    for (j in 1:n) {
        if (j!=k) {
            product <- product * (x - x_list[j])/(x_list[k] - x_list[j])
        }
    }
    return(product)
}
x = c(-sqrt(3/5), 0, sqrt(3/5))

lagrange_basis(x, 0)

lagrange_poly <- function(x, y) {
    result = 0
    n <- length(x)
    for (k in 1:n) {
        result <- result +  y[k] * lagrange_basis(x, k-1)
    }
    return(round(result, 15))
}


cheby.x <- chebychev_nodes(0,2*pi,6)
cheby.y <- sapply(cheby.x, cos)
cheby.x
cheby.y

my.poly <- lagrange_poly(cheby.x, cheby.y)
my.poly

actual.poly <- round(poly_calc(cheby.x, cheby.y), 15)
actual.poly


