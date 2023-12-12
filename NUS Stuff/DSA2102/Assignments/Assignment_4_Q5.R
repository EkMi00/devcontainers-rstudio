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

print(lagrange_basis(x, 0))



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
print("Chebyshev nodes:")
# 6.1761381 5.3630341 3.9546967 2.3284886 0.9201512 0.1070472
print(cheby.x)
print("f(x_k):")
# 0.9942759  0.6056999 -0.6872469 -0.6872469  0.6056999  0.9942759
print(cheby.y)

my.poly <- lagrange_poly(cheby.x, cheby.y)
print(my.poly)
# 0.9736137 + 0.2992113*x - 1.025028*x^2 + 0.3111183*x^3 - 0.02475801*x^4 - 2.602085e-18*x^5

actual.poly <- round(poly_calc(cheby.x, cheby.y), 15)
print(actual.poly)
# 0.9736137 + 0.2992113*x - 1.025028*x^2 + 0.3111183*x^3 - 0.02475801*x^4

