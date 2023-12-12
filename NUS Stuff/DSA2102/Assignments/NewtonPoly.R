# install.packages("PolynomF")
library(PolynomF)

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

newton_basis <- function(x_list, k) {
    x <- polynom()
    product <- 1
    if (k == 0) { return(1) }
    for (i in 1:k) {
        product <- product * (x - x_list[i])
    }
    return(product)
}

newton_poly <- function(x,y) {
    n <- length(x)
    diff <- div_diff(x,y)
    result <- 0
    for (i in 1:n) {
        result <- result + diff[i] * newton_basis(x,i-1)
    }
    return(result)
}

# x <- c(-1, 0, 1, 2)
# y <- c(-10, -4 , 0, 14)
x <- c(0,2,4,6)
y <- c(1,3,-2,0)

my.poly <- newton_poly(x,y)
my.basis_coeff <- div_diff(x,y)
my.coeff <- coef(my.poly)
print(my.poly)
print(my.basis_coeff)
print(my.coeff)

actual.poly <- poly_calc(x,y)
actual.coeff <- coef(actual.poly)
print(actual.poly)
print(actual.coeff)

# Random Test
set.seed(1234)
test <- function(n) {
    x <- sort(sample(-10:10,n) )
    print(x)
    y <- sample(-10:10,n, replace=TRUE)
    print(y)
    my.poly <- newton_poly(x,y)
    actual.poly <- poly_calc(x,y)
    return(list("My Poly"=my.poly, "Actual Poly"=actual.poly))
}

# print(test(5)) # can take inputs up to and including 21

x <- c(-1,0,1)
y <- c(2,3,6)
print(newton_poly(x,y))