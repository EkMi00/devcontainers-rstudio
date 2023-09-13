# Write a function that takes a number as an input and returns its square

sq <- function(x) {x^2}
sq(3)
sq(-pi)

# Write a for loop to compute the sum of the first n positive integers

n <- 10
s <- 0
for (i in 1:n){
  s <- s + i
}
s

# Write a function that takes in two vectors and computes their dot product

dot <- function(x,y) {sum(x*y)}

x <- c(1,-2,3)
y <- c(1,1,1)

dot(x,y)
dot(pi,x)
dot("a",x)

dot <- function(x,y){
  if (is.numeric(x) & is.numeric(y) & length(x) == length(y)){
    n = length(x)
    result <- 0
    for (i in 1:n){
      result <- result + x[i]*y[i]
    }
    return(result)
  }
  else { return("error") }
}

dot(x,y)
dot(pi,x)
dot("a",x)
