x1 <- seq(-1,1,0.1)
x2 <- seq(-1,1,0.1)
y1 <- sqrt(1-x^2)
y2 <- -sqrt(1-x^2)

x <- c(x1,x2)
y <- c(y1,y2)
plot(y~x, xlim = c(-2.1,2.1), ylim = c(-2.1,2.1))

n <- length(x)
z <- matrix(0,nrow = n, ncol =2)
A <- matrix(c(1,2,0,2), nrow = 2, byrow = T)
for (i in 1:n){
  z[i,] <- A%*%(c(x[i],y[i]))
}
points(z)

wtm.backsub <- function(U,b){
  n <- nrow(U)
  for (j in n:2){
    if (U[j,j] == 0) {return("Error: singular")}
    else{
      b[j] <- b[j]/U[j,j]
      for (i in 1:(j-1)){
        b[i] <- b[i]-U[i,j]*b[j]
      }
    }
  }
  b[1] <- b[1]/U[1,1]
  return(b)
}

B <- matrix(rnorm(16),nrow = 4)
B[lower.tri(B)] <- 0
b <- 1:4

wtm.backsub(B,b)
solve(B,b)