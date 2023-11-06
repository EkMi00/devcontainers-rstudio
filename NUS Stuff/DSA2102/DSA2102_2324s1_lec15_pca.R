x1 <- rnorm(100, mean = 0, sd = 1)
y1 <- rnorm(100, mean = 0, sd = 1)
z1 <- rnorm(100, mean = 0, sd = 1.5)

x2 <- rnorm(100, mean = -1, sd = 2)
y2 <- rnorm(100, mean = -1, sd = 1)
z2 <- rnorm(100, mean = 10, sd = 2) 

x3 <- rnorm(100, mean = 0, sd = 2)
y3 <- rnorm(100, mean = 3, sd = 1)
z3 <- rnorm(100, mean = 5, sd = 1) 

A <- cbind(x1,y1,z1)
B <- cbind(x2,y2,z2)
C <- cbind(x3,y3,z3)

X <- as.matrix(data.frame(rbind(A,B,C)))
X[,1] <- X[,1] - mean(X[,1])
X[,2] <- X[,2] - mean(X[,2])
X[,3] <- X[,3] - mean(X[,3])

plot(X[,c(1,2)])
plot(X[,c(1,3)])
plot(X[,c(2,3)])

Y <- svd(X)

plot(Y$d, pch = 19)

r <- 2
U <- Y$u
S <- diag(Y$d)
V <- Y$v

Z <- X%*%V
#sum(Z[,1]*Z[,2])
plot(Z[,1:2])

Z1 <- (U[,1:2])%*%(S[1:2,1:2])
#sum(Z1[,1]*Z1[,2])
plot(Z1)

#####################################

x1 <- rnorm(100, mean = 0, sd = 1)
y1 <- rnorm(100, mean = 0, sd = 1)
z1 <- rnorm(100, mean = 0, sd = 1)

x2 <- rnorm(100, mean = 6, sd = 1)
y2 <- rnorm(100, mean = 0, sd = 1)
z2 <- rnorm(100, mean = 0, sd = 1)

x3 <- rnorm(100, mean = 0, sd = 1)
y3 <- rnorm(100, mean = 6, sd = 1)
z3 <- rnorm(100, mean = 0, sd = 1)

x4 <- rnorm(100, mean = 6, sd = 1)
y4 <- rnorm(100, mean = 6, sd = 1)
z4 <- rnorm(100, mean = 0, sd = 1)

x5 <- rnorm(100, mean = 6, sd = 1)
y5 <- rnorm(100, mean = 6, sd = 1)
z5 <- rnorm(100, mean = 8, sd = 1)

A <- cbind(x1,y1,z1)
B <- cbind(x2,y2,z2)
C <- cbind(x3,y3,z3)
D <- cbind(x4,y4,z4)
E <- cbind(x5,y5,z5)

X <- rbind(A,B,C,D,E)
X[,1] <- X[,1] - mean(X[,1])
X[,2] <- X[,2] - mean(X[,2])
X[,3] <- X[,3] - mean(X[,3])

plot(X[,c(1,3)])
plot(X[,c(2,3)])
plot(X[,c(1,2)])

Y <- svd(X)

plot(Y$d, pch = 19)

r <- 2
U <- Y$u
S <- diag(Y$d)
V <- Y$v

Z <- X%*%V
#sum(Z[,1]*Z[,2])
plot(Z[,1:2])

##################################

x <- rnorm(200,0,10)
x <- x - mean(x)
y <- x + rnorm(200,0,10) 
y <- y - mean(y)
plot(y~x, pch = 18, cex = 1, col = "blue")
model <- lm(y~x) #fit a linear model
abline(model, col = "red", lwd = 3)
M <- matrix(cbind(x,y),ncol = 2)
N <- svd(M)
NU <- N$u[,1]
NS <- diag(N$d[1])
NV <- t(N$v[,1])
NP <- NU %*% t(NU)
NQ <- NP %*% M
lines(NQ, lwd = 3, col = "darkorange")

#######

X <- as.matrix(iris[,1:4])
#X <- as.matrix(mtcars)
#X <- as.matrix(LifeCycleSavings)

for (i in 1:ncol(X)){
  X[,i] <- X[,i] - mean(X[,i])
}

Y <- svd(X)

plot(Y$d, pch = 19)

r <- 2
U <- Y$u
S <- diag(Y$d)
V <- Y$v

Z <- X%*%V
#sum(Z[,1]*Z[,2])
plot(Z[,1:2])

######

plot(Z[,1:2],col=iris$Species)
#L <- Z[Z[,1] >0 & Z[,2] >0,]
#L

#######
install.packages("expm")
library(expm)

S <- matrix(c(95,1,1,15), nrow = 2)
Z <- matrix(rnorm(200), ncol = 2)
S2 <- sqrtm(S)
X <- t((S2%*%t(Z)))
cov(X)

plot(X[,2] ~ X[,1])

plot(X[,2] ~ X[,1], xlim = c(-35,35), ylim = c(-35,35))
eigen(cov(X))

S <- matrix(c(50,40,40,50), nrow = 2)
Z <- matrix(rnorm(200), ncol = 2)
S2 <- sqrtm(S)
X <- t((S2%*%t(Z)))
cov(X)

plot(X[,2] ~ X[,1], xlim = c(-35,35), ylim = c(-35,35))
eigen(cov(X))

########

S <- matrix(c(91.43,171.92,297.99,171.92,373.92,545.21,297.99,545.21,1297.26),nrow = 3, byrow = T)
Z <- matrix(rnorm(300),ncol=3)
S2 <- sqrtm(S)
X <- t((S2%*%t(Z)))

cov(X)
S

Y <- svd(X)

plot(Y$d, pch = 19)

r <- 2
U <- Y$u
S <- diag(Y$d)
V <- Y$v

Z <- X%*%V
sum(Z[,1]*Z[,2])
plot(Z[,1:2], xlim = c(-110,100), ylim = c(-110,110))
V
