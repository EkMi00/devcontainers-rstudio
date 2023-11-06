library(OpenImageR)
pic <- readImage("helping.jpg")
gpic <- rgb_2gray(pic)
A <- svd(gpic)
U <- A$u
S <- A$d
V <- A$v

plot(S, pch = 18, cex = 2, col ="purple")
plot(log(S), pch = 18, cex = 2, col ="blue")

R1 <- S[1]*outer(U[,1],V[,1])
imageShow(R1)

r <- 2
R <- (U[,1:r]%*%diag(S[1:r]))%*%t(V[,1:r])
imageShow(R)

#imageShow(gpic)
#imageShow(pic)
