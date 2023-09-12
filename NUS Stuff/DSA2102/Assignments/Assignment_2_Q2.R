set.seed(123)

getBanded <- function(n, w) {
    M <- matrix(sample(-9:9, size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 
w = 2
n <- 7
M <- getBanded(n, w)
b <- 1:n
print(M)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    5    8    9    0    0
# [2,]    9    1   -1   -1    0
# [3,]    4   -5   -7    9   -3
# [4,]    0    4   -2   -6    2
# [5,]    0    0   -3    4    5

LU <- M
for (i in (1:(1+w))) {
    for (j in (i+1):(i+w+1)) {
        # m <- M[j,i]/M[i,i]
        print(c(j,i))
    }
}

# print(LU)
my.elimination <- function(A,b,w) {
    A <- cbind(A,b)
    n <- nrow(A)
    for (i in 1:(n-1)) {
        for (j in (i+1):n) {
            m <- A[j,i]/A[i,i]
            for (k in (i+1):(n+1)) {
                # print(c(j,k))
                A[j,k] <- A[j,k] - (m*A[i,k])
            }
        }
    }
    # print(A[,-(n+1)])
    return(A)
}

my.backsub <- function(U,b) {
    n <- nrow(U)
    for (j in n:2) {
        if (U[j,j] == 0) {return("Error: singular")}
        else {
        b[j] <- b[j]/U[j,j]
            for (i in 1:(j-1)){
                b[i] <- b[i]-U[i,j]*b[j]
            }
        }
    }
    b[1] <- b[1]/U[1,1]
    return(b)
}

LU <- (my.elimination(M,b,w))[,1:n] 
# print(LU)

my.solve <- function(A,b){
  n <- nrow(A)
  LU <- (my.elimination(A,b,w)) 
  U <- LU[,1:n]
  u <- LU[,n+1]
  my.backsub(U,u)
}

# print(my.solve(M, b))
# print(solve(M, b))

# print(getLU(M,b))