set.seed(123)

getBanded <- function(n, w) {
    M <- matrix(sample(-99:99, size = n*n), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 
w = 1
rows <- 10
M <- getBanded(rows, w)
b <- 1:rows
print(M)

my.elimination <- function(A,b,w) {
    A <- cbind(A,b)
    n <- nrow(A)
    A[1,2] = A[1,2]/A[1,1]

    # Diagonal 
    for (i in 1:n) { 
        for (j in 1:n) {
            for (k in 1:(j-1)) {
                A[i,i] - A[i,k]*A[k,i]
            }
        }
    }
    # Lower Sub-diagonal
    for (i in 1+w:n) { 
        for (j in 1:n) {
            for (k in 1:(j-1)) {
            }
        }
    }
    for (i in 1:w) {
        for (j in 1:(i+w)) {
            # print(c(i,j))
            for (k in i:i+w) {
                # print(c(i,k, k, j))
            }
        }
    }
    for (i in (n-w+1):n) {
        for (j in (i-w):n) {
            # print(c(i,j))
            
        }
    }
    for (i in (1+w):(n-w)) {
        for (j in (i-w):(i+w)) {
            # print(c(i,j))
            m <- A[j,i]/A[i,i]
            # A[j,i] <- m   
            for (k in (i+1):(n+1)) {
                # A[j,k] <- A[j,k] - (m * A[i,k])
            }
        }
    }
  return(A)
}

# my.elimination <- function(A, w) {
#     A <- cbind(A)
#     n <- nrow(A)
#     L <- replicate(n, numeric(n))
#     U <- replicate(n, numeric(n))

    
#   return(A)
# }

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

LU <- (my.elimination(M,b,w))[,1:rows] 
# print(LU)

my.solve <- function(A,b){
  n <- nrow(A)
  LU <- (my.elimination(A,b)) 
  U <- LU[,1:n]
  u <- LU[,n+1]
  my.backsub(U,u)
}

# print(solveLU(M, b))
# print(solve(M, b))

# print(getLU(M,b))