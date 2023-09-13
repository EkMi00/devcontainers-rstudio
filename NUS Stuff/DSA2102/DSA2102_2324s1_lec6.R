wtm.elimination <- function(A,b){
	A <- cbind(A,b)
    n <- nrow(A)
    for (i in 1:(n-1)){
      for (j in (i+1):n){
        # print(c(j,i))
        m <- A[j,i]/A[i,i]
        for (k in (i+1):(n+1)){
          # print(m*A[i,k])
          A[j,k] <- A[j,k] - (m*A[i,k])

        }
      }
    }
    return(A)
}

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

wtm.solve <- function(A,b){
	n <- nrow(A)
	U <- (wtm.elimination(A,b))[,1:n]
	u <- (wtm.elimination(A,b))[,n+1]
	wtm.backsub(U,u)
}

set.seed(123)

getBanded <- function(n, w) {
    M <- matrix(sample(-9:9, size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 


getBanded <- function(n, w) {
    M <- matrix(sample(-9:9, size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 

my.elimination <- function(A,b,w) {
	A <- cbind(A,b)
    n <- nrow(A)
    # for (i in 1:(n-1)) {
    #     for (j in (i+1):(min(i+w,n))) {
    #         m <- A[j,i]/A[i,i]
    #         A[j,i] <- A[j,i]
    #         for (k in (i+1):min(i+w,n)) {
    #             A[j,k] <- A[j,k] - (m * A[i,k])
    #         }
    #         A[j,n+1] <- A[j,n+1] - (m * A[i,n+1]) # For the b vector
    #     }
    # }
    for (i in 1:(n-w)) {
        for (j in (i+1):(i+w)) {
            m <- A[j,i]/A[i,i]
            for (k in (i+1):(i+w)) {
                A[j,k] <- A[j,k] - (m * A[i,k])
            }
            A[j,n+1] <- A[j,n+1] - (m * A[i,n+1]) # For the b vector
        }
    }

    for (i in (n-w+1):(n-1)) {
        w <- w - 1
        for (j in (i+1):(i+w)) {    
            m <- A[j,i]/A[i,i]
            for (k in (i+1):(i+w)) {
                A[j,k] <- A[j,k] - (m * A[i,k])
            }
            A[j,n+1] <- A[j,n+1] - (m * A[i,n+1]) # For the b vector
        }
        
    }
    return(A)
}

n <- 7
w <- 3
M <- getBanded(n,w)
# print(M)
b <- 1:n
LU <- my.elimination(M, b, w)[,-(n+1)]
# [1,]  5 -5 -3.000000  1.0000000  0.00000000   0.0000000   0.000000
# [2,]  9 13  5.400000 -4.8000000 -1.00000000   0.0000000   0.000000
# [3,]  4 -1  1.815385  0.8307692 -0.07692308   3.0000000   0.000000
# [4,] -7  2  3.969231  5.3220339 -2.67796610   1.4406780  -4.000000
# [5,]  0 -1 -5.584615  2.1864407 -3.21337580  -0.3630573   2.643312
# [6,]  0  0  4.000000  1.1694915 -7.24203822 -10.1085233  -7.078295
# [7,]  0  0  0.000000 -3.0000000 -6.50955414   6.5475719 -15.194323
# print(LU)

myans <- my.solve(M, b, w)
correct <- solve(M, b)
print(myans)
print(correct)



my.backsub <- function(U, b, w) {
    n <- nrow(U)    
    for (j in n:2) {
        if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
            return("Error: singular or need pivoting")
        }
		else {
            b[j] <- b[j]/U[j,j]
            for (i in max(1, j-w):(j-1)) {
                b[i] <- b[i] - U[i,j] * b[j]
            }
		}
	}
	b[1] <- b[1]/U[1,1]
	return(b)
}

my.solve <- function(A,b,w){
    n <- nrow(A)
    LU <- (my.elimination(A,b,w)) 
    U <- LU[,1:n]
    u <- LU[,n+1]
    my.backsub(U,u,w)
}

testCases <- function(n) {
    w <- 1:(n-1)
    for (i in w) {
        M <- getBanded(n,i)
		b <- 1:n
        myans <- my.solve(M, b, i)
        correct <- solve(M, b)
        case <- sprintf("Case: %.0f x %.0f matrix, Bandwidth, w = %.0f",n,n,i)
        print(case)
        print(myans)
        print(correct)
        # print(M)
    }  
}

n <- 5
# print(testCases(5))
