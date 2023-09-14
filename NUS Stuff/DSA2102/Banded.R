# set.seed(1234)

getBanded <- function(n, w) {
    M <- matrix(sample((-9:9)[-10], size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 

reduce <- function(A, i, upper) {
    n <- nrow(A)
    for (j in (i+1):(upper)) {
        A[j,i] <- A[j,i]/A[i,i]
        for (k in (i+1):(upper)) {
            A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
        }
        A[j,n+1] <- A[j,n+1] - (A[j,i] * A[i,n+1]) # For the b vector
    }
    return(A)
}

my.elimination <- function(A,b,w) {
	A <- cbind(A,b)
    n <- nrow(A)
    for (i in 1:(n-w)) {
        #for (j in (i+1):(i+w)) {
        #    A[j,i] <- A[j,i]/A[i,i]
        #    for (k in (i+1):(i+w)) {
        #        A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
        #    }
        #    A[j,n+1] <- A[j,n+1] - (A[j,i] * A[i,n+1]) # For the b vector
        #}
        A <- reduce(A, i, i+w)
    }
    if (w != 1) {
        for (i in (n-w+1):(n-1)) {
            #for (j in (i+1):n) {  
            #    A[j,i] <- A[j,i]/A[i,i]
            #    for (k in (i+1):n) {
            #        A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
            #    }
            #    A[j,n+1] <- A[j,n+1] - (A[j,i]  * A[i,n+1]) # For the b vector
            # }
            A <- reduce(A, i, n)
        }
    }
    return(A)
}

back_reduce <- function(U, lower, j) {
    b[j] <- b[j]/U[j,j]
    for (i in (lower):(j-1)) {
        b[i] <- b[i] - U[i,j] * b[j]
    }
    return(U)
}

my.backsub <- function(U, b, w) {
    n <- nrow(U)    
    # print(U)
    for (j in n:(w+1)) {
        if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
            return("Error: singular or need permuting/pivoting")
        }
		else {
            #b[j] <- b[j]/U[j,j]
            #for (i in (j-w):(j-1)) {
            #    b[i] <- b[i] - U[i,j] * b[j]
            #}
            U <- back_reduce(U, j-w, j)
		}
	}
    if (w != 1) {
        for (j in w:2) {
            if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
                return("Error: singular or need permuting/pivoting")
            }
            else {
                #b[j] <- b[j]/U[j,j]
                #for (i in 1:(j-1)) {
                #    b[i] <- b[i] - U[i,j] * b[j]
                #}
                U <- back_reduce(U, 1, j)
            }
        }
    }
	b[1] <- b[1]/U[1,1]
	return(b)
}

n <- 5
w <- 2
M <- getBanded(n,w)
# print(M)
b <- 1:n
LU <- my.elimination(M, b, w)
# print(LU)

x <- my.backsub(LU[,1:n], LU[,n+1], w)
# print(x)
# print(solve(M,b))

my.solve <- function(A,b,w){
    n <- nrow(A)
    LU <- (my.elimination(A,b,w)) 
    U <- LU[,1:n]
    u <- LU[,n+1]
    my.backsub(U,u,w)
}

library("matrixcalc")

testCases <- function(n) {
    w <- 1:(n-1)
    for (i in w) {
        M <- getBanded(n,i)
		b <- 1:n
        myans <- my.solve(M, b, i)
        correct <- solve(M, b)
        case <- sprintf("Case: %.0f x %.0f matrix, Bandwidth w = %.0f",n,n,i)
        print(case)
        L <- my.elimination(M,b,i)[, -(n+1)]
        L[upper.tri(M)] <- 0
        diag(L) <- 1
        U <- my.elimination(M,b,i)[, -(n+1)]
        U[lower.tri(M)] <- 0
    
        print(L%*%U)
        print(M)
        print(L%*%U - M < 10^-10)
        # print("My Answer:" )
        # print(myans)
        # print("Correct Answer:" )
        # print(correct)
    }  
}

# print(LU[1])
n <- 5
print(testCases(n))
