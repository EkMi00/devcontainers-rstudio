set.seed(1234)

getBanded <- function(n, w) {
    M <- matrix(sample((-9:9)[-10], size = n*n, replace=TRUE), nrow=n)
    M[abs(row(M) - col(M)) > w] = 0
    return(M)
} 

count <- 0

reduce <- function(A, i, upper) {
    n <- nrow(A)
    for (j in (i+1):(upper)) {
        A[j,i] <- A[j,i]/A[i,i]
        # print(A[j,i] != 0)
        for (k in (i+1):(upper)) {
            A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
            # count <- count + 2
        }
        A[j,n+1] <- A[j,n+1] - (A[j,i] * A[i,n+1]) # For the b vector
        # count <- count + 3
    }
    # print(count)
    return(A)
}

my.elimination <- function(A,b,w) {
	A <- cbind(A,b)
    n <- nrow(A)
    for (i in 1:(n-w)) {
        # for (j in (i+1):(i+w)) {
        #    A[j,i] <- A[j,i]/A[i,i]
        #    for (k in (i+1):(i+w)) {
        #        A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
        #    }
        #    A[j,n+1] <- A[j,n+1] - (A[j,i] * A[i,n+1])
        # }
        A <- reduce(A, i, i+w)
    }
    if (w != 1) {
        for (i in (n-w+1):(n-1)) {
            # for (j in (i+1):n) {  
            #    A[j,i] <- A[j,i]/A[i,i]
            #    for (k in (i+1):n) {
            #        A[j,k] <- A[j,k] - (A[j,i] * A[i,k])
            #    }
            #    A[j,n+1] <- A[j,n+1] - (A[j,i]  * A[i,n+1])
            # }
            A <- reduce(A, i, n)
        }
    }
    return(A)
}

back_reduce <- function(U, b, lower, j) {
    if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
        return("Error: singular or need permuting/pivoting")
    }
    else {
        b[j] <- b[j]/U[j,j]
        # count <- count + 1
        for (i in (lower):(j-1)) {
            b[i] <- b[i] - U[i,j] * b[j]
            # count <- count + 2
        }
    }
    # print(count)
    return(b)
}

my.backsub <- function(U, b, w) {
    n <- nrow(U)
    for (j in n:(w+1)) {
        # if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
        #     return("Error: singular or need permuting/pivoting")
        # }
		# else {
        #     b[j] <- b[j]/U[j,j]
        #     for (i in (j-w):(j-1)) {
        #        b[i] <- b[i] - U[i,j] * b[j]
        #     }
		# }
        b <- back_reduce(U, b, j-w, j)
	}
    if (w != 1) {
        for (j in w:2) {
            # if (U[j,j] == 0 | is.na(U[j,j]) | is.nan(U[j,j])) {
            #     return("Error: singular or need permuting/pivoting")
            # }
            # else {
            #     b[j] <- b[j]/U[j,j]
            #     for (i in 1:(j-1)) {
            #        b[i] <- b[i] - U[i,j] * b[j]
            #     }
            # }
            b <- back_reduce(U, b, 1, j)
        }
        
    }
	b[1] <- b[1]/U[1,1]
    # count <- count + 1
    # print(count)
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

        case <- sprintf("Case: %.0f x %.0f matrix, Bandwidth w = %.0f",n,n,i)
        print(case)
        print(count)
        # print(myans)
        # print(correct)
    }  
}


n <- 7
# print(testCases(n))

w <- 3
M <- getBanded(n, w)
b <- 1:n
# LU <- my.elimination(M,b,w)
# print(LU)
x <- my.solve(M, b, w)
mycount <- (n - w)*(2*w*w + 5*w + 1) + (w-1)*(4*w*w + 7*w)/6 + w*w
# print(mycount)