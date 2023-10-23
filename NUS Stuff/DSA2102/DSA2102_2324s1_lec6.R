wtm.elimination <- function(A,b){
	A <- cbind(A,b)
    n <- nrow(A)
    for (i in 1:(n-1)){
      for (j in (i+1):n){
        m <- A[j,i]/A[i,i]
        for (k in (i+1):(n+1)){
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


A <- rbind(
	c(1,4,1,1,1),
	c(0,1,-7, 2,3),
	c(0,0,2,3,4),
	c(0,0,1,7,3),
	c(0,0,1,0,2))
b <- c(0,-2,-3,-7,0)

print(solve(A,b))