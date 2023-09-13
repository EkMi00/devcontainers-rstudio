library(float)

sprintf("%.5f",as.float(as.float(10^6)+as.float(12.3456)))

123.45*67.89
8381.02-(123.45*67.89)
sprintf("%.23f",123.45*67.89)
as.float(as.float(8381.02)-as.float(as.float(123.45)*as.float(67.89)))



#########

n <- 100000000
x <- as.float(1:n)
y <- as.float(1/x)
z <- sum(y)
sprintf("%.23f",z)
sprintf("%.23f",as.float(1/n))
sprintf("%.23f",z + as.float(1/n))

#########

wtm.exp <- function(x,n) {
  result <- 0
  for (i in 0:n){
    z <- ((x^i)/factorial(i))
    result <- result + z
  }
  return(result)
}
wtm.exp(25,500)
exp(25)

