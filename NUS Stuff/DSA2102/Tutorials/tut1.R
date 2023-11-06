wtm.findiff <- function(f,x,h){
  df <- (f(x+h) - f(x))/h
  return(df)
}

f <- tan
x <- 1

err <- rep(0,17)

for (i in 0:16){
  h <- 10^(-i)
  err[(i+1)] <- abs(wtm.findiff(f,x,h) - ((1/cos(x))^2))
}
k <- 0:16
plot((log10(err))~k)
