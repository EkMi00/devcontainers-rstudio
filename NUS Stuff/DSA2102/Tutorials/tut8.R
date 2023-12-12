install.packages("capn")
install.packages("cmna")
library(capn)
library(cmna)
wtm.ccq <- function(f,a,b,n){
  x <- chebnodegen(n,a,b)
  y <- polyinterp(x,f(x))
  z <- 1:(n)
  z <- 1/z
  bn <- rep(b,n)
  for (i in 2:(n)){
    bn[i] <- b*bn[i-1]
  }
  an <- rep(a,n)
  for (i in 2:(n)){
    an[i] <- a*an[i-1]
  }
  I <- sum((y*z*bn) - (y*z*an))
  return(I)
}

integrate(sin,0,1)
wtm.ccq(sin,0,1,5)
