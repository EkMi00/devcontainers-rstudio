free = 10
pchisq(18.31/2, df=free, lower.tail=FALSE)  
pchisq(18.31/4, df=free, lower.tail=FALSE) 

pchisq(18.31, df=free, lower.tail=FALSE) 
c = 5.62393
n = 54
round(1-pnorm(c/(25/sqrt(n))), 2)
round(1-pnorm((c-10)/(25/sqrt(n))), 2)
