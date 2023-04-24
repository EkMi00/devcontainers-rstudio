# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
library("matlib") # Less Accurate

colleges <- read.table("Colleges.txt",header =TRUE,sep= "\t")
attach(colleges)

LLS <- function(x, y) {
  beta <- solve(t(x) %*% x) %*% (t(x) %*% y)
  return(beta)
}

print(LLS(cbind(1, SAT), Acceptance))
print(lm(Acceptance ~ SAT, data=colleges)$coefficients)

print(LLS(cbind(1, SAT, Top.10p), Acceptance))
print(lm(Acceptance ~ SAT + Top.10p, data=colleges)$coefficients)
detach(colleges)

house <- read.csv("house_selling_prices_FL.csv")
house$NW <- factor(house$NW)
attach(house)

corr <- cor(size, price)

# plot(size, price, type='n')
# points(price ~ size, col="black", pch = 20)
plot(size, price, pch=20)

M1 <- lm(price ~ size, data=house)
r_sq <- summary(M1)$r.squared

print(corr == sqrt(r_sq))

M2 <- lm(price ~ size + NW, data=house)
M2$coefficients["NW1"]

predict(M2, newdata=data.frame(size=4000, NW="1"))


