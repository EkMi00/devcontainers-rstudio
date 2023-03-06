setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
fev <- read.csv("FEV.csv")
attach(fev)

hist(FEV, col = 10, freq= FALSE) # freq = count or ratio

boxplot(FEV, col = 10,
        ylab="FEV", main = "Boxplot of FEV") #col <- color

out <- boxplot(FEV, col = 10,
               ylab="FEV", main = "Boxplot of FEV")$out

outliers_index <- which(FEV %in% c(out))
length(c(out))
fev[outliers_index,]

qqnorm(FEV, pch = 20)
qqline(FEV, col = 10) 
# Right tail is longer than left

fev$Sex <- factor(Sex, levels = c(0,1))

FEV_female <- FEV[Sex==0] # / FEV[which(Sex==0)]
FEV_male <- FEV[Sex==1]

opar <- par(mfrow=c(1,2))
hist(FEV_female, col = 10, freq=FALSE, ylim = c(0.0, 0.60))
hist(FEV_male, col = 12, freq=FALSE, ylim = c(0.0, 0.60))
par(opar)

summary(FEV_female)
IQR(FEV_female)
var(FEV_female)
summary(FEV_male)
IQR(FEV_male)
var(FEV_male)


plot(height, FEV, pch=20)
female_height <- height[Sex==0]
male_height <- height[Sex==1]
plot(height, FEV, type="n")
points(FEV_female ~ female_height, col='red', pch = 20)
points(FEV_male ~ male_height, col='blue', pch = 20)
legend(x=1.2, y=5, legend = c("Female", "Male"),
       col = c("red","darkblue"), pch=c(20,20))

cor(height, FEV)


fibo <- function(n) {
  a = 0
  b = 1
  for (i in 2:n) {
    temp = a + b
    a = b
    b = temp
  }
  return(b)
}

terms <- c(seq(1:45))
fibo_terms <- lapply(terms, fibo)

print(fibo(40))

n = 1
while (fibo(n) < 5000000) {
  n <- n + 1
}
n; fibo(34)
