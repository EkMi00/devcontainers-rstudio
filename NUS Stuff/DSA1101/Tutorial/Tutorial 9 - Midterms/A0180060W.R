setwd("C:/Data");

data1 <- read.table("data1.txt", header=TRUE, sep="");
head(data1)
data1$color <- factor(data1$color)
attach(data1)

#Q1

hist(satell, freq=FALSE)
lines(density(satell), col = "red")
x <- seq(0, max(satell), length.out=length(satell))
y <- dnorm(x, mean(satell), sd(satell))
lines(x, y, col = "red") 
# The plot is showing a right-skew and
# and with a unimodal distribution. 

#Q2

boxplot(satell)
outliers <- boxplot(satell)$out
index <- which(satell %in% c(outliers))
print(data1[index,])
# color spine width satell weight
# 15     3     1  26.0     14    2.3
# 56     3     3  28.3     15    3.0

#Q3

qqnorm(satell, pch = 20)
qqline(satell, col = "red")
# The qq plot shows a longer left tail than the
# right which shows a ;eft skew.  (WRONG)  
# This plot also shows the satell quantity 
# does not follow a normal distributiom.

# comments: the left tail is obviously shorter than normal. 
# The sample of satell hence is NOT normally distributed.

#Q4

col = ifelse(color=="2" | color=="3", "light", "dark")

#Q5
table(col)
# dark light 
# 66   107 

#Q6
cbind(data1, col)
light <- satell[which(col=="light")]
dark <- satell[which(col=="dark")]
plot(weight, satell, type = "n")
points(light ~ weight[which(col=="light")],
       col = "red", pch = 20)
points(dark ~ weight[which(col=="dark")], 
       col = "darkblue", pch = 20)
legend(1.2, 13, legend = c("light", "dark"), 
       col = c("red","darkblue"), pch=c(20,20))

#Q7
M <- lm(satell ~ color + weight + width, data=data1)

#Q8
summary(M)$r.squared
# R^2 value = 0.149192
# Since R^2 value is significantly low, the goodness of
# fit is low and therefore the model is not an
# accurate fit for the data.

# Through F-test for the significance of the overall  model: model is significant
# However, R^2 is too low. It means model doesn't fit the data well.

detach(data1)

###################################
data2 <- read.csv("data2.csv");
data2$survival.status <- factor(data2$survival.status)
attach(data2)
library(class)
set.seed(999)
data2 <- data2[, -2]

X <- data2[-3]
Y <- data2[3]

n_folds=3

folds_j <- sample(rep(1:n_folds, length.out = dim(data2)[1] )) 

table(folds_j)
ave.type1=numeric(50)
ave.type2=numeric(50)

for (k in 1:50) {
  typeI=numeric(n_folds)
  typeII=numeric(n_folds)
  
  for (j in 1:n_folds) {
    test_j <- which(folds_j == j) # get the index of the points that will be in the test set
    pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j,], k=k)
    err[j]=mean(Y[test_j,] != pred)
    acc[j]=mean(Y[test_j,] == pred) 
    confusion.matrix <- table(knn.pred, test.y)
    TP <- confusion.matrix[2,2]
    TN <- confusion.matrix[1,1]
    FP <- confusion.matrix[1,2]
    FN <- confusion.matrix[2,1]
    eachtypeI <- FP /(FP + TN)
    eachtypeII <- FN /(TP + FN)
    typeI[j] <- eachtypeI
    typeII[j] <- eachtypeII
  }
  
  ave.type1[k] <- mean(typeI)
  ave.type2[k] <- mean(typeII)
}

print(ave.type1)
print(ave.type2)

# Length of ave.type1 = 50
# Length of ave.type2 = 50

plot(ave.type1, ave.type2, pch = 20)


# A larger k should be used as it has a lower variance
# which would increase the number of true positives
# and true negatives.

# for k = 50, ave.type1 = 0.205
# ave.type2 = 0.636

