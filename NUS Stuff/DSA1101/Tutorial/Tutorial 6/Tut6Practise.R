# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

credit <- read.csv("German_credit.csv")
attach(credit)

library(class)
set.seed(1)

credit[ ,2:5] = scale(credit[ ,2:5])
train = sample(1:1000 , 800)
train.x = credit[train, 2:5]
train.y = credit[train, 1]
test.x = credit[-train, 2:5]
test.y = credit[-train, 1]

pred = knn(train.x, test.x, train.y, k=1)

confusion.matrix = table(pred, test.y)
TP = confusion.matrix[2, 2]
FP = confusion.matrix[2, 1]
TN = confusion.matrix[1, 1]
FN = confusion.matrix[1, 2]
# acc = (TP + TN) / sum(confusion.matrix)
acc = sum(diag(confusion.matrix)) / sum(confusion.matrix) # 0.58
# TPR = TP / (TP + FN)
TPR = confusion.matrix[2,2] / sum(confusion.matrix[, 2]) # 0.7042254
# FPR = FP / (FP + TN)
FPR = confusion.matrix[2,1] / sum(confusion.matrix[, 1]) # 0.7241379
# FNR = FN / (TP + FN)
FNR = confusion.matrix[1,2] / sum(confusion.matrix[, 2]) # 0.2957746
# pre = TP / (TP + FP) 
pre = confusion.matrix[2,2] / sum(confusion.matrix[2, ]) # 0.7042254

# print(acc)

###############################################################################
n_folds=5 # each fold has 50 data points

folds_j <- sample(rep(1:n_folds, length.out = dim(credit)[1] )) 

table(folds_j)

X = credit[,2:5] # input features
Y = credit[,1] # response

acc=numeric(n_folds) # to store the accuracy for each iteration of n-fold CV

for (j in 1:n_folds) {
	test_j <- which(folds_j == j) # get the index of the points that will be in the test set
	pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=1) # KNN with k = 1, 5, 10, etc

	acc[j]=mean(Y[test_j] == pred) 
      # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test_j],pred)
}

# print(mean(acc))
###############################################################################

# k_class <- function(n_folds, k_value) {
n_folds = 5
k_value = 100
# folds_j = sample(rep(1:n_folds, length.out=dim(credit)[1]))
acc = numeric(n_folds)
accuracy = numeric(k_value)
# err = numeric(n_folds)

for (i in 1:k_value) {
    for (j in 1:n_folds) {
        test_j = which(folds_j == j)
        # X = credit[,2:5]
        # Y = credit[,1]
        pred = knn(train=X[-test_j,], test=X[test_j,],
        cl=Y[-test_j], k = i)
        acc[j] = mean(Y[test_j] == pred) 
        # err[j] = mean(Y[test_j] != pred)
    }
    accuracy[i] = mean(acc)
}
index = which(accuracy == max(accuracy))
# print(accuracy)
# print(c(index, max(accuracy)))


plot(x=1:100, accuracy, xlab = "K")
abline(v = index, col = "red", )

detach(credit)

###############################################################################
library(rpart)
library(rpart.plot)

iris = read.csv("iris.csv")

attach(iris)
fit <- rpart(class ~ sepal.length + sepal.width + petal.length + petal.width,
method="class",
data=iris,
control=rpart.control(minsplit=1),
parms=list(split='information'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)