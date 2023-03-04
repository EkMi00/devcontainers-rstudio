
################# STOCK MARKET EXAMPLE
# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
market = read.csv("Smarket.csv")
dim(market)

# print(summary(market[,2:10]))


train =(market$Year <2005)# take all the days in the years from 2001 to 2004 as training set
train.data = market[train,]
test.data  = market[!train ,] # take the year 2005 as test set

dim(test.data)

library(class)


train.x = train.data[,c("Lag1","Lag2","Lag3","Lag4","Lag5")]
test.x = test.data[,c("Lag1","Lag2","Lag3","Lag4","Lag5")]
train.y = train.data[,c("Direction")]
test.y = test.data[,c("Direction")]


set.seed(1)
knn.pred = knn(train.x,test.x,train.y,k=1)  # KNN with k = 1, k = 2 is more accurate
# print(knn.pred)
confusion.matrix=table(knn.pred, test.y)
# print(confusion.matrix)
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix) # 0.515873
# (55+75)/252 ~ 51.59% of the observations are correctly predicted
# print(accuracy)

knn.pred = knn(train.x,test.x,train.y,k=10)  # KNN with k = 10
# confusion.matrix=table(knn.pred, test.y)
confusion.matrix=table(test.y, knn.pred)
print(confusion.matrix)
accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix) # Accuracy = 0.5357143
# print(accuracy)




######### N-FOLD CROSS VALIDATION 

#  A small example on dividing whole data set into n folds  #################
n_folds=3
Number_of_datapoints=12 # sample size
index=rep(1:n_folds,length.out = Number_of_datapoints) # length.out?
s = sample(index); s
table(s) 
# dataset of 12 points is devided into 3 folds randomly, each fold has 4 points.
# the 4 points for each of 3 folds are selected from the dataset following s. For example,
# s = 3 1 1 3 2 2 2 2 1 3 1 3
# then, the first data point belongs to 3rd fold. The next 2 points belong to 1st fold, etc.
##################################################

## 5-fold Cross-Validation for KNN with k=1, 5, 10, etc. for the data set Smarket.csv

X=market[,c("Lag1","Lag2","Lag3","Lag4","Lag5")] # explanatories
Y=market[,c("Direction")] # response

dim(market) # 1250 data points/observations


n_folds=20

folds_j <- sample(rep(1:n_folds, length.out = dim(market)[1] )) 

table(folds_j)

err=numeric(n_folds)
acc=numeric(n_folds)

for (j in 1:n_folds) {
	test_j <- which(folds_j == j) # get the index of the points that will be in the test set
	pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=1) # KNN with k = 1, 5, 10, etc
	err[j]=mean(Y[test_j] != pred)
	acc[j]=mean(Y[test_j] == pred) 
	# this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix),
	# where confusion.matrix=table(Y[test_j],pred)

}
err
acc
error=mean(err); error
accur=mean(acc); accur



