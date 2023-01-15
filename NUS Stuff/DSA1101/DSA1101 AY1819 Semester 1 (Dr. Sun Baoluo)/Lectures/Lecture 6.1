######
iris= read.csv('C:/iris.csv', header=FALSE) #no header, R takes first row as first data point
#if there's no header, there should be a manual/explanatory file for the headers. Use that to change your variable names.
head(iris)
#rename the attributes more meaningfully
names(iris)=c('X1','X2','X3','X4','Y') 
head(iris)

###### kNN for n=1
set.seed(1)
library(class)
X=iris[,c('X1','X2','X3','X4')]
Y=iris[,c('Y')]
pred = knn(train=X, test=X, cl=Y, k=1)
#Note that the code mean(Y!=pred) computes the proportion
of data points which label is predicted incorrectly
mean(Y!= pred)

######
Predicted=c(1, 1, 0, 0, 1, 1)
Actual=c(1, 1, 1, 0, 0, 1)
Predicted != Actual
mean(Predicted != Actual) #true converts to 1, false converts to 0
#i.e the proportion of data points differ from actual result
Predicted == Actual
mean(Predicted == Actual)

###### Visualise the label and feature patterns
library(ggplot2)
library(magrittr)
#sepal width vs. sepal length
ggplot(aes(fortify(x=X[,1], y=X[,2], color=Y))) + 
geom_point()+
labs(x='sepal length')+ labs (y='sepal width')
### + means adding things to the same plot
#petal width vs. petal width
ggplot(aes(x=X[,3], y=X[,4], color =Y)) +
geom_point()+
labs(x='petal length') + labs(y= 'petal width')

###### Perform 10-fold cross validation, 150 points into 10 sets
n_folds=10
folds_i=sample(rep(1:n_folds, length.out=150)) 
#rep function: repeat sequence 1:10 so that the total length out would be 150.
#sample function: randomly shuffle the labels, every label appears the same number of times.
table(folds_i)

###### kNN, k=1
#We start with k = 1, and observe first iteration: using the
#first dataset as our test data and the remaining 9 datasets as training data
test_j = which (folds_i == 1) #extract indexes of data points with label 1
pred = knn(train=X[-test_j, ], test=X[test_j,], cl=Y[-test_j], k=1)

The command X[-test_j,] removes the data points fromfirst data set from the feature dataframe, keeping the data
points from the remaining nine data sets.
Similarly, the command Y[-test_j] removes the data points
from first data set from the label vector, keeping the data
points from the remaining nine data sets.
The command X[test_j,] keeps the data points only fromfirst data set from the feature dataframe.

mean(Y[test_j] != pred)
mean(Y[test_j] == pred)

###### Illustration
folds_i[1:20]
which(folds_i==1) #location of data points with label 1

###### for loop:
for (j in (1:10)) { #end inclusive
 print(j) #indentation: 1 space

######
err=numeric(10) # a vector of length 10
acc=numeric(10) #accuracy

for (j in 1:10) {

test_j=which(folds_i==j)
pred = knn(train=X[-test_j, ], test=X[test_j,], cl=Y[-test_j], k=1)

err[j]=mean(Y[test_j] != pred)
acc[j]=mean(Y[test_j] == pred)
}
err
acc
error=mean(err)
accur=mean(acc)
error
accur

######
However, to plot the accuracy (or error rate) against di
erent
values of k, we need to repeat the 10-fold cross validation
procedure at di
erent values of k
This suggest another for loop structure, indexed by k
In this example, we will perform 10-fold cross validation for
k = 1; 2; :::; 50

error_differentK=numeric(50)
accur_differentK=numeric(50)

for (K in 1:50) {

error=numeric(10)
accur=numeric(10)
for (j in 1:10) {
 test_j= which (folds_i==j)
 pred = knn(train=X[-test_j, ], test=X[test_j,], cl=Y[-test_j], k=K)
 error[j]=mean(Y[test_j]!=pred)
 accur[j]=mean(Y[test_j]==pred)
}
error_differentK[K]=mean(error)
accur_differentK[K]=mean(accur)
}
plot(1:50, accur_differentK, type ="o",ylab ="accuracy ", xlab ="K", cex.axis =1, cex =2)
plot(1:50, error_differentK, type ="o",ylab ="error rate ", xlab ="K",cex.axis =1, cex =2)

######
library(caret)
fitControl= trainControl (method= 'repeatedcv', number=10, repeats=10)
set.seed(2)
knnFit1 = train(Y ~., data = iris ,method = " knn",trControl = fitControl ,preProcess = c("center","scale"), tuneLength =50)

#slide 45