######################TUT 6######################
###n-fold cross validation for decision trees###
library("rpart")
library("rpart.plot")
setwd("c:/")

#CV for decision tree

banktrain <- read.table("bank-sample.csv",header=TRUE,sep=",")

## drop a few columns to simplify the tree
drops<-c("age", "balance", "day", "campaign", "pdays","previous", "month", "duration")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]

## total records in dataset
n=dim(banktrain)[1]

## Randomly split into 10 datasets 
## We have seen this code before
n_folds=10
folds_j <- sample(rep(1:n_folds, length.out = n))
head(folds_j)
table(folds_j)

cp=10^(-5:5)
misC=rep(0,length(cp))

for(i in 1:length(cp)){
      misclass=0
	for (j in 1:n_folds) {
	 	test <- which(folds_j == j)
		train=banktrain[-c(test),]
		fit <- rpart(subscribed ~ job + marital + 
             	education+default + housing + 
             	loan + contact+poutcome, 
             	method="class", 
             	data=train,
             	control=rpart.control(cp=cp[i]),
             	parms=list(split='information'))

		new.data=data.frame(banktrain[test,c(1:8)])
            ##predict label for test data based on fitted tree
		prd=predict(fit,new.data,type='class')
            misclass = misclass + sum(prd!=banktrain[test,9])
       }
       misC[i]=misclass/n
}

plot(log(cp,base=10),misC,type='b')
#minimum at cp=10^(-2)


## determine the best cp in terms of 
## misclassification rate
which(misC==min(misC))
best.cp =cp[which(misC==min(misC))]

## Fit decision tree
fit <- rpart(subscribed ~ job + marital + 
            education+default + housing + 
            loan + contact+poutcome, 
            method="class", 
            data=train,
            control=rpart.control(cp=best.cp),
            parms=list(split='information'))

## Plot the tree
rpart.plot(fit, type=4, extra=4)





