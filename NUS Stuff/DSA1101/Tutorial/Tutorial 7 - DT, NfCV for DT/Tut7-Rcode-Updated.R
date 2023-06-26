# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

########## CHURN DATA SET  (DECISION TREE)


# (i) Age (years)
# (ii) Married (true/false)
# (iii) Duration as a customer (years)
# (iv) Churned contacts (count)-Number of the customer's contacts
# that have churned (count)
# (v) Churned (true/false)-Whether the customer churned


churn = read.csv("churn.csv")
head(churn)

dim(churn)

summary(as.factor(churn$Churned))

#Remove ID column
churn= churn[,-1]

#churn.X = churn[,-1] # only the input features


library("rpart")
library("rpart.plot")


fit<- rpart(Churned ~ Age + Married + Cust_years + Churned_contacts,
method ="class",
data = churn ,
control = rpart.control(minsplit = 1),
parms = list(split ='information'))

# rpart.plot(fit , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


# minsplit = 20 is by default. But we can change it.
# Equivalently, we can use cp instead of minsplit.
# cp =0.01 is by default, we can change it. 
# Smaller cp makes the tree be bigger, more complex.
# Try cp = 0.001, 0.002, 0.005



age = c(26,23,56,36,45,28,22,22,60,32)
married = c(1,1,1,1,0,0,1,0,1,0)
cust =c(2,3,5,5,2,2,3,3,2,3)
contact = c(2,3,2,2,1,2,0,2,1,1)

head(churn)

new = data.frame(Age = age,Married = married, Cust_years = cust,Churned_contacts = contact)
new

pred = predict(fit, newdata = new, type = 'class'); pred
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used minsplit = 1 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.002 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.004 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.005 for the decison tree





########## IRIS DATA SET  (DECISION TREE & N-FOLD CV)

set.seed(555)

library("rpart")
library("rpart.plot")

iris = read.csv("iris.csv")
head(iris)
names(iris)

n_folds=5 #

folds_j_1 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 1 = setosa

folds_j_2 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 2 = versicolor

folds_j_3 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 3 = virginica


table(folds_j_1)
table(folds_j_2)
table(folds_j_3)

data1 = iris[1:50,] # data for type 1 = setosa
data2 = iris[51:100,] # data for type 2 = versicolor
data3 = iris[101:150,] # data for type 3 = virginica

acc=numeric(n_folds)

for (j in 1:n_folds) {

	test1 <- which(folds_j_1 == j)
	test2 <- which(folds_j_2 == j)
	test3 <- which(folds_j_3 == j)

	train.1=data1[ -test1, ]
	train.2=data2[ -test2, ]
	train.3=data3[ -test3, ]

	train = rbind(train.1, train.2, train.3) # this is the training data set

	fit.iris <- rpart(class ~ sepal.length + sepal.width + petal.length + petal.width,
	method = "class", data =train, control = rpart.control( minsplit =1),
	parms = list( split ='gini')) 

	test = rbind(data1[test1,1:4], data2[test2,1:4], data3[test3,1:4] ) # test data 

	real = c(data1[test1,5], data2[test2,5], data3[test3,5] ) 
	# should use c(), instead of rbind() when creating "real".

	pred = predict(fit.iris, newdata = test, type = 'class')

	confusion.matrix = table(pred, real)

	acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)

}
acc
print(mean(acc)) # the accuracy is very high.



#### BANK-SAMPLE DATA SET

########## N-FOLD CROSS-VALIDATION FOR DECISION TREE  
library("rpart") # load libraries
library("rpart.plot")

banktrain <- read.csv("bank-sample.csv", header=TRUE)
dim(banktrain)

## drop a few columns to simplify the tree
drops<-c("age", "balance", "day", "campaign", 
         "pdays", "previous", "month", "duration")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]

head(banktrain)

## total records in dataset
n=dim(banktrain)[1]; n


## We'll randomly split data into 10 sets of (about) equal size

n_folds=10
folds_j <- sample(rep(1:n_folds, length.out = n))
# this is to randomly sample the indexes of subsets for the observation
#table(folds_j)

cp=10^(-5:5); length(cp)
misC=rep(0,length(cp)) # a vector to record the rate of mis-classification for each cp

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
		pred=predict(fit,new.data,type='class')
		misclass = misclass + sum(pred!=banktrain[test,9])
    }
	misC[i]=misclass/n
}
print(misC)

# plot(-log(cp,base=10),misC,type='b')

## determine the best cp in terms of
## misclassification rate

best.cp =cp[which(misC == min(misC))] 
# this is the value of cp that gives the lowest mis-classification rate

## Fit decision tree with that smallest cp
fit <- rpart(subscribed ~ job + marital + education+default + housing + loan + contact+poutcome, 
             method="class", 
             data=train,
             control=rpart.control(cp=best.cp),
             parms=list(split='information'))

# to get the tree plotted:
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0)#, faclen=3)
