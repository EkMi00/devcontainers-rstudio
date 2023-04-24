# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

churn = read.csv("churn.csv")

library("rpart")
library("rpart.plot")

churn$Churned = factor(churn$Churned)
churn = churn[,-1]
attach(churn)
fit = rpart(Churned ~ Age + Married + Cust_years + Churned_contacts,
method ="class",
data = churn ,
control = rpart.control(minsplit = 1),
parms = list(split ='information'))

# rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)


age = c(26,23,56,36,45,28,22,22,60,32)
married = c(1,1,1,1,0,0,1,0,1,0)
cust =c(2,3,5,5,2,2,3,3,2,3)
contact = c(2,3,2,2,1,2,0,2,1,1)

newdata = data.frame(cbind(Age=age, Married=married, 
    Cust_years=cust, Churned_contacts=contact))

pred = predict(fit, newdata, type='class')

detach(churn)

###############################################################################

set.seed(555)

iris = read.csv("iris.csv")
attach(iris)
n_folds = 5

folds_j1 =  sample(rep(1:n_folds, length.out=50))
folds_j2 =  sample(rep(1:n_folds, length.out=50))
folds_j3 =  sample(rep(1:n_folds, length.out=50))

data1 = iris[which(class=="Iris-setosa"),]
data2 = iris[which(class=="Iris-versicolor"),]
data3 = iris[which(class=="Iris-virginica"),] 

acc = numeric(n_folds)

for (j in 1:n_folds) {
    test1 = which(folds_j1 == j)
    test2 = which(folds_j2 == j)
    test3 = which(folds_j3 == j)
    
    train1 = data1[-test1,]
    train2 = data2[-test2,]
    train3 = data3[-test3,]

    train = rbind(train1, train2, train3)

    fit = rpart(class ~ sepal.length + sepal.width + petal.length + petal.width,
    method="class",
    data=train,
    control=rpart.control(minsplit=1),
    parms=list(split='gini'))


    test = rbind(data1[test1,], data2[test2,], data3[test3,])
    test.X = test[,-5]
    test.Y = test[,5]

    pred <- predict(fit, newdata = test.X, type='class')

    confusion.matrix = table(pred, test.Y)

    acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
}

# print(mean(acc))

detach(iris)

###############################################################################

banktrain <- read.csv("bank-sample.csv", header=TRUE)

concern = c("subscribed", "job", "marital", "education",
 "default", "housing", "loan", "contact", "poutcome")

banktrain = banktrain[,concern]
attach(banktrain)

n_folds = 10
folds_j = sample(rep(1:n_folds, length.out=dim(banktrain)[1]))

cp=10^(-5:5);
misC = numeric(length(cp))

for (i in 1:length(cp)) {
    misClass = 0

    for (j in 1:n_folds) {
        test_j = which(folds_j == j) 
        train = banktrain[-test_j,]

        fit = rpart(subscribed ~ job + marital + education
        + default + housing + loan + contact + poutcome,
        method="class",
        data=train,
        control=rpart.control(cp = cp[i]),
        parms=list(split='information'))

        new.data = data.frame(banktrain[test_j, -1])

        pred = predict(fit, newdata=new.data, type="class")
        real = banktrain[test_j, 1]

        # confusion = table(pred, real)
        misClass = misClass + sum(pred != real)
    }

    misC[i] = misClass/dim(banktrain)[1]
}
print(misC)
bestCP = cp[which(misC == min(misC))]

# 0.1080 0.1080 0.1070 0.1030 0.1055 0.1055 0.1055 0.1055 0.1055 0.1055
# 0.1055
plot(-log(cp,base=10), misC, type='b') # -log_10(10^(-5, 5))

fit = rpart(subscribed ~ job + marital + education
    + default + housing + loan + contact + poutcome,
    method="class",
    data=train,
    control=rpart.control(cp = bestCP),
    parms=list(split='information'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=3)

detach(banktrain)