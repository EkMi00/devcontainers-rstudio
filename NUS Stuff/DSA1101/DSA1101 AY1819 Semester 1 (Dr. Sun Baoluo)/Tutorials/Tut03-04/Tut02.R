banktrain=read.table('C:/German_credit.csv',header=TRUE,sep=',')
summary(banktrain)
dim(banktrain)

set.seed(1)
train=sample(1:1000,500)
banktrain[,2:5]=lapply(banktrain[,2:5],scale)
#lapply: apply the function to each column of your data frame
train.data=banktrain[train,]
test.data=banktrain[-train,]

train.x=train.data[,2:5]
test.x=test.data[,2:5]
train.y=train.data[,1]
test.y=test.data[,1]

library(class)
knn.pred=knn(train.x, test.x, train.y, k=10)
confusion.matrix=table(knn.pred, test.y)
confusion.matrix
sum(diag(confusion.matrix)/sum(confusion.matrix)) #should be >50%