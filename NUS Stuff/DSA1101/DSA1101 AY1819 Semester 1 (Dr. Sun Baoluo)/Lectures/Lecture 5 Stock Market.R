market=read.csv('C:/Smarket.csv')
dim(market)
head(market)
summary(market[,2:10])
train=(market$Year<2005) #boolean value
train.data=market[train,] #import in training data
test.data=market[!train ,] #import EXCLUDING(!) training data
dim(test.data)
train.x=train.data[,c('Lag1','Lag2','Lag3','Lag4','Lag5')]
test.x=test.data[,c('Lag1','Lag2','Lag3','Lag4','Lag5')]
train.y=train.data[,c('Direction')]
set.seed(0) #same number everytime you run the programme
test.y=test.data[,c('Direction')]
knn.pred=knn(train.x,test.x,train.y,k=5)
confusion.matrix=table(knn.pred, test.y)
confusion.matrix #predicted class/test(y) and actual class position is changed
sum(diag(confusion.matrix))/sum(confusion.matrix) #accuracy



