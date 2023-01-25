churn=read.csv('C:/churn.CSV')
head(churn)
summary(as.factor(churn$Churned))
#Remove ID column
churn=churn[,-1]
#Standardize continuous variables
churn[,c('Age','Cust_years','Churned_contacts')]=scale(churn[,c('Age','Cust_years','Churned_contacts')])
churn.X=churn[,-1]
test=1:4000
train.X=churn.X[-test,]
test.X=churn.X[test ,]
train.Y=churn$Churned[-test]
test.Y=churn$Churned[test]
set.seed (1)
knn.pred=knn(train.X, test.X, train.Y, k=5)
confusion.matrix=table(test.Y, knn.pred)
confusion.matrix
#k=5 is optimal, precision=420/(420+266)~61%