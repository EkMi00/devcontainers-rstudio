##################
######TUT 05######
churn=read.csv('C:/churn.CSV')
head(churn)
dim(churn)

library("rpart")
library("rpart.plot")

#to fit a decision tree: Outcome~feature variables, method: build a decision tree,
data, control parameters: complexity parameter, parms.

fit=rpart(Churned~Age+ Married+ Cust_years+ Churned_contacts, method ="class",
data= churn, control = rpart.control(cp=0.002), parms = list(split = "information"))

rpart.plot(fit, type=2, extra=1, clip.right.labs=TRUE, varlen=0, faclen=0)