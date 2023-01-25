##Tutorial 5
churn=read.csv("churn.csv")

head(churn)
dim(churn)


library ("rpart")
library ("rpart.plot")

head(churn)

fit <- rpart ( Churned ~ Age + Married + Cust_years
+ Churned_contacts,
method ="class",
data = churn,
control =rpart.control(cp=0.002),
parms = list(split ='information'))

rpart.plot(fit , type=4, extra=2, 
clip.right.labs =FALSE , varlen =0, faclen =0)
