#function
logistic= function(z) {
 exp(z)/(1+exp(z))
} 

z= seq(-10, 10, 0.1)
plot(z, logistic(z), xlab='z', ylab='f(z)', lty=1)

churn=read.csv('c:/churn.CSV')
head(churn)
summary(as.factor(churn$Churned)) #21.8% churned

#Specify the family to be binomial, with the logit link.
Churn_logistic1=glm(Churned~Age + Married + Cust_years
+ Churned_contacts, data=churn, family=binomial(link='logit')) #logit: logistic

summary(Churn_logistic1)
#smaller p value -> more * -> more influential in predicting the outcome
#Married & Cust_years: insignificant variables contributing to outcome
#***: most important variables, useful to predict the outcome

#so we drop Married & Cust_years from the model
