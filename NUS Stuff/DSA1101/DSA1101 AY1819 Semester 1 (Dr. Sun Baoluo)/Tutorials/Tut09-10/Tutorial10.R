Titanic_dataset= read.csv("c:/Titanic.csv")
dim(Titanic_dataset)
head(Titanic_dataset)

#Logistic regression

Survival_logistic <- glm (Survived~.,
data=Titanic_dataset, 
family=binomial(link="logit"))

#Naive Bayes classification

library(e1071) 
Survival_Nbayes <- naiveBayes(Survived~.,
data=Titanic_dataset)

#different classfiers produce different performances
#plot 2 ROC curves in the same plane

library(ROCR)
pred = predict(Survival_logistic, type="response")
predObj = prediction(pred, Titanic_dataset$Survived)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
plot(rocObj)


nb_prediction <- predict(Survival_Nbayes, Titanic_dataset,type='raw')
score <- nb_prediction[, 2]
pred_nb <- prediction(score, Titanic_dataset$Survived)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb, add = TRUE, col = 2) #add = TRUE, plot the curve in the existing plot


legend("bottomright", c("logistic regression","naive Bayes"),col=c("black","red"), lty=1)

