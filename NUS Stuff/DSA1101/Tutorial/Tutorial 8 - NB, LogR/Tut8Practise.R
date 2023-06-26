# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

titanic= read.csv("Titanic.csv")

attach(titanic)

# P(Y==survived/1) and P(Y==survived/0)
tprior <- table(Survived)/sum(table(Survived))


# P(X_i| Y==1 or Y==0)
classCounts <- table(titanic[,c("Survived", "Class")])
classCounts <-classCounts/rowSums(classCounts) # rowSums is a column vector
# print(classProb)

#         Class
# Survived        1st        2nd        3rd       Crew
#      No  0.08187919 0.11208054 0.35436242 0.45167785
#      Yes 0.28551336 0.16596343 0.25035162 0.29817159

genderCounts <- table(titanic[,c("Survived", "Sex")])
genderCounts <-genderCounts/rowSums(genderCounts)
# print(sexProb)

ageCounts <- table(titanic[,c("Survived", "Age")])
ageCounts <-ageCounts/rowSums(ageCounts)
# print(ageProb)


# Predict Survive given adult, female, 2nd class
prob_survived <-
unname(classCounts["Yes","2nd"]*
genderCounts["Yes","Female"]*
ageCounts["Yes","Adult"]*
tprior["Yes"])
# print(prob_survived)

prob_not_survived <-
unname(classCounts["No","2nd"]*
genderCounts["No","Female"]*
ageCounts["No","Adult"]*
tprior["No"])

# print(prob_not_survived)

# print(c(prob_survived, prob_not_survived))

library(e1071)

M1 <- naiveBayes(Survived ~., titanic)

test <- data.frame(Class="2nd", Sex="Female", Age="Adult")


results <- predict(M1,test)
# print(results)
results <- predict(M1,test, "raw")
# print(results)    

r1 = prob_survived/prob_not_survived
r2 = results[2]/results[1]

names(r1) <- c("Ratio 1")
names(r2) <- c("Ratio 2")
# print(c(r1, r2))

detach(titanic)
###################################################################

titanic$Survived <- titanic$Survived == "Yes"
attach(titanic)

M2 <- glm(Survived ~ Class + Sex + Age, data=titanic, family=binomial(link="logit"))

# print(summary(M2))
# print(M2$coefficients)
# (Intercept)    Class2nd    Class3rd   ClassCrew     SexMale    AgeChild
#   2.0438374  -1.0180950  -1.7777622  -0.8576762  -2.4200603   1.0615424

# log[phat/(1-phat)] =  2.0438 -1.0181*I(Class = 2nd) -1.7778*I(Class = 3rd) 
#                       -0.8577* I(Class = Crew) -2.4201*I(Sex = Male) + 1.0615*I(Age = Child)

# (c) Interpret the coefficient of variable SEX:
# coefficient is estimated = -2.4201. It means, given the same condition on the class and age,
# when comparing to a female, the LOG-ODDS of survivalfor a male is less than by 2.42.
# It means, the ODDS of survival of a male passenger will be less than that of a female by
# e^2.42 = 11.25 TIMES.

# (d) Interpret the coefficient of variable AGE:
# coefficient is estimated = 1.0615. It means, given the same condition on the class and gender,
# when comparing to an adult, the LOG-ODDS of survival of a child is larger by 1.0615.
# That means, the ODDS of survival of a child passenger is larger than that of an adult passenger by 
# e^1.0615 = 2.89 TIMES.

library(ROCR)
#  Lowering the classification threshold classifies more items as positive,
#  thus increasing both False Positives and True Positives.
# ROC/AUC for Logistic Regression:
pred <- predict(M2, type = "response")
predObj <- prediction(pred, Survived)
rocObj <- performance(predObj, measure="tpr", x.measure="fpr")
aucObj <- performance(predObj, measure="auc")
# print(auc@y.values[[1]]) # 0.7597259

plot(rocObj, col="blue")

# ROC/AUC for Naive-Bayes Classifier
# naiveB <- predict(M1, titanic[,1:3], type="class")
naiveB <- predict(M1, titanic[,1:3], type="raw")
score <- naiveB[,2]
pred_nb <- prediction(score, Survived)
roc_nb <- performance(pred_nb, measure="tpr", x.measure="fpr")
auc_nb <- performance(pred_nb, measure="auc")
# print(auc_nb@y.values[[1]]) # 0.7164944
plot(roc_nb, add=TRUE, col="red")

legend(x=0.7, y=0.4, c("logistic regression", "naive-bayes"), col=c("blue", "red"), lty=1)
detach(titanic)