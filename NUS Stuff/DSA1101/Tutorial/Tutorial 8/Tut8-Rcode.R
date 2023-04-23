###### Q1:  TITANIC DATA SET & NAIVE BAYES
# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

titanic= read.csv("Titanic.csv")
dim(titanic)
head(titanic)

attach(titanic)

table(Survived)

# (a) Compute the probabilities P(Y = 1) (survived) and P(Y = 0) (did not survive).

tprior <- table(Survived) # Number of ppl survived & not survived
tprior
tprior <- tprior/sum(tprior) # the probability scores
tprior


# (b) Compute the conditional probabilities P(Xi = xi|Y = 1) and P(Xi = xi|Y = 0):

classCounts <- table(titanic[,c("Survived", "Class")]); classCounts
classCounts <- classCounts/rowSums(classCounts); classCounts

genderCounts <- table(titanic[,c("Survived", "Sex")]); genderCounts
genderCounts <- genderCounts/rowSums(genderCounts) ; genderCounts

ageCounts <- table(titanic[,c("Survived", "Age")]); ageCounts
ageCounts <- ageCounts/rowSums(ageCounts); ageCounts


# (c) Predict survival for an adult female passenger in 2nd class cabin.

prob_survived <-
classCounts["Yes","2nd"]*
genderCounts["Yes","Female"]*
ageCounts["Yes","Adult"]*
tprior["Yes"]

# print(prob_survived)


prob_not_survived <-
classCounts["No","2nd"]*
genderCounts["No","Female"]*
ageCounts["No","Adult"]*
tprior["No"]


prob_survived
# print(prob_not_survived)
# print(c(prob_not_survived, prob_survived))
prob_survived/prob_not_survived


# (d) Compare your prediction above with the one performed by the naiveBayes()
#function in package `e1071'
library(e1071)

M1 <- naiveBayes(Survived ~., titanic)

test <- data.frame(Class="2nd", Sex="Female", Age="Adult")

results <- predict(M1,test)
results
results <- predict(M1,test, "raw")
# print(results)

#ratio of probability score
prob_survived/prob_not_survived

#ratio of actual probabilities
results[1,"Yes"]/results[1,"No"]

# conclude: same answer


##################### Q2: TITANIC DATA SET & NAIVE BAYES & LOGISTIC REGRESSION

# Naive Bayes classification
M1 <- naiveBayes(Survived ~., titanic)


# (a) Logistic regression
# response must be of 0 and 1 to fit the model

sur = (Survived == 'Yes')
titanic$sur = sur ; head(titanic)
M2 <- glm(sur~ Class + Sex + Age, data=titanic, family=binomial(link= "logit"))

summary(M2)

# (b) write down the fitted model, phat = predicted probability of survival:

# log[phat/(1-phat)] = 2.0438 -1.0181*I(Class = 2nd) -1.7778*I(Class = 3rd) 
#                   -0.8577* I(Class = Crew) -2.4201*I(Sex = Male) + 1.0615*I(Age = Child)

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


# (e) ROC & AUC
#different classfiers produce different performances
#plot 2 ROC curves in the same plane

library(ROCR)

# ROC for Logistic Regresison:
pred = predict(M2, type="response")
predObj = prediction(pred, titanic$Survived)
rocObj = performance(predObj, measure="tpr", x.measure="fpr")
plot(rocObj)

auc1 = performance(predObj , measure ="auc")
auc1@y.values[[1]] # 0.7597259


# ROC for Naive Bayes classifier
naiveB <- predict(M1, titanic[,1:3],type='raw')
score <- naiveB[, 2]
pred_nb <- prediction(score, titanic$sur)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb, add = TRUE, col = "red") #add = TRUE means plot the curve in the existing plot

auc2 <- performance(pred_nb , "auc")@y.values[[1]]
auc2 #0.7164944


legend("bottomright", c("logistic regression","naive Bayes"),col=c("black","red"), lty=1)





















