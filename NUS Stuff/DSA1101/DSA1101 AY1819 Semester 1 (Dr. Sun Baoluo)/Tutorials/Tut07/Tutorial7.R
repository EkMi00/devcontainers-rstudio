#(a) Load the dataset \Titanic.csv" which has been posted under the folder
#for Tutorial 7.

Titanic_dataset= read.csv("c:/Titanic.csv")
dim(Titanic_dataset)
head(Titanic_dataset)


#(b) Compute the probabilities P(Y = 1) (survived) and P(Y = 0) (did not
#survive).

tprior <- table(Titanic_dataset$Survived) #Number of ppl survived/ not survived
tprior
tprior <- tprior/sum(tprior) #the probability scores
tprior


#(c) Compute the conditional probabilities P(Xi = xijY = 1) and P(Xi =
#xi|Y = 0) , where i = 1; 2; 3; 4 for the feature variables X = fclass; sex; age
classCounts <- table(Titanic_dataset[,c("Survived", "Class")])
classCounts <- classCounts/rowSums(classCounts)
classCounts

genderCounts <- table(Titanic_dataset[,c("Survived", "Sex")])
genderCounts <- genderCounts/rowSums(genderCounts)
genderCounts

ageCounts <- table(Titanic_dataset[,c("Survived", "Age")])
ageCounts <- ageCounts/rowSums(ageCounts)
ageCounts


#(d) Predict survival for an adult female passenger in 2nd class cabin.

prob_survived <-
classCounts["Yes","2nd"]*
genderCounts["Yes","Female"]*
ageCounts["Yes","Adult"]*
tprior["Yes"]


prob_not_survived <-
classCounts["No","2nd"]*
genderCounts["No","Female"]*
ageCounts["No","Adult"]*
tprior["No"]


prob_survived
prob_not_survived

prob_survived/prob_not_survived


#(e) Compare your prediction in (d) with the one performed by the naiveBayes
#function in package `e1071'
library(e1071)

model <- naiveBayes(Survived ~.,
Titanic_dataset)

test <- data.frame(Class="2nd", Sex="Female",
Age="Adult")

results <- predict(model,test)
results
results <- predict(model,test, "raw")
results

#ratio of probability score
prob_survived/prob_not_survived

#ratio of actual probabilities
results[1,"Yes"]/results[1,"No"]




