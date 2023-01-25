# training set
banktrain <- read.table("c:/bank-sample.csv",header=TRUE,sep=",")
# drop a few columns
drops <- c("balance", "day", "campaign", "pdays", "previous", "month")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]
# testing set
banktest <- read.table("c:/bank-sample-test.csv",header=TRUE,sep=",")
banktest <- banktest [,!(names(banktest) %in% drops)]

head(banktrain)
library(e1071)

# build the naïve Bayes classifier
nb_model <- naiveBayes(subscribed~.,
data=banktrain)
# perform on the testing set
nb_prediction <- predict(nb_model,
# remove column "subscribed"
banktest[,-ncol(banktest)],
type='raw') 
#raw conditional probability,
#R will vary the threshold automatically for optimal TPR and FPR

library(ROCR)

# can plot ROC for decision trees, kNN
# plot ROC
score <- nb_prediction[, c("yes")] #extract second column

actual_class <- banktest$subscribed =='yes'
pred <- prediction(score, actual_class)

perf <- performance(pred, "tpr", "fpr")
#compute tpr and fpr of pred for each threshold

plot(perf, lwd=2, xlab="False Positive Rate (FPR)",
ylab="True Positive Rate (TPR)")
abline(a=0, b=1, col="gray50", lty=3)
#certain areas are flat -> change threshold doesn't change TPR, FPR.

# compute AUC
# can also apply for N-Fold Cross Validation
# calculate AUC for test dataset, repeat 10 times, final AUC = mean of AUCs
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc

