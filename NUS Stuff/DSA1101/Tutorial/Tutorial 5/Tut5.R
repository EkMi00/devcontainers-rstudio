setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
caravan = read.csv("Caravan.csv")
crab = read.csv("crab.csv")
attach(crab)
# plot(crab$width, crab$weight)
# plot(crab$width, crab$weight, type = "n")
# points(crab$weight[crab$spine==1] ~ crab$width[crab$spine==1], col = "red", pch = 20)
# points(crab$weight[crab$spine==2] ~ crab$width[crab$spine==2], col = "darkblue", pch = 20)
# points(crab$weight[crab$spine==3] ~ crab$width[crab$spine==3], col = "darkgreen", pch = 20)
# legend(1.2, 5, legend = c(1, 2, 3), col = c("red","darkblue", "darkgreen"), pch=c(20,20))

M1 <- lm(weight ~ width + spine, data=crab)
corr <- cor(crab$weight, crab$width)
r_squared <- summary(M1)$r.squared
adj_r_squared <- summary(M1)$adj.r.squared
# Formula : weight = -3.78909 + 0.24067(width) - 0.04142(spine)
formula <- (sprintf("Formula : weight = %f + (%f)(width) + (%f)(spine)",
    M1$coefficients[1], M1$coefficients[2], M1$coefficients[3]))
# crab$spine <- as.factor(crab$spine)
# Rmb to attach as the names need to match
# print(predict(M1, newdata=data.frame(width = 27, spine = 3))) # y_hat = 2.584627
detach(crab)
##############################################

euc_dist <- function(x, y) sqrt(sum((x - y)^2))
training <- rbind(c(0,3,0),
    c(2,0,0),
    c(0,1,3),
    c(0,1,2),
    c(-1,0,1),
    c(1,1,1))

Y_values <- c()
for (i in 1:nrow(training)) {
    row <- training[i,]
    Y_values <- append(Y_values, (euc_dist(c(0,0,0) , row)))
}

k = 1
Y_values2 <- Y_values
i_of_nearest <- which(Y_values == sort(Y_values2)[1:k])
# index = 5, Green
k = 3
Y_values2 <- Y_values
values <- sort(Y_values2)[1:k]
# i_of_nearest <- which(Y_values == sort(Y_values2)[1:k])
# print(Y_values[i_of_nearest])
# Red = 0, Green = 1
# index = [5, 6, 2] (Green + Red + Red)/3 = 1/3 < 0.5, so Red


# If Bayes Decision Boundary is non-linear, we should use a small k value
# as it allows for high variance, which would allow the fitted values
# to vary greatly when compared to a linear model, which would thus
# produce a more non-linear model.


##############################################
training2 <- data.frame(rbind(c(1, 0.9),
    c(1, 0.5),
    c(0, 0.7),
    c(1, 0.4),
    c(1, 0.5),
    c(0, 0.2),
    c(0, 0.7),
    c(1, 0.9),
    c(0, 0.1),
    c(0, 0.1)))

classify <- function(data, dell) {
    tp <- 0
    fp <- 0
    tn <- 0
    fn <- 0
    for (i in 1:nrow(data)) {
        row <- data[i,]
        if (row[2] > dell & row[1] != 0) {
            tp <- tp + 1
        } else if (row[2] < dell & row[1] != 0) {
            fn <- fn + 1
        } else if (row[2] > dell & row[1] == 0) {
            fp <- fp + 1
        } else {
            tn <- tn + 1
        }

    }
    return((matrix(c(tp, fn, fp, tn), nrow = 2)))
}


prates <- function(cmatrix) {
    tp <- cmatrix[1,1]
    fp <- cmatrix[2,1]
    tn <- cmatrix[1,2]
    fn <- cmatrix[2,2]
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    return(c(tpr, fpr))
}

# cmatrix1 <- classify(training2, 0.3)
# rates1 <- prates(cmatrix)
# print(classify(training2, 0.6))
# print(classify(training2, 0.8))
cmatrix <- rbind(prates(classify(training2, 0.3)),
    prates(classify(training2, 0.6)),
    prates(classify(training2, 0.8)))

tpr <- cmatrix[,1]
fpr <- cmatrix[,2]

# plot(fpr, tpr) # negative association




##############################################
caravan <- read.csv("Caravan.csv");
# Based on cursary look, unlikely to purchase

attach(caravan)

library(class)
X <- scale(caravan[, 2:86]) # explanatories
Y <- caravan[87] # response

set.seed(1)
indices <- sample(nrow(X), size = 1000)
train <- caravan[-indices,]
test <- caravan[indices,]
train.x <- train[, 2:86]
train.y <- train[, 87]
test.x <- test[, 2:86]
test.y <- test[, 87]


knn_k <- function(k_value) {
    knn.pred <- knn(train.x, test.x, train.y, k=k_value)
    confusion.matrix <- table(knn.pred, test.y) 
    accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
    return(list(confusion.matrix, accuracy))
}
# print(knn_k(1))
# print(knn_k(3))
# print(knn_k(5))
# print(knn_k(10))


nf_x_valid <- function(n_folds, k_value) {
    err=numeric(n_folds)
    acc=numeric(n_folds)
    folds_j <- sample(rep(1:n_folds, length.out = dim(caravan)[1]))
    X <- scale(caravan[, 2:86]) # explanatories
    Y <- caravan[87] # response
    for (j in 1:n_folds) {
        test_j <- which(folds_j == j) # get the index of the points that will be in the test set
        pred <- knn(train=X[-test_j,], test=X[test_j,], cl=Y[-test_j,], k=k_value) # KNN with k = 1, 5, 10, etc
        # what is the tiebreaker for even k?
        # do we sample then split or other way around?
        
        err[j]=mean(Y[test_j,] != pred)
        acc[j]=mean(Y[test_j,] == pred) 
        # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix),
        # where confusion.matrix=table(Y[test_j],pred)
    }
    error=mean(err);
    accur=mean(acc);
    return(list("error" = error, "accuracy" = accur))
}

# print(nf_x_valid(20, 1))
# # $error
# # [1] 0.1111295

# # $accuracy
# # [1] 0.8888705

# print(nf_x_valid(20, 3))
# # $error
# # [1] 0.07608859

# # $accuracy
# # [1] 0.9239114

# print(nf_x_valid(20, 5))
# # $error
# # [1] 0.06613002

# # $accuracy
# # [1] 0.93387

print(nf_x_valid(20, 10))
# $error
# [1] 0.06046168

# $accuracy
# [1] 0.9395383

detach(caravan)


