
####### PROBLEM 1  ###########


data<-read.table('C:/Data/crab.txt', header=T) # change to 'data1.txt'
head(data)
dim(data)


names(data)

attach(data)

### PART I: exploring variable satell


#Q. Histogram and normal density curve overlaying
hist(satell,prob = TRUE)
x= seq(0,16, length.out = 173)
y = dnorm(x, mean(satell), sd(satell))
lines(x,y, col = "red")
#comments:the range of histogram is from 0 to 15
#the histogram is right skewed. No gap in the data.


#Q. Boxplot

boxplot(satell)
out = boxplot(satell)$out; out
index =  which(satell %in% c(out))
data[index,]
# There are outliers above median. They are:
# color spine width satell weight
# 15     3     1  26.0     14    2.3
# 56     3     3  28.3     15    3.0


#Q. QQ plot
qqnorm(satell, datax = TRUE, pch = 20)
qqline(satell, datax = TRUE)
# comments: the left tail is obviously shorter than normal. 
# The sample of satell hence is NOT normally distributed.



## PART II


#Q. Create a new variable, col = light if color = 2,3 and col = dark otherwise.

n = length(satell)

data$col = numeric(n)

data$col[which(color <=3)] = "light"
data$col[which(color >3)] = "dark"

head(data)

attach(data)

data$col = as.factor(col)



#Q. frequency table for col

table(col) # 66 dark and 107 light


#Q. Scatter plot of weight and satell, classified by col:

plot(satell~weight,type = "n",xlab="Weight",ylab="Satell", main = "Crab Data")
points(satell[col=="light"]~weight[col=="light"], pch = 20, col = "red")
points(satell[col=="dark"]~weight[col=="dark"], pch = 2, col = "blue")
legend(4.5,15,legend=c("light", "dark"),col=c("red", "blue"), pch=c(20,2))

# comment: Overall, weight and satell has (quite weak) POSITIVE and possibly LINEAR relationship.
#this relationship might be quite WEAK
#Can see that for light color or for dark color, the relationship between response and Weight are 
#about similar, NO CLEAR DIFFERENCE.

##### PART III: MODELLING

#Q. linear model for satell
data$color = as.factor(data$color)
M = lm(satell ~ color + weight + width, data = data)

summary(M)

# R^2 = 0.1492
# Though F-test for the significance of the overall  model: model is significant
# However, R^2 is too low. It means model doesn't fit the data well.





###############  PROBLEM 2 ###########
set.seed(999)

hab<-read.csv('C:/Data/haberman.csv')# change to 'data2.csv'
head(hab)

dim(hab)
names(hab)[4] = "status"

hab$status = as.factor(hab$status)

hab[,1:3] = lapply(hab[ ,1:3],scale ) # SCALING THE FEATURES


attach(hab)

table(status)
# status: 1 = the patient survived 5 years or longer # negative test
#         2 = the patient died within 5 years  # positive test

# type 1 can be tolerated but not type 2.


library(class)

# use 3-fold CV to check the goodness of a classfier
# form a function to return: the best k for KNN, the coresponding accuracy & precision


X = hab[,c(1,3)] # we do not use the 2nd column, year.
Y=hab[,4] # response
n = length(Y) #= sample size


n_folds=3

folds_j <- sample(rep(1:n_folds, length.out = n ))

#table(folds_j)

K = 50


ave.type1 = numeric(K) # to store the accuracy for each k of KNN, k is from 1 to K = 50.
ave.type2 = numeric(K) # to store the precision for each k of KNN

type1=numeric(n_folds)
type2=numeric(n_folds)

for(i in 1:K) {

for (j in 1:n_folds) {

	test_j <- which(folds_j == j) # get the index of the points that will be in the test set
      test.y = Y[test_j] # response for the test points
	knn.pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=i) 
      confusion.matrix=table(test.y, knn.pred) # 2 = positive and 1 = negative

	type1[j]= confusion.matrix[1,2]/sum(confusion.matrix[1,]) # row 1 means actual negative
	type2[j]=confusion.matrix[2,1]/sum(confusion.matrix[2,]) # row 2 means actual positive
}
      ave.type1[i] = round(mean(type1), digits = 3)
      ave.type2[i] = round(mean(type2), digits = 3)

}


plot(ave.type1, ave.type2, pch = 20)
sort(ave.type2)[1:3]
index = which(ave.type2 %in% c(sort(ave.type2)[1:3])); index
ave.type1[index]









