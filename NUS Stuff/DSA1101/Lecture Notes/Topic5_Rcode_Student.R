
# setwd("/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

bankdata = read.csv("bank-sample.csv", header=TRUE)
head(bankdata)
head(bankdata[,2:8])
head(bankdata[,c(9,16,17)])

#### Some SUMMARIES ###
table(bankdata$job)

table(bankdata$marital)

table(bankdata$education)

table(bankdata$default)

table(bankdata$housing)

table(bankdata$loan)

table(bankdata$contact)

table(bankdata$poutcome)


#######  DECISION TREE

#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
fit <- rpart(subscribed ~job + marital + education+default + 
housing + loan + contact+poutcome,
method="class",
data=bankdata,
control=rpart.control(minsplit=1),
parms=list(split='gini'))

# there is another argument in rpart.control, that is cp.
#smaller values of cp correspond to decision trees of larger sizes, 
#and hence more complex decision surfaces.


#method = "anova", "poisson", "class" or "exp"
# If response is a survival object, then method = "exp" is assumed, 
# if response has 2 columns then method = "poisson" is assumed, 
# if response is a factor then method = "class" is assumed, 
# otherwise method = "anova" is assumed
# minslpit = 1: a stem is created when data have at least one observation in that stem
# split = 'information' or 'gini'



# To plot the fitted tree:
rpart.plot(fit, type=4, extra=2)# can try with extra = 4 to see the difference
rpart.plot(fit, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE)

#varlen = length of variable's name,varlen = 0 means full name
#faclen = length of category's name, faclen = 0 means full name
#clip.right.labs: TRUE means: don't print the name of variable for the right stem




length(bankdata$poutcome)
table(bankdata$poutcome)

#### ENTROPY PLOT ####

p=seq(0,1,0.01)
D=-(p*log2(p)+(1-p)*log2(1-p))
# plot(p,D,ylab="D", xlab="P(Y=1)", type="l")


### Calculating conditional entropy when 'poutcome' is splitted 
# as x1 = failure, other, unknown and x2 = success

x1=which(bankdata$poutcome!="success")
x2=which(bankdata$poutcome=="success")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])

### Calculating conditional entropy when 'poutcome' is splitted 
# as x1 = success, other, unknown and x2 = failure
x1=which(bankdata$poutcome!="failure")
x2=which(bankdata$poutcome=="failure")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])


############  PLAYING GOLF EXAMPLE

library("rpart") # load libraries
library("rpart.plot")

play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
head(play_decision)

fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
method="class",
data=play_decision,
control=rpart.control(minsplit=1),
parms=list(split='information'))

# rpart.plot(fit, type=4, extra=2)


newdata <- data.frame(Outlook="rainy", Temperature="mild",
Humidity="high", Wind=FALSE)
newdata

predict(fit,newdata=newdata,type="prob")

predict(fit,newdata=newdata,type="class")


######



