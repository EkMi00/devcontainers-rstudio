caravan= read.csv('C:/Caravan.csv')
head(caravan$Purchase)
summary(caravan$Purchase)
plot(caravan$Purchase)
caravan=caravan[,-1] #exclude ID column
standardized.X= scale(caravan[,-86])
var(caravan[,1])
var(caravan[,1])
var(standardized.X[,1]) #STANDARDIZED
var(standardized.X[,2])
#rest is the same as the previous example


