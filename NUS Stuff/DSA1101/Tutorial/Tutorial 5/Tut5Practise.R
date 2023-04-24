# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

crab <- read.csv('crab.csv')
crab$spine <- factor(crab$spine)
attach(crab)

plot(width, weight, type='n')
points(width[spine==1], weight[spine==1], col='red', pch = 20)
points(width[spine==2], weight[spine==2], col='blue', pch = 6)
points(width[spine==3], weight[spine==3], col='green', pch = 10)
legend(22, 5, legend=c("Spine 1", "Spine 2", "Spine 3"),
       col=c("red", "blue", "green"), pch=c(20, 6, 10)) # Coordinate for legend

M <- lm(weight ~ width + spine, data=crab)
summary(M)$r.squared
summary(M)$adj.r.squared

# For spine good condition: weight = −3.93 + 0.244width + 0.05540 - 0.0697(0)
# For spine broken: weight = −3.93 + 0.244width  + 0.0554(0) - 0.0697
# Diff = 0.0697

predict(M, newdata=data.frame(width=27,spine="3"))
predict(M, newdata=data.frame(width=27,spine="2"))



