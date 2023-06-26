library("ggplot2")
setwd("/mnt/c/Users/Keck/Documents/GitHub/Devcontainers-Rstudio/NUS Stuff/DSA1101/Data");

##### 1 ######
colleges <- read.table("Colleges.txt", sep = "\t" , header = T)

# print(head(colleges))
matrix <- function(x, y) {
beta <- solve(t(x )%*% x )%*% t(x )%*% y
return( beta )
}
m1 <- lm(Acceptance ~ SAT, data = colleges)
print(m1)

LRSS <- matrix(cbind(1,dat$SAT), dat$Acceptance)
# print(LRSS) 
# b0 = 202.2677440
# b1 = -0.1300894

m2 <- lm(colleges$Acceptance ~ colleges$SAT + colleges$Top.10p, data = colleges)
# print(m2)



##### 2 #####

resale <- read.csv("house_selling_prices_FL.csv", sep = "," , header = T)

resale$NW = as.factor(resale$NW) # to declare that NW is categorical
attach(resale)
# print(head(resale))

price_size <- lm(resale$price ~ resale$size, data = resale)

my_graph <- ggplot(resale, aes(x = resale$size, y = resale$price)) + 
    geom_point() + 
    geom_smooth(method=lm , color="red", se=FALSE)
    # + stat_smooth()
# print(my_graph) # Postive assoication

rsq <- function(x, y) cor(x, y) ^ 2
# print(rsq(resale$size, resale$price))

model2 <- lm(price ~ size + NW, data=resale)
print(model2) #NW = 30569.08729 

print(predict(model2, newdata=data.frame(size=4000, NW = "1")))


print(summary(model2)) # 0.6276

