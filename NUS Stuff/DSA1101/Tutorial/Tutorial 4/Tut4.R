library("ggplot2")
setwd("/mnt/c/Users/Keck/Documents/GitHub/myrepo/Rstudio Projects/NUS Stuff/DSA1101/Data");

colleges <- read.csv("Colleges.txt", sep = "\t" , header = T ,
                     na.strings ="", 
                     stringsAsFactors= F)

# print(head(colleges))

y <- (colleges$Acceptance)
x0 <- seq(1, 1, length.out=nrow(colleges))
x1 <- colleges$SAT

X_1 <- cbind(x0, x1)


X_1T <- t(X_1)
LRSS <- solve(X_1T%*%X_1, X_1T%*%y)
# print(LRSS) 
# b0 = 42732.0424
# b1 = -329.9377

x2 <- (colleges$Top.10p)  

X_2 <- cbind(x0, x1, x2)
X_2T <- t(X_2)
LRSS2 <- solve(X_2T%*%X_2, X_2T%*%y)

# print(LRSS2)

m2 <- lm(colleges$Acceptance ~ colleges$SAT + colleges$Top.10p, data = colleges)
# print(m2)

resale <- read.csv("house_selling_prices_FL.csv", sep = "," , header = T ,
                     na.strings ="", 
                     stringsAsFactors= F)

# print(head(resale))

price_size <- lm(resale$price ~ resale$size, data = resale)

# print(price_size)

my_graph <- ggplot(resale, aes(x = resale$size, y = resale$price)) + 
    geom_point() + 
    geom_smooth(method=lm , color="red", se=FALSE)
    # + stat_smooth()

# print(my_graph) # Postive assoication

rsq <- function(x, y) cor(x, y) ^ 2
# print(rsq(resale$size, resale$price))

model2 <- lm(resale$price ~ resale$size + resale$NW, data=resale)
# print(model2) #NW = 30569.08729 
b0 = model2$coefficients["(Intercept)"]
b1 = model2$coefficients["resale$size"]
b2 = model2$coefficients["resale$NW"]

projected_y <- b0 + b1 * (4000) + b2 * (1)

# print(projected_y)

print(summary(model2)) # 0.6276

