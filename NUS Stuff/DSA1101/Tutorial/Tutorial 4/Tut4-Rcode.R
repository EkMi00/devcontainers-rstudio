
########### TUTORIAL 3 ##########


##########   Q1
dat= read.table("/mnt/c/Users/Keck/Documents/GitHub/Devcontainers-Rstudio/NUS Stuff/DSA1101/Data/Colleges.txt",header =TRUE,sep= "\t")
names(dat)
head(dat)

# print(dat)
matrix <- function(x, y) {
beta <- solve(t(x )%*% x )%*% t(x )%*% y
return( beta )
}
matrix( x = cbind (1,dat$SAT),y = dat$Acceptance )

m1 <- lm(dat$Acceptance ~dat$SAT , data =dat )
# print(m1)

matrix( cbind (1, dat$SAT ,dat$Top.10p), dat$Acceptance )
# Compare outputs with lm()
m2 <- lm(Acceptance ~ SAT +Top.10p, data = dat )
# print(m2)


##########   Q2

house = read.csv("/mnt/c/Users/Keck/Documents/GitHub/Devcontainers-Rstudio/NUS Stuff/DSA1101/Data/house_selling_prices_FL.csv")
# names(house) # names of columns
# dim(house) # 100 observations and 9 columns
house$NW = as.factor(house$NW) # to declare that NW is categorical
attach(house)


#(a)
cor(price, size)

#(b)
# plot(size, price, pch = 20)

#(c)
M1 = lm(price ~ size, data = house)
summary(M1)


#(d)
M2 = lm(price ~ size + NW, data = house)
print(summary(M2))
print(M2)


#(f)
print(predict(M2, newdata=data.frame(size=4000, NW = "1")))







