# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

# Q1
x1 = c(1, 1.5, 3, 3.5, 4.5)
x2 = c(1, 2, 4, 5, 5)

plot(x1, x2, pch = 20, col = "blue")

text(1.1, 1.1,"A")
text(1.6, 2.2, 'B')
text(3.1, 4.1, 'C')
text(3.63, 5, 'D')
text(4.35, 5, 'E')

# Adding the starting centroids 
points(2,2, pch = 2, col = 'red')
text(2.2, 2.1, 'C-P')
points(4,4, pch = 10, col = 'darkgreen')
text(4,3.8, 'C-Q')

# Adding the new centroids after the first iteration:
points(1.25, 1.5, col = 'red', pch = 2)
text(1.35, 1.4, 'C-P-new')
points(11/3, 14/3, col = 'darkgreen', pch = 10)
text(11/3, 4.5, 'C-Q-new')



# Q2

data = read.csv("hdb-2012-to-2014.csv")

dim(data)
names(data)

attach(data)

plot(floor_area_sqm, resale_price, pch = 20)


# PLOT WSS vs K TO PICK OPTIMAL K:

K = 15 
wss <- numeric(K)

for (k in 1:K) { 
   wss[k] <- sum(kmeans(data[,c("floor_area_sqm","resale_price")], centers=k)$withinss)
}


plot(1:K, wss, col = "blue", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")

# k=3 might be a good choice.


# k = 3 groups
kout <- kmeans(data[,c("floor_area_sqm","resale_price")],centers=3)

# visualize the 3 groups:

# plot(data$floor_area_sqm, 
#      data$resale_price, 
#      col=kout$cluster)








