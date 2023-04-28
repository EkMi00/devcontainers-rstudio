# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")


x1 <- c(1, 1.5, 3, 3.5, 4.5)
x2 <- c(1, 2, 4, 5, 5)
data <- data.frame(x1, x2)

dist_sq <- function(p1, p2) {
    return (sqrt((p1[1] - p2[1])**2 + (p1[2] - p2[2])**2)**2)
}

# plot(x1, x2, pch = 20, col = "blue")

# Cluster P: (A, B), Cluster Q: (C, D, E)

# P new centroid: (A+B/(2)), Q new centroid: (C+D+E/3)
# New Cluster P: (A, B), Cluster Q: (C, D, E)
# Algorithm has converged

# WSS = SS_1 + SS_2
# = d(A, centroid_P)^2 + d(B, centroid_P)^2
# + d(C, centroid_Q)^2 + d(D, centroid_Q)^2 + d(E, centroid_Q)^2 
# = 0.625 + 1.833 = 2.458

dist_sq <- function(p1, p2) {
    return (sqrt((p1[1] - p2[1])**2 + (p1[2] - p2[2])**2)**2)
}

c1 <- c(1.25,1.5)
c2 <- c(3.67,4.67)
clusterP <- data[1:2,]
SS_1 <- sum(apply(clusterP, 1, function(x) {dist_sq(x, c1)}))   
clusterQ <- data[3:5,]
SS_2 <- sum(apply(clusterQ, 1, function(x) {dist_sq(x, c2)}))   
WSS <- SS_1 + SS_2 # 2.4584 

########################################################################
data <- read.csv("hdb-2012-to-2014.csv")
attach(data)
K <- 15
wss <- numeric(K)
for (i in 1:K) {
    wss[i] <- sum(kmeans(data[, c("floor_area_sqm", "resale_price")], centers=i)$withinss)
}

# plot(1:K, wss, col="blue", type="b", xlab="k-cluster value", ylab="WSS")
# Choose k=3 as it is at the "bend" of the curve
kout <- kmeans(data[,c("floor_area_sqm","resale_price")],centers=3)
print(kout$centers)
#   floor_area_sqm resale_price
# 1      108.85993     451901.1
# 2       80.72349     345664.3
# 3      142.34795     633996.8

plot(floor_area_sqm, resale_price, col=kout$cluster)

detach(data)