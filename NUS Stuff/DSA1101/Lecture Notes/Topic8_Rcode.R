###########  HDB FLATS GROUPING
# setwd("/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

# hdb=read.csv("C:/Data/hdbresale_cluster.csv")
hdb=read.csv("hdbresale_cluster.csv")
head(hdb)
dim(hdb)

table(hdb$flat_type)

set.seed(1)


plot(x=hdb$floor_area_sqm, y=hdb$amenities,
     xlab="Floor area in sqm", ylab="Number of amenities", col="red")

kout <- kmeans(hdb[,c("floor_area_sqm","amenities")],centers=2)

plot(hdb$floor_area_sqm, 
     hdb$amenities, 
     col=kout$cluster)


kout$cluster # A vector of integers (from 1:k) indicating the cluster to which each point is allocated.

kout$centers # A matrix of cluster centres.

kout$size # The number of points in each cluster.

kout$withinss # Vector of SS_k, one value per cluster

kout$tot.withinss # Total within-cluster sum of squares = WSS


# PLOT TO SEE HOW WSS CHANGES WHEN K CHANGES

K = 10 # WE'LL TRY WITH k = 1, ...10.

wss <- numeric(K)

for (k in 1:K) { 
   wss[k] <- sum(kmeans(hdb[,c("floor_area_sqm","amenities")],centers=k)$withinss )
}

# plot(1:K, wss, col = "red", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")








################## GRADE GROUPING

set.seed(1)
# grade = read.csv("C:/Data/grades_km_input.csv")
grade = read.csv("grades_km_input.csv")
head(grade)

attach(grade)

# VISUALIZE DATA SET BY FEATURES:
# plot(grade[,2:4])
# PROPOSE: MIGHT BE 3 OR 4 GROUPS


# kout <- kmeans(grade[,c("English","Math","Science")],centers=3)

# kout$withinss

# PLOT WSS vs K TO PICK OPTIMAL K:

K = 15 
wss <- numeric(K)

for (k in 1:K) { 
   wss[k] <- sum(kmeans(grade[,c("English","Math","Science")], centers=k)$withinss)
}


# plot(1:K, wss, col = "blue", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")

# comments:

# WSS is greatly reduced when $k$ increases from 1 to 2. 
# Another substantial reduction in WSS occurs at $k = 3$.

# However, the improvement in WSS is fairly linear for $k > 3$.
# Therefore, the $k$-means analysis will be conducted for $k = 3$.

# The process of identifying the appropriate value of k is
# referred to as finding the ``elbow'' of the WSS curve








