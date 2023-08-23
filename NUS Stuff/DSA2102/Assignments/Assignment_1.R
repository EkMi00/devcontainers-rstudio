mymean <- function(vector) {
    return(sum(vector)/length(vector))
}

myvar1 <- function(vector) {
    mean <- mymean(vector)
    dev <- vector-mean
    return(sum((dev)*(dev))/(length(vector) - 1) )
}

myvar2 <- function(vector) {
    n <- length(vector)
    mean <- mymean(vector)
    return((sum(vector*vector) - n*mean*mean)/(n-1))
}


print_stats <- function(vector) {
    print(mymean(vector))
    print(myvar1(vector))
    print(myvar2(vector))
    # print(mean(vector))
    # print(var(vector))
}
set.seed(123)
v1 <- sample(-9:9, size=10)
v2 <- sample(-99:99, size=100)
# print_stats(v1)
# print_stats(v2)

v3 <- c(10000000.2, 10000000.1, 10000000.3, 10000000.1, 10000000.3, 10000000.1,
10000000.3, 10000000.1, 10000000.3, 10000000.1, 10000000.3) 
print_stats(v3)

