################ 5b ################
my_sum <- function(vector) {
    result <- 0
    for (v in vector) {
        result <- result + v
    }
    return(result)
}

my_length <- function(vector) {
    count <- 0;
    for (v in vector) {
        count <- count + 1
    }
    return (count)
}

mymean <- function(vector) {
    return(my_sum(vector)/my_length(vector))
}

myvar1 <- function(vector) {
    mean <- mymean(vector)
    dev <- vector-mean
    return(my_sum((dev)*(dev))/(my_length(vector) - 1) )
}

myvar2 <- function(vector) {
    n <- my_length(vector)
    mean <- mymean(vector)
    return((my_sum(vector*vector) - n*mean*mean)/(n-1))
}

################ 5c ################
print_stats <- function(vector) {
    print(mymean(vector))
    print(myvar1(vector))
    print(myvar2(vector))
}
set.seed(123)
v1 <- sample(-9:9, size=10);
v2 <- sample(-99:99, size=100);
print_stats(v1)
print_stats(v2)

v3 <- c(10000000.2, 10000000.1, 10000000.3, 10000000.1, 10000000.3, 10000000.1,
10000000.3, 10000000.1, 10000000.3, 10000000.1, 10000000.3)
print_stats(v3)
