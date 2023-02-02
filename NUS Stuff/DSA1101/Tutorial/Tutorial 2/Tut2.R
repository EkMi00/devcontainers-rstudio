# Response Variable: FEV
setwd("/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
fev <- read.csv("FEV.csv")
fev_freq <- fev$FEV
# hist(fev_freq,
#      freq = TRUE, main = paste("Histogram of FEV"),
#      xlab = "", ylab = "Frequency", axes = TRUE, col = "white"
# )


# boxplot(fev_freq, xlab = "FEV")


out <- boxplot.stats(fev_freq)$out
outliers <- which(fev_freq %in% c(out))
# print(outliers) # Rows: 321 452 464 517 609 624 632 648 649
# print(fev[outliers, ]) # Prints outlier rows


# qqnorm(fev_freq, main = "FEV QQ Plot", pch = 20)
# qqline(fev_freq, col = "red")


fev_male <- fev[fev$Sex == 1, ]$FEV
fev_female <- fev[fev$Sex == 0, ]$FEV
# hist(fev_male,
#     freq = TRUE, main = paste("Histogram of Male FEV"),
#     xlab = "", ylab = "Frequency", axes = TRUE, col = "blue"
# ) # Right Skew
# hist(fev_female,
#     freq = TRUE, main = paste("Histogram of Male FEV"),
#     xlab = "", ylab = "Frequency", axes = TRUE, col = "pink"
# ) # Left Skew


# plot(fev$height, fev_freq, pch = 20, col = "darkblue")
# print(cor(fev$height, fev_freq)) # r = 0.8675619


fib <- function(n) {
    if (n < 2) {
        return(n)
    }
    a <- 0
    b <- 1
    for (i in 2:n + 1) {
        c <- a + b
        a <- b
        b <- c
    }
    return(b)
}


fibos <- c()
for (i in 1:45) {
    fibos <- append(fibos, fib(i))
}
print(fibos)


print(fib(40)) # 102334155


i <- 1
while (fib(i) < 5000000) {
    i <- i + 1
}
print(i)
# Smallest n such that Fn > 5000000: n = 34
# print(fib(34))
# print(fib(33))
