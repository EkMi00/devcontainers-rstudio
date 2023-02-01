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

# fev_male <- filter(fev, "FEV" >= "1",)
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

# library("r2r")
table <- hashmap()
fib <- function(n) {
    str_n <- toString(n)
    helper <- function(n) {
        if (n < 2) {
            return(n)
        } else if (n %in% names(table)) {
            return(table[[str_n]])
        } else {
            table[[str_n]] <- (fib(n - 1) + fib(n - 2))
            return(table[[str_n]])
        }
    }
    return(helper(n))
}
fibos <- c()
for (i in 1:20) {
    fibos <- append(fibos, fib(i))
}
print(fibos)

# 40th Fn = 63245986
# Smallest n such that Fn > 5000000: n = 34
