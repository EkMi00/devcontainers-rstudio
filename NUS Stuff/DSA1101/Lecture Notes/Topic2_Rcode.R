####  TOPIC 2: BASIC PROBABILITY & STATISTICS
library(dplyr)
library(ggplot2)
# setwd("C:/Data")
# setwd("/mnt/c/Users/Keck/Documents/GitHub/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
sales <- read.csv("yearly_sales.csv")

head(sales)
total <- sales$sales_total

n <- length(total)
n
summary(total)


range(total)
var(total)
sd(total)
IQR(total)
print(total[order(total)[1:5]]) # The 5 smallest observations

total[order(total)[(n - 4):n]] # The 5 largest observations

# HISTOGRAM
# hist(total,
#      freq = TRUE, main = paste("Histogram of Total Sales"),
#      xlab = "total", ylab = "Frequency", axes = TRUE, col = "blue"
#      )    

hist_plot <- ggplot(sales, aes(x = sales_total)) +
  geom_histogram(bins = 10)  # bin = 30 is default +
  labs(x = "Total Sales", y = "Frequency", title = "Histogram of Total Sales")
# +  facet_wrap(~ gender)

# print(hist_plot)

# HISTOGRAM WITH DENSITY LINE
# hist(total,
#      freq = FALSE, main = paste("Histogram of total sales"),
#      xlab = "total", ylab = "Probability", axes = TRUE,
#      col = "blue", ylim = c(0, 0.0045)
# )
# lines(density(total), col = "red") # this is the density curve of "total"

# HISTOGRAM WITH NORMAL DENSITY
# hist(total,
#      freq = FALSE, main = paste("Histogram of Total Sales"),
#      xlab = "total sales", ylab = "Probability", axes = TRUE,
#      col = "grey", ylim = c(0, 0.002)
# )
# x <- seq(0, max(total), length.out = n) # x-axis
# y <- dnorm(x, mean(total), sd(total)) # normal distribution
# lines(x, y, col = "red") # this is the normal density curve


# BOX PLOTS
# boxplot(total, xlab = "Total Sales")

boxplot <- ggplot(sales, aes(x = "", y = sales_total)) + 
     geom_boxplot(outlier.shape=NA) +
     scale_y_continuous(limits = quantile(sales$sales_total))

b_out <- boxplot(total, xlab = "Total Sales")$out
index <- which(total %in% c(b_out))
print(sales[c(index), ], )
# print(boxplot)


# QQ plot
# qqnorm(total, main = "QQ Plot", pch = 20)
# qqline(total, col = "red")



# CORRELATION COEFFICIENT
order <- sales$num_of_orders
cor(total, order) # 0.75


# SCATTER PLOT
# plot(order, total, pch = 20, col = "darkblue")


# BOX PLOTS OF MULTIPLE GROUP
# boxplot(total ~ sales$gender)



# 3 VARIABLES = SCATTER PLOT ADDING LEGEND
order <- sales$num_of_orders
attach(sales)

plot(order, total, type = "n") # a scatter plot with no point added

# points(order[gender == "M"], total[gender == "M"], pch = 2, col = "blue") # MALE
# points(order[gender == "F"], total[gender == "F"], pch = 20, col = "red") # FEMALE
# legend(1, 7500, legend = c("Female", "Male"), col = c("red", "blue"), pch = c(20, 2))

# (x = 1, y =7500) tells R the place where you want to put the lengend box in the plot
# do note on the size of the points since the points added latter will overlay on the points added earlier
# hence, the points added latter should be chosen with smaller size so that they will not cover the points earlier


# BARPLOT FOR CATEGORICAL VARIABLE
count <- data.frame(table(gender))
gender_levels <- factor(sales$gender,
     levels = c("F", "M"))
count # frequency table
# barplot(count)
barplot <- ggplot(sales, aes(x = order, fill = gender)) + 
  geom_bar(position = "dodge") + 
  facet_wrap(~ gender)

# print(barplot)

# PIE CHART
count2 <- table(gender_levels)
# piechart <- ggplot(sales, aes(x = '' )) + 
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0)
# print(piechart)

# pie(count2)


# CATEGORIZING "ORDER"
order <- sales$num_of_orders
order.size <- ifelse(order <= 5, "small", "large")
table(order.size)

# CONTINGENCY TABLE
table <- table(gender, order.size)
table
tab <- prop.table(table, "gender") # proportion by gender
tab
tab[1] / (1 - tab[1]) # the odds of large order among FEMALES
tab[2] / (1 - tab[2]) # the odds of large order among MALES
OR <- (tab[1] / (1 - tab[1])) / (tab[2] / (1 - tab[2]))
OR # 0.76
# it means: the odds of larger orders among females is 0.76 times the odds of large orders among males.


########### USER DEFINED FUNCTION IN R

# Function for finding OR in general for a matrix x of 2x2:

OR <- function(x) {
     if (any(x == 0)) {
          x <- x + 0.5
     }
     odds.ratio <- x[1, 1] * x[2, 2] / (x[2, 1] * x[1, 2])

     return(odds.ratio)
}



OR(table)
