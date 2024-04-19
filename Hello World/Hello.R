# setwd("C:\\Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# # setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA2101\\tutorial")
# "Hello"
# 8 + 3

# foo <- function() {
#     print("kill me")
# }
# foo()

# x = c(31, 41, 59, 26, 53, 59, 47, 43, 23, 34, 42, 44)
# var(x)

# a = c(21, 22, 18, 29, 35, 20, 30, 32, 26, 23, 19, 29, 31, 28, 27)

# mean(a)
# var(a)

# install.packages("tidytuesdayR")
# # update.packages(ask = FALSE)
# install.packages("installr")
# library(installr)
# updateR()
# install.packages("devtools")
# library(devtools)


library(tidyverse)
x = tibble(c(340, 395, 358, 268))
x %>% mutate(res = ((x - 340.25)**2)/340.25) %>%
    summarize(test = sum(res)) %>% .$test
chisq.test(x)

x1 = matrix(c(45, 68, 45, 25, 15, 5, 11, 8, 3, 10, 5, 0), ncol = 4, nrow = 3)
rownames(x1) <- c('Soccer', 'NS_athletes', 'Non_athletes')
colnames(x1) <- c('c0', 'c1', 'c2', 'c3')
x1
chisq.test(x1)

pnorm(0)-pnorm(-1)
