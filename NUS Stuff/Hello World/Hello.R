# setwd("C:/Data")
# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
# setwd("/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/Data")
# setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")

print("Hello")
print(8 + 3)

foo <- function() {
    print("fuck me")
}
foo()

v1 <- c(1, 2, 3)
x <- cbind(1, v1)
# print(x)

x = c( -1, 0, 1, 2)
y = c( -10, -4 , 0, 14) 
print(lm(y~x))
x_2 = x^2
x = cbind(x, x_2)
print(lm(y~x))
