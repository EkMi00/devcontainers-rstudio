# setwd("C:\\Users\\Keck\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
setwd("C:\\Users\\five8\\Documents\\GitHub\\devcontainers-rstudio\\NUS Stuff\\DSA1101\\Data")
colleges <- read.table("Colleges.txt", header=TRUE, sep="\t")

simple <- function(x,y) {
  beta_1 <- (sum(x*y)- mean(y)*sum(x))/(sum(x**2)- mean(x)*sum(x));
  beta_0 <- mean(y)- beta_1* mean(x);
  return(c( beta_0 , beta_1)) ;
}

attach(colleges)
print(simple(SAT, Acceptance))
print(lm(Acceptance ~ SAT, data=colleges))

F2 <- function(salary, price = 1200000, rate = 0.01, portion_save = 0.4) {
  r = 0.02 #monthly rate return from investment
  saved <- 10000 # savings given by parents initially
  month <- 0
  cost = 0.25*price
  while(saved < cost) {
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month %% 4 == 0){salary = salary * (1 + rate)} 
  }
  return(month)
}

F3 <- function(salary, price) {
  rate = 0.01
  proportion <- seq(0.01, 1, by=0.01)
  i = 1
  num_months <- F2(salary, price, rate, 0)
  while(num_months > 12 * 5) {
    portion_saved <- proportion[i]
    num_months <- F2(salary, price, rate, portion_saved)
    if (num_months > 12 * 5) {i <- i + 1}
  }
  return(proportion[i])
}

print(F3(price = 1200000, salary = 7000)) # answer: 0.32
print(F3(price = 800000, salary = 4000)) # answer: 0.35