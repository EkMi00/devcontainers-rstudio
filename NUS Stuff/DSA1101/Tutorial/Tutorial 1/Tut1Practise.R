# Q1

num_months <- function(salary) {
    price <- 1200000
    downPay <- 0.25 * 1200000
    saved <- 10000
    portion <- 0.40
    invest <- 1.02
    month_count <- 0
    
    while (saved < downPay) {
      month_count <- month_count + 1
      saved <- invest * saved + portion * salary
    }
    return(month_count)
}
print(num_months(7000))
print(num_months(10000))

#Q2
num_months <- function(salary, rate) {
  price <- 1200000
  downPay <- 0.25 * 1200000
  saved <- 10000
  portion <- 0.40
  invest <- 1.02
  month_count <- 0
  rate <- 1 + rate
  
  while (saved < downPay) {
    month_count <- month_count + 1
    saved <- invest * saved + portion * salary
    if(month_count %% 4 == 0) {
      salary <- salary * rate
    }
  }
  return(month_count)
}

print(num_months(7000, 0.02))
print(num_months(10000, 0.01))