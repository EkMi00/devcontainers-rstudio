# 1)
numOfMonths <- function(salary, start, price, rate) {
  # # 1i)
  downPay <- 0.25 * price
  setAside <- 0
  withInterest <- start
  totalSaved <- 0
  numMonths <- 0
  while (totalSaved < downPay) {
    numMonths <- numMonths + 1
    setAside <- setAside + 0.4 * salary
    withInterest <- withInterest * 1.02
    totalSaved <- setAside + withInterest
    # #1ii)
    if (numMonths %% 4 == 0) {
      salary <- salary * (1 + rate)
    }
  }
  return(numMonths)
}

price <- 1200000
salary <- 10000
print(numOfMonths(7000, salary, price, 0))
print(numOfMonths(10000, salary, price, 0))
print(numOfMonths(7000, salary, price, 0.02))
print(numOfMonths(10000, salary, price, 0.01))
