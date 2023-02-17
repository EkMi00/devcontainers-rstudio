# 1)
num_of_years <- function(salary, start, price, rate) {
    # # 1i)
    downPay <- 0.25 * price
    portion_saved <- 0.4
    with_interest <- 1.02
    totalSaved <- start
    numMonths <- 0
    while (totalSaved < downPay) {
        numMonths <- numMonths + 1
        totalSaved <- totalSaved * with_interest + salary * portion_saved
        # #1ii)
        if (numMonths %% 4 == 0) {
            salary <- salary * (1 + rate)
        }
    }
    return(numMonths)
}

price <- 1200000
start <- 10000
print(num_of_years(7000, start, price, 0))
print(num_of_years(10000, start, price, 0))
print(num_of_years(7000, start, price, 0.02))
print(num_of_years(10000, start, price, 0.01))
