library("ggplot2")
setwd("/mnt/c/Users/Keck/Documents/GitHub/myrepo/Rstudio Projects/NUS Stuff/DSA1101/Data");

colleges <- read.csv("Colleges.txt", sep = "\t" , header = T ,
                     na.strings ="", 
                     stringsAsFactors= F)
# str(colleges)
# head(colleges)
my_graph <- ggplot(colleges, aes(x = colleges$SAT, y = colleges$Acceptance)) +
    geom_point() +
    geom_smooth(method=lm , color="red", se=FALSE) +
    stat_smooth()

print(my_graph)

# M1 <- lm(colleges$Acceptance ~ colleges$SAT, data = colleges) # y = -329.9x + 42732
# print(M1) 

F3 <- function(salary, start, price, rate, years) {
    mini <- list()
    # print(mini)
    for (portion_saved in 40: 100) {
        months <- years * 12
        downPay <- 0.25 * price
        # portion_saved <- 0.4
        with_interest <- 1.02
        numMonths <- 0
        totalSaved <- start
        while (totalSaved < downPay) {
            if (numMonths >= months) {
                break
            }
            numMonths <- numMonths + 1
            totalSaved <- totalSaved * with_interest + salary * (portion_saved / 100)
            if (numMonths %% 4 == 0) {
                salary <- salary * (1 + rate)
            }
        }
        mini[toString(portion_saved)] <- numMonths 
    }
    # print(unlist(mini))
    minimum <- which.min(unlist(mini))
    return(minimum)
}

start <- 10000
print(F3(7000, start, 1200000  , 0.01, 5))
print(F3(4000, start, 800000, 0.01, 5))



