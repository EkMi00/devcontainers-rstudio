if(!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

msft <- read.csv("MSFT.csv") %>%
    mutate(Tmr = lead(Close),
        R = log(Tmr/Close)) %>% slice(-length(R))
r_bar = msft %>% summarise(mean = mean(R)) %>% .$mean
r_bar # 0.000869503
r_var = msft %>% summarise(var = var(R)) %>% .$var
r_var # 0.0004519334
n = nrow(msft)

# a) 
H_0 = 0
data <- msft %>% arrange(R) %>% 
    select(R) %>%
    mutate(order = 1:length(R),
        diff = sapply(R, FUN = function(x) x - H_0),
        sign = sign(diff)) %>% group_by(sign) %>% count()
n_neg = data[1,]$n; n_neg
n_pos = data[3,]$n; n_pos
p_val = pbinom(n_pos, n, prob=0.5, lower.tail=FALSE) + dbinom(n_pos, n, prob=0.5); p_val 
# p-value = 0.2038856 > 0.05, thus we fail to reject H_0 at 5% significance level.

# b) 
t_stat = (r_bar - H_0)/sqrt(r_var/n); t_stat
t_score = 1.645
# Test-statistic, t = 4.002032 > 1.645
# thus reject H_0 at 5% significance level

# c)
wilcox <- msft %>% select(R) %>% arrange(R) %>%
    mutate(order = 1:length(R), diff = R - 0,
    sign = sign(diff), abs_diff = abs(diff)) %>% 
    filter(diff != 0) %>% arrange(abs_diff) %>%
    mutate(Rank = rank(abs_diff), Sign_Rank = sign*Rank) %>% 
    arrange(order)
head(wilcox)

w_stat = wilcox %>% 
    summarise(w = pnorm((sum(Sign_Rank) + 1)/
    sqrt(nrow(wilcox)*(nrow(wilcox) + 1)*(2*nrow(wilcox) + 1)/6), lower.tail=FALSE)) %>% .$w
w_stat # 2.378469e-06 < 0.05, we reject H_0 at 5% significance level


