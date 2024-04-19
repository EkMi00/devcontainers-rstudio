if(!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

cat = c('-1e+99,-0.001', '-0.001, -0.0004',
    '-0.0004, 0', '0, 0.0004', 
    '0.0004, 0.001', '0.001,1e+99')

msft <- read.csv("MSFT.csv") %>%
    mutate(Tmr = lead(Close),
        R = log(Tmr/Close),
        categories = case_when(
            R < -0.001 ~ cat[1],
            R >= -0.001 & R < -0.0004 ~ cat[2],
            R >= -0.0004 & R < 0 ~ cat[3],
            R >= 0 & R < 0.0004 ~ cat[4],
            R >= 0.0004 & R < 0.001 ~ cat[5],
            R >= 0.001  ~ cat[6]
        ),  categories = factor(categories, levels = cat)) %>%
        na.omit %>% arrange(R)

n = nrow(msft)

# a)
mu = 0; sd = 0.02

table1 = msft %>% group_by(categories) %>%
    summarize(observed = n()) %>% 
    mutate(LB = str_split(categories, ',', simplify = TRUE),
    UB = as.numeric(LB[,2]), LB = as.numeric(LB[,1]),
    expected = n*(pnorm(UB, mu, sd) - pnorm(LB, mu, sd))) ; table1
#   categories      observed     LB     UB expected
#   <fct>              <int>  <dbl>  <dbl>    <dbl>
# 1 -1e+99,-0.001       4340 -1e+99 -1e- 3   4596.
# 2 -0.001, -0.0004      136 -1e- 3 -4e- 4    115.
# 3 -0.0004, 0            60 -4e- 4  0         76.4
# 4 0, 0.0004            289  0      4e- 4     76.4
# 5 0.0004, 0.001        132  4e- 4  1e- 3    115.
# 6 0.001,1e+99         4617  1e- 3  1e+99   4596. 

test1 = table1 %>% 
    mutate(res = ((observed - expected)^2)/expected) %>%
    summarize(test = sum(res)) %>% .$test; test1
# Test-statistic = 616.3996
df1 = nrow(table1) - 1 ; df1
# Degreee of freedom = 5

chi_crit1 = 11.07

test1 > chi_crit1 # Reject

# Since test-statistics = 616.3996 > chi_crit = 11.07, we reject 
# H_0 at 5% significance level

# b) 
mu_mle = msft %>% summarise(mean = mean(R)) %>% .$mean
mu_mle # 0.000869503
sd_mle = sqrt(msft %>% summarize(var = var(R)*(n - 1)/(n)) %>% .$var)
sd_mle # 0.0004518862


table2 = msft %>% group_by(categories) %>%
    summarize(observed = n()) %>% 
    mutate(LB = str_split(categories, ',', simplify = TRUE),
    UB = as.numeric(LB[,2]), LB = as.numeric(LB[,1]),
    expected = n*(pnorm(UB, mu_mle, sd_mle) 
        - pnorm(LB, mu_mle, sd_mle))); table2
#   categories      observed     LB     UB expected
#   <fct>              <int>  <dbl>  <dbl>    <dbl>
# 1 -1e+99,-0.001       4340 -1e+99 -1e- 3   4452.
# 2 -0.001, -0.0004      136 -1e- 3 -4e- 4    108.
# 3 -0.0004, 0            60 -4e- 4  0         71.8
# 4 0, 0.0004            289  0      4e- 4     71.8
# 5 0.0004, 0.001        132  4e- 4  1e- 3    108. 
# 6 0.001,1e+99         4617  1e- 3  1e+99   4764.

test2 = table2 %>% 
    mutate(res = ((observed - expected)^2)/expected) %>%
    summarize(test = sum(res)) %>% .$test; test2
# Test-statistic = 678.7558
df2 = nrow(table2) - 1 - 2; df2
# Degreee of freedom = 3

chi_crit2 = 7.815

test2 > chi_crit2 # Reject

# Since test-statistic = 678.7558 > chi_crit = 7.815, we reject 
# H_0 at 5% significance level