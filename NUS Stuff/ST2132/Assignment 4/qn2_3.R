library(tidyverse)
library(BSDA)
library(stats)

H_0 = 8
x1 =c(5.2, 11.8, 7.1, 14.4, 6.3, 7.5, 11.1, 5.5, 9.8, 12.7, 10.5, 10.9, 8.7, 9.2)

# binom.test(sum(x1 < 8), 14, p=0.5, alternative=c('greater'), conf.level=0.95)$p.value
SIGN.test(x1, md=8, alternative='less', conf.level=0.95)$p.value

df = tibble(x = sort(x1)) %>%
    mutate(order = 1:14,
    diff = x - 8,
    sign = sign(diff),
    abs_diff = abs(diff)) %>%
    arrange(abs_diff) %>%
    mutate(Rank = rank(abs_diff), Sign_Rank = sign*Rank) %>% 
    arrange(order)
df
W = df %>% summarise(w = sum(Sign_Rank))
W 

wilcox.test(x1, mu=8, alternative=c('less'), paired=FALSE)


tibble(x = sort(x1)) %>% 
    summarise(mu = mean(x),
    var = var(x))


#############################################

H_0 = 9
x2 <- c(9, 6, 12, 8, 15, 7, 8, 12, 6, 10, 13, 11, 11, 10, 10)

# binom.test(sum(x2 > 9), 14, p=0.5, alternative=c('greater'), conf.level=0.95)$p.value
SIGN.test(x2, md=9, alternative='greater', conf.level=0.90)$p.value

df2 = tibble(x = sort(x2)) %>%
    mutate(order = 1:length(x),
    diff = x - 9,
    sign = sign(diff),
    abs_diff = abs(diff)) %>%
    filter(diff != 0) %>%
    arrange(abs_diff) %>%
    mutate(Rank = rank(abs_diff), Sign_Rank = sign*Rank) %>% 
    arrange(order)
df2
W2 = df2 %>% summarise(w = sum(Sign_Rank))
W2

wilcox.test(x2, mu=9, alternative=c('greater'), paired=FALSE)

tibble(x = sort(x2)) %>%
    summarise(mu = mean(x),
    var = var(x))

############################################
# a = c(-3.9, -2.0, 1.7, 7.3, 11.7)
# qqnorm(a)
# qqline(a)
# df = tibble(x = a) %>% arrange(x) %>%
#     mutate(p = (1:length(x))/(length(x) + 1),
#     z = qnorm(p), p_t = 3 + z*7)
# plot(df$p_t, df$x, 
#     main = "QQ Plot",
#     ylab='Sample Quantiles',
#     xlab='Theoretical Quatiles')
# abline(a = 0, b=1, col='red')
