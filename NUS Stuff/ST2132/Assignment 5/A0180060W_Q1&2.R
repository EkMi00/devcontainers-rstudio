x = tibble(c(340, 395, 358, 268))
x %>% mutate(res = ((x - 340.25)**2)/340.25) %>%
    summarize(test = sum(res)) %>% .$test
chisq.test(x)

x1 = matrix(c(45, 68, 45, 25, 15, 5, 11, 8, 3, 10, 5, 0), ncol = 4, nrow = 3)
x2 = matrix(c(59.9, 63.2, 34.89, 17.06, 18, 9.94, 8.34, 8.8, 4.86, 5.69, 6, 3.31), ncol=4, nrow=3)
rownames(x1) <- c('Soccer', 'NS_athletes', 'Non_athletes')
colnames(x1) <- c('c0', 'c1', 'c2', 'c3')
x1;x2
sum(((x1 - x2)^2)/x2)
chisq.test(x1)
