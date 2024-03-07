AB <- read.csv("AB_2024.csv")

# Q4
# b)
page_A <- AB[AB$landing_page == 'A',]$converted
y_A = sum(page_A); n = length(page_A)
p_A = y_A/n
mle_p_A <- mean(page_A)

page_B <- AB[AB$landing_page == 'B',]$converted
y_B = sum(page_B); m = length(page_B)
p_B = y_B/m
mle_p_B <- mean(page_B)

p_A
mle_p_A

p_B
mle_p_B

# c)
p_hat <- (y_A + y_B)/(n + m)
t_stat <- abs((p_A - p_B)/sqrt(p_hat*(1-p_hat)*(1/n + 1/m)))
t_stat
