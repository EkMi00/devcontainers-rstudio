if(!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

msft <- read.csv("MSFT.csv") %>%
    mutate(Tmr = lead(Close),
        R = log(Tmr/Close)) %>% slice(-length(R))

# a)
r_bar = msft %>% summarise(mean = mean(R)) %>% .$mean
r_bar # 0.000869503
r_var = msft %>% summarise(var = var(R)) %>% .$var
r_var # 0.0004519334

# b)
data <- msft %>% arrange(R) %>% 
    select(R) %>% 
    mutate(p = (1:length(R))/(length(R) + 1),
        z = qnorm(p), p_t = mean(R) + z*sd(R))

plot(data$p_t, data$R, 
    main = "QQ Plot",
    ylab='Sample Quantiles',
    xlab='Theoretical Quatiles')
abline(a = 0, b=1, col='red')

# c)
# r_i does not follow a normal distribution since the data
# is more likely to take on extremely large or extremely
# small values than standard normal (heavy-tailed distribution)

