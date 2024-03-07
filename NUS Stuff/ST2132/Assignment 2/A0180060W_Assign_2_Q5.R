tf <- read.csv("traffic.csv")

#a
clear <- tf[tf$weather_main == "Clear",]$traffic_volume
x_bar <- mean(clear)
x_bar #3055.909

rain <- tf[tf$weather_main == "Rain",]$traffic_volume
y_bar <- mean(rain)
y_bar #3317.906

#b
x_var <- var(clear)
y_var <- var(rain)

n = length(clear)
m = length(rain)

xy_diff = abs(x_bar - y_bar)
z_crit = 1.96
var_p = ((n-1)*x_var + (m-1)*y_var)/(n + m - 2)
sd_p = sqrt(var_p)

lower_1 = round(xy_diff - z_crit*sd_p*sqrt(1/n + 1/m), 2)
upper_1 = round(xy_diff + z_crit*sd_p*sqrt(1/n + 1/m), 2)
sprintf("95%% two-sample pooled t-interval: [%s, %s]", lower_1, upper_1) #[200.34, 323.65]

#c
lower_2 = round(xy_diff - z_crit*sqrt(x_var/n + y_var/m), 2)
upper_2 = round(xy_diff + z_crit*sqrt(x_var/n + y_var/m), 2)
sprintf("95%% two-sample pooled t-interval: [%s, %s]", lower_2, upper_2) #[200.4, 323.59]
