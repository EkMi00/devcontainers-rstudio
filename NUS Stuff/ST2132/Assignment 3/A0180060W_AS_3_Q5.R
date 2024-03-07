tf <- read.csv("traffic.csv")

clear <- tf[tf$weather_main == "Clear",]$traffic_volume
rain <- tf[tf$weather_main == "Rain",]$traffic_volume

x_bar <- mean(clear)
y_bar <- mean(rain)

x_var <- var(clear)
y_var <- var(rain)

n = length(clear)
m = length(rain)

xy_diff = x_bar - y_bar
z_crit = 1.96
var_p = ((n-1)*x_var + (m-1)*y_var)/(n + m - 2)
sd_p = sqrt(var_p)

# b) 
t_stat = xy_diff/(sd_p*sqrt(1/n + 1/m))
t_stat
deg_free = n + m - 2
deg_free

# c)
T_stat = xy_diff/(sqrt(x_var/n + y_var/m))
T_stat

num = (x_var/n + y_var/m)^2
denom = (x_var/n)^2/(n-1) + (y_var/m)^2/(m-1)
deg_r = floor(num/denom)
deg_r

# d)
F_stat = x_var/y_var
F_stat
r_1 = n-1; r_1
r_2 = m-1; r_2
