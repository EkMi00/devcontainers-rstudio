items <- data.frame(battery = 6000,
sunscreen = 5000,
sandals = 4000,
bowls = 2000,
bat_sun = 1500,
bat_san = 1000,
bat_bow = 250,
bat_sun_san = 600)

n <- 10000

support <- function(item) {
    return (item/n)
}

item_supp <- apply(items, 2, function(x) {support(x)})
# battery   sunscreen     sandals       bowls     bat_sun     bat_san 
#       0.600       0.500       0.400       0.200       0.150       0.100
#     bat_bow bat_sun_san
#       0.025       0.060

min_supp = 0.05
frequent <- item_supp > 0.05
# battery   sunscreen     sandals       bowls     bat_sun     bat_san 
#        TRUE        TRUE        TRUE        TRUE        TRUE        TRUE
#     bat_bow bat_sun_san
#       FALSE        TRUE

confidence <- function(item12, item2) {
    return (support(item12)/support(item2))
}

conf_bat_sun <- items[,"bat_sun"]/items[,"battery"]
# 0.25
conf_batsun_san <- items[,"bat_sun_san"]/items[,"bat_sun"]
# 0.4
# {battery, sunscreen} -> {sandals} is better than
# {battery} -> {sunscreen}

###############################################################################
supp_A = 0.6
supp_B = 0.6
conf_B_A = 0.9 
conf_C_AB = 0.5
supp_AB = conf_B_A * supp_B

lift_A_B = (supp_AB) / (supp_A * supp_B)

lev_A_B = (supp_AB) - (supp_A * supp_B)

conf_A_B = (supp_AB) / supp_A

# lift_AB_C = (conf_C_AB * supp_C) / (supp_AB * supp_C)
lift_AB_C = (conf_C_AB) / (supp_AB)

# print(lift_A_B)
# print(lev_A_B)
# print(conf_A_B)
# print(lift_AB_C)


###############################################################################
library('arules')
library('arulesViz')

data(Groceries)
summary(Groceries)

Groceries@itemInfo[1:10,]
Groceries@data[,100:110]

apply(Groceries@data[,1:5], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))

apply(Groceries@data[,100:105], 2,
      function(r) paste(Groceries@itemInfo[r,"labels"], collapse=", "))
