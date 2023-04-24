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

suppressWarnings(   
itemset.1 <- apriori(Groceries,
    parameter=list(minlen=1, maxlen=1, support=0.02, target="frequent itemsets")) 
)

# print(summary(itemset.1))


# list the most 10 frequent 1-itemsets:
# print(inspect(head(sort(itemsets.1, by = "support"), 10)))

# list all the 59 frequent 1-itemsets:
# print(inspect(sort(itemsets.1, by ="support")))



itemsets.2 <- apriori(Groceries, parameter=list(minlen=2, maxlen=2,
          support=0.02, target="frequent itemsets"))

# print(summary(itemsets.2))

# list all the frequent 2-itemsets:
# inspect(sort(itemsets.2, by ="support"))

# list of most 10 frequent 2-itemsets:
# inspect(head(sort(itemsets.2, by = "support"), 10))

itemsets.3 <- apriori(Groceries, parameter=list(minlen=3, maxlen=3,
          support=0.02, target="frequent itemsets"))

summary(itemsets.3)
# inspect(sort(itemsets.3, by ="support"))



itemsets.4 <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
    support=0.02, target="frequent itemsets"))

summary(itemsets.4)

# inspect(sort(itemsets.4, by ="support")) # nothing



itemsets.4 <- apriori(Groceries, parameter=list(minlen=4, maxlen=4,
    support=0.007, target="frequent itemsets"))

summary(itemsets.4)
# inspect(sort(itemsets.4, by ="support"))

# there are three frequent 4-itemsets if the minimum support is 0.007.



itemsets<- apriori( Groceries , parameter = list( minlen=1,
            support =0.02 , target ="frequent itemsets"))

summary( itemsets )
# this summarizes that: there are 59 frequent 1-itemsets; 
# 61 frequent 2-itemsets; and 2 frequent 3-itemsets

# inspect(sort( itemsets , by ="support")) 
# this will rank the itemsets by their support, regardless of itemsets with 1 item or 2 items.
# row 17: {other vegetables, whole milk}  with support = 0.07483477

rules <- apriori(Groceries, parameter=list(support=0.001,
         confidence=0.6, target = "rules"))

#     lhs                              rhs                support     confidence
# [1] {honey}                       => {whole milk}       0.001118454 0.7333333
# [2] {cereals}                     => {whole milk}       0.003660397 0.6428571
# [3] {rice}                        => {whole milk}       0.004677173 0.6133333
# [4] {liver loaf, yogurt}          => {whole milk}       0.001016777 0.6666667
# [5] {tropical fruit, curd cheese} => {other vegetables} 0.001016777 0.6666667
# [6] {curd cheese, rolls/buns}     => {whole milk}       0.001016777 0.6250000
#     coverage    lift     count
# [1] 0.001525165 2.870009 11
# [2] 0.005693950 2.515917 36
# [3] 0.007625826 2.400371 46
# [4] 0.001525165 2.609099 10
# [5] 0.001525165 3.445437 10
# [6] 0.001626843 2.446031 10
plot(rules)
plot(rules, measure= c("support", "confidence"),
     shading="lift", col="red") # scatter plot of all 2918 rules


# the top 3 rules sorted by LIFT:
inspect(head(sort(rules, by="lift"), 3))

# the top 5 rules sorted by LIFT
inspect(head(sort(rules, by="lift"), 5))
highLiftRules <- head(sort(rules, by="lift"), 5)

# plot the top 5 rules above for visualzation:
# plot(highLiftRules, method="matrix") # this is simple and a bit difficult to see

# more parameters added, plot looks better:
plot(highLiftRules, method = "graph", engine = "igraph",
     edgeCol = "blue", alpha = c(1))
# alpha = c(0,1)
# the size of the node is sorted (determined?) by the support.
# the darkness of the color represents the change in lift

#some common choices for 'method':
# matrix, mosaic, doubledecker, graph, paracoord, scatterplot, grouped matrix, two-key plot, matrix3D