library('arules')
library('arulesViz') #visualisation

data(Groceries)
Groceries
summary(Groceries)
Groceries@itemInfo[1:10,] #ignore level2 and level1
Groceries@data[,100:110] #every column for every transaction
apply(Groceries@data[,100:105], 2,
function(r) paste(Groceries@itemInfo[r, 'labels'], collapse=','))

itemsets=apriori(Groceries, parameter=list(minlen=1, maxlen=1,
support=0.02, target='frequent itemsets'))
summary(itemsets)

inspect(head(sort(itemsets, by='support'),10))

itemsets=apriori(Groceries, parameter=list(minlen=2, maxlen=2, support=0.02, target='frequent itemsets'))
summary(itemsets)

inspect(head(sort(itemsets, by='support'),10))

itemsets=apriori(Groceries, parameter=list(minlen=3, maxlen=3, support=0.02, target='frequent itemsets'))
summary(itemsets)

itemsets=apriori(Groceries, parameter=list(minlen=4, maxlen=4, support=0.02, target='frequent itemsets'))
summary(itemsets) #algorithm ends

#generate rules
rules = apriori(Groceries,parameter=list(support
=0.001,confidence =0.6, target = "rules"))

plot(rules)

inspect(head(sort(rules,by="lift"),3))

plot(highLiftrules,method="graph", control=list(alpha =1))