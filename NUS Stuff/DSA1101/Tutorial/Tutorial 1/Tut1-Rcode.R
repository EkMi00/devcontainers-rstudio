


###########  SOLUTION OF TUTORIAL 1 #######



#Q1 SALARY IS NOT CHANGED OVER THE YEARS

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

# salary is the monthly salary

#### FIRST PERSON WITH salary = 7000

salary = 7000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
print(month)


# when salary = 7000 # answer should be 55 months



#### SECOND PERSON WITH salary = 10000

salary = 10000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
print(month)


# when salary = 10000 # answer should be 44 months

# Extra question: 
# Can you think of a way which can make the code be easy if we have 10 persons with different salary?
# Hint: Which part of the code above for 2 persons is repeated? Use FOR loop

####### SHORTER CODE 

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000,10000) # vector of salary for two persons

total.month = numeric(length(sal)) # vector to record the output - number of months

print(cbind(sal,total.month)) # we'll let the code to update the second column

for (i in 1:length(sal)){
  
salary = sal[i]
saved <- 10000 # initial savings that parents give

month <- 0

while(saved < cost){
  month = month +1
  saved = saved+ portion_save *salary + saved*r
}
total.month[i] = month
}

print(cbind(sal,total.month))


#####################################

#Q2 THERE IS A RAISE IN THE SALARY EVERY 4 MONTHS:

# rate is the raise in salary per 4 months, change from person to person

price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month



# FIRST PERSON: SALAPRY = 7000 & rate = 0.02

salary = 7000

rate = 0.02

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 7000, & rate = 0.02,  answer: 52 months


# SECOND PERSON: SALAPRY = 10000 & rate = 0.01

salary = 10000

rate = 0.01

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 10000, & rate = 0.01, answer: 43 months

#################3


#Extra question: Can you use FOR loop to write a shorter code for Q2?


price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000, 10000) # salary of 2 persons
Rate = c(0.02,0.01) # rate of increasement of 2 persons

total.month = numeric(length(sal)) # vector to record the output - number of months


print(cbind(sal, Rate,total.month))

for (i in 1:length(sal)){
  
  salary = sal[i]
  rate = Rate[i]
  saved <- 10000 # initial savings that parents give
  
  month <- 0
  
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }
  total.month[i] = month
}

print(cbind(sal,Rate,total.month))












