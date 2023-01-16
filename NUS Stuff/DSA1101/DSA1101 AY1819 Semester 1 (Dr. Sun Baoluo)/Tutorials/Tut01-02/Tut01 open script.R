col <- read.table("/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/DSA1101 AY1819 Semester 1 (Dr. Sun Baoluo)/Tutorials/Tut01-02/Colleges.txt", header = T, sep = "\t")
View(col)

hist(col$SAT)
hist(col$SAT,prob=T) #prob=T for density, the distribution of the values on x range, not depends on the number of samples.

plot(col$DPerStudent, col$GradPer)
#Draw a plot of the DPerStudent versus GradPer variables
#2e+04 = 2x10^4, 1e+05=1x10^5

hist(col$DPerStudent[col$School_Type=="Lib Arts"])
hist(col$DPerStudent[col$School_Type!="LibArts"])

hist(col$GradPer[col$School_Type=="Lib Arts"])
hist(col$GradPer[col$School_Type!="LibArts"])

col$School[col$PerPhD>75] #institutions have more than 75% faculty members with PhD
fit=lm(Acceptance~Top.10p+ PerPhD + GradPer ,data=col)
summary(fit)#show result


