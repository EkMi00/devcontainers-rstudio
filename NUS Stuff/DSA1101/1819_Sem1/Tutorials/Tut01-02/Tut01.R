college_txt <- "/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/1819_Sem1/Tutorials/Tut01-02/Colleges.txt"
col <- read.table(college_txt, header = T, sep = "\t")
View(col)

hist(col$SAT, prob=T)
plot(col$DPerStudent, col$GradPer)

hist(col$DPerStudent[col$School_Type=="Lib Arts"])
hist(col$DPerStudent[col$School_Type!="LibArts"])

hist(col$GradPer[col$School_Type=="Lib Arts"])
hist(col$GradPer[col$School_Type!="LibArts"])

col$School[col$PerPhD>75] #institutions have more than 75% faculty members with PhD
fit=lm(Acceptance~Top.10p+ PerPhD + GradPer ,data=col)
summary(fit)#show result