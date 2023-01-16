college_txt <- "/workspaces/devcontainers-rstudio/NUS Stuff/DSA1101/DSA1101 AY1819 Semester 1 (Dr. Sun Baoluo)/Tutorials/Tut01-02/Colleges.txt"
col <- read.table(college_txt, header = T, sep = "\t")
View(col)

hist(col$SAT, prob=T)
plot(col$DPerStudent, col$GradPer)

hist(col$DPerStudent[col$School_Type=="Lib Arts"])
hist(col$DPerStudent[col$School_Type!="LibArts"])

hist(col$GradPer[col$School_Type=="Lib Arts"])
hist(col$GradPer[col$School_Type!="LibArts"])
