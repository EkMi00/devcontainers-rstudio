library(float)

0.23
s1 = sprintf("%.23f",as.float(0.23))
print(s1)
0.25
s2 = sprintf("%.23f",as.float(0.25))
print(s2)
print(as.float(1.728^as.float(1/3))*as.float(1.728^as.float(1/3))*as.float(1.728^as.float(1/3))-1.728)
print(as.float(3.375^as.float(1/3))*as.float(3.375^as.float(1/3))*as.float(3.375^as.float(1/3))-3.375)

s3 = sprintf("%.23f", as.float(5/7 + 1/3))
print(s3)

