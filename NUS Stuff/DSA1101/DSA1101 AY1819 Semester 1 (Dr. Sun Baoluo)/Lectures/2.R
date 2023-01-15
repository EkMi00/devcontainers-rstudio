resale = read.csv('c:/hdbresale_reg.csv')
head(resale)
x=c(-1,3,5)
y=c(-1,3.5,3)
lm(y~x)
lmout=lm(y~x)
plot(x=lmout$fitted.values, y=lmout$residuals, xlab="Fitted", ylab="Res", cex=2, cex.lab=2, cex.axis=2, pch.16)
abline(0,0)
levels(resale$town)
levels(resale$flat_type)
clmv=lm(resale_price~town+floor_area_sqm+flat_type,data=resale)
confint(clmv,level= .95)
summary(clmv)
town='CENTRAL AREA'
flat_type='3 ROOM'
floor_area_sqm=70

new_pt=data.frame(town,flat_type,floor_area_sqm)
conf_int_pt=predict(clmv,new_pt,level=.95,interval="confidence")
conf_int_pt