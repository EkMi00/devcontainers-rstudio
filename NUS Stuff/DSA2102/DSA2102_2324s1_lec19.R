library(PolynomF)

t <- c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24)
d <- c(22, 20.5, 19, 18.5, 18, 18, 18.5, 19, 21, 23, 24, 24.5, 25, 26, 27, 28, 28, 26, 24.5, 23, 22, 22, 21.5, 21, 22)
plot(d~t, pch=19)

p <- poly_calc(t,d)
curve(p, add = T)

lines(spline(t,d))

#######

spx <- function(x){sin(pi*x)}
plot(spx, xlim = c(-2,2))

n <- 4
x <- seq(from=-2, to = 2, by = 4/(n-1))
y <- spx(x)

p <- poly_calc(x,y)
plot(spx, xlim = c(-2,2), ylim = c(-2,2))
curve(p, add = T, col = "red")
lines(spline(x,y), col = "blue")

