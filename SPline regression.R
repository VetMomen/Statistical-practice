Quantity <- c(25,39,45,57,70,85,89,100,110,124,137,150,177)
Sales <- c(1000,1250,2600,3000,3500,4500,5000,4700,4405,4000,3730,3400,3300)
data <- data.frame(Quantity,Sales)
data
plot(data)

#fitting linear
fitlinear<-lm(data$Sales~data$Quantity)
summary(fitlinear)

abline(reg = fitlinear)

#fitting polynomial

fitpoly<-lm(data$Sales~poly(data$Quantity,2)+data$Quantity)
summary(fitpoly)

plot(data)
lines(x = data$Quantity,y = fitpoly$fitted.values,col="red")

#Make Spline :

k<-89 #knot

xk<-data$Quantity-89

d<-ifelse(data$Quantity<89,0,1)

fitspline<-lm(data$Sales~data$Quantity+I(d*xk))
summary(fitspline)

plot(data)
lines(x = data$Quantity,y = fitspline$fitted.values,col="red")
