#Computing Prediction interval for Y and lnY

library(tidyverse)

dir<-"/home/debian/Statistical-practice/data sets/Auction Data for Monet Paintings.csv"

dat<-read.csv(dir)
str(dat)

#make log variable 

dat<-dat%>%mutate(logrpice=log(PRICE),
                  logarea=log(HEIGHT*WIDTH),
                  aspect=HEIGHT/WIDTH)

#Fitting it by lm()

fit<-dat%>%lm(formula = logrpice~logarea+aspect)

summary(fit) #Noting the coefs and its SE

#getting SE(e)

r<-fit$residuals

n<-nrow(dat)

k<-length(coef(fit))

df<-n-k

e_var<-(t(r)%*%r)/df

#CI of log(y)

cI<-qt(p = c(.025,.975),df = df)*sqrt(e_var[1,1])

cI<-matrix(data = cI,nrow = 1)

PI<-list()
for(i in 1:2){
        PI[[i]]<-fit$fitted.values+cI[,i]
}

PI<-bind_cols(PI)

names(PI)<-c("lower","upper")

head(PI)
predict(object = fit,newdata = data.frame(dat),interval  = "predict")%>%head() #the same

#CI for Y not log(y)

PIy<-exp(PI) 


