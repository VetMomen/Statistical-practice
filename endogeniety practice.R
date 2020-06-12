library(tidyverse);library(readxl)

dir<-"/home/debian/Downloads/TrainExer45.xls"
dat<-read_xls(dir)

fit<-dat%>%lm(formula = GPA~PARTICIPATION+GENDER)
summary(fit)

sls1<-dat%>%lm(formula = PARTICIPATION~GENDER+EMAIL)
summary(sls1)

part<-predict(sls1)

sls2<-dat%>%lm(formula = GPA~part+GENDER)
summary(sls2)

#Do Haussman to ensure the validity of the instruments

v<-sls1$residuals
e<-fit$residuals
hfit<-dat%>%lm(formula = e~PARTICIPATION+GENDER+v)
summary(hfit)


qc<-1000*0.0368

1-pchisq(q = qc,df = 1)#the instrument is valid

