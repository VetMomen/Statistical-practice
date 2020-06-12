library(tidyverse)

dir<-"/home/debian/Statistical-practice/data sets/gasoline - endogeniety.txt"

dat<-read.delim(dir,sep = ",")

#first stage sls
#regress endog(PG) on instruments + exogenous 

sls1<-dat%>%lm(formula = PG~RI+RPN+RPT+RPU)
summary(sls1)

#second stage correlation

z<-predict(sls1)

sls2<-dat%>%lm(formula = GC~z+RI)
summary(sls2)


#sargant test
#get the residual from regressing Y on X1 and X2
fit<-dat%>%lm(formula = GC~PG+RI)

slsr<-fit$residuals

#regress e on all instruments
sarg<-dat%>%lm(formula = slsr~RI+RPN+RPT+RPU)
summary(sarg)
R2<-0.1185

qc<-30*R2

pchisq(q = qc,df = 2)

#the endogeniety is valid 

