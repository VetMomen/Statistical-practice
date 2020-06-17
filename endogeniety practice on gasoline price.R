library(tidyverse)

dir<-"/home/debian/Statistical-practice/data sets/gasoline - endogeniety.txt"

dat<-read.delim(dir,sep = ",")

#first stage sls
#regress endog(PG) on instruments + exogenous 

sls1<-dat%>%lm(formula = PG~RI+RPN+RPT+RPU)
summary(sls1)

#second stage correlation

pgz<-predict(sls1)

sls2<-dat%>%lm(formula = GC~pgz+RI)
summary(sls2)


#sargant test

esls2<-dat$GC-(as.matrix(cbind(rep(1,nrow(dat)),dat[,c("PG","RI")]))%*%coef(sls2))

sargfit<-dat%>%lm(formula = esls2~RI+RPN+RPT+RPU)%>%summary()

R2<-0.1042

qc<-30*R2

pchisq(q = qc,df = 2)

#the endogeniety is valid 



