#Calculating standard error of b by hand using this formula : 

#var(b)=s^2(x'x)-1

library(tidyverse);library(readxl) 


#importing and prepairing the data 

dir<-"/home/debian/Statistical-practice/data sets/Ch3 application econometric analysis.xlsx"

dat<-read_xlsx(dir)

str(dat)

minus<-which(str_detect(dat$Ability,pattern = "−"))

dat$Ability<-str_remove_all(dat$Ability,"−")

dat$Ability<-as.numeric(dat$Ability)

dat$Ability[minus]<-dat$Ability[minus]*-1

names(dat)<-c("Person","Education","Wage","Experience","Ability","M_education","F_education","Siblings")


#Look at it using regular function lm

fit<-dat%>%lm(formula = Wage~Education+Ability)

summary(fit)

#now get the (x'x)-1 matrix

x<-cbind(C=rep(1,nrow(dat)),dat[,c("Education","Ability")])%>%as.matrix()
A<-solve(t(x)%*%(x))

S2<-(t(fit$residuals)%*%fit$residuals)/(nrow(dat)-length(coef(fit)))%>%as.vector()

varb<-S2[1,1]*diag(A)

sdb<-sqrt(varb)%>%as.matrix() #Good it is the same 
dim(sdb)


#Now calculate CI

tk<-c(qt(p = c(.025,.975),df = (nrow(dat)-3)))%>%as.matrix()

dim(tk)

cI<-coef(fit)+(sdb%*%t(tk))

cI

confint(fit) #the same


#H testing 

tvalue<-coef(fit)/sdb

#prop of t value

pt(q = tvalue,df = 12,lower.tail = F)
