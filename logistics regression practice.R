library(tidyverse);library(readxl)

dir<-"/home/debian/Statistical-practice/data sets/TrainExer5-5.xls"

dat<-read_xls(dir)

#exploring the data

str(dat)

#converting dumies o factors

dat<-dat%>%mutate(response=factor(response),male=factor(male),activity=factor(activity))

#loooking at the prop table

table(dat$response)%>%prop.table()

table(dat$male)%>%prop.table()

table(dat$activity)%>%prop.table()

table(dat$response,dat$male)%>%prop.table()

dat%>%group_by(male)%>%summarize(mean(age))
dat%>%group_by(response)%>%summarize(mean(age))
dat%>%group_by(activity)%>%summarize(mean(age))

dat%>%group_by(male,response,activity)%>%summarize(mean(age))

#fitting the model

logfit<-dat%>%glm(formula = response~male+activity+age+I(age^2),family = "binomial")
summary(logfit)


#fitting the new response
newres<-(-1*as.character(dat$response)%>%as.numeric())+1
logfit2<-dat%>%glm(formula = newres~male+activity+age+I(age^2),family = "binomial")
summary(logfit2)

#testing B1=B2=0

logfitR<-dat%>%glm(formula = response~age+I(age^2),family = "binomial")

llfull<-logLik(logfit)
llR<-logLik(logfitR)

LR<-(-2*(llR-llfull))
df<-5-3

LR>=qchisq(p = .99,df = df) #Reject H0




