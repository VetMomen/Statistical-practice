library(tidyverse)
library(ez)
library(car)
cell1<-expand.grid(A="1",B="1",dep=c(3,5,6))
cell2<-expand.grid(A="1",B="2",dep=c(2,4,8))
cell3<-expand.grid(A="1",B="3",dep=c(11,7,8))
cell4<-expand.grid(A="2",B="1",dep=c(9,14,5))
cell5<-expand.grid(A="2",B="2",dep=c(6,7,7))
cell6<-expand.grid(A="2",B="3",dep=c(9,8,10))

data<-rbind(cell1,cell2,cell3,cell4,cell5,cell6)%>%data.frame()%>%mutate(A=factor(A),
                                                                         B=factor(B))%>%
        group_by(A,B)%>%mutate(idx=1:n())

dum<-model.matrix(A~.,data[,-4])[,-1]
fit<-lm(dep~A+B+A*B,data[,-4])
summary.aov(fit)
summary(fit)

#type III SS
anov<-ezANOVA(data = data,dv = dep,wid = idx,between = c(A,B),type = 3,return_aov = T)
anov$aov%>%summary

#type II SS

anov<-ezANOVA(data = data,dv = dep,wid = idx,between = c(A,B),type = 2,return_aov = T)
anov$aov%>%summary

#type I SS
anov<-ezANOVA(data = data,dv = dep,wid = idx,between = c(A,B),type = 1,return_aov = T)
anov$aov%>%summary


Anova(fit,type = "III")
#################################################


cell1<-expand.grid(A="1",B="1",dep=c(3,5,6))
cell2<-expand.grid(A="1",B="2",dep=c(2,4,8))
cell3<-expand.grid(A="1",B="3",dep=c(11,7,8,6,9))
cell4<-expand.grid(A="2",B="1",dep=c(9,14,5,11))
cell5<-expand.grid(A="2",B="2",dep=c(6,7,7,8,10,5,6))
cell6<-expand.grid(A="2",B="3",dep=c(9,8,10))

data2<-rbind(cell1,cell2,cell3,cell4,cell5,cell6)%>%data.frame()%>%mutate(A=factor(A),
                                                                         B=factor(B))%>%
        group_by(A,B)%>%mutate(idx=1:n())

fit<-lm(dep~A+B+A*B,data2[,-4])
summary.aov(fit)
summary(fit)

#type III SS
anov<-ezANOVA(data = data2,dv = dep,wid = idx,between = c(A,B),type = 3,return_aov = T)
anov$aov%>%summary

#type II SS

anov<-ezANOVA(data = data2,dv = dep,wid = idx,between = c(A,B),type = 2,return_aov = T)
anov$aov%>%summary

#type I SS
anov<-ezANOVA(data = data2,dv = dep,wid = idx,between = c(A,B),type = 1,return_aov = T)
anov$aov%>%summary


#########################

fit<-lm(dep~A+B+A*B,data2[,-4])

model.matrix(dep~A+B+A*B,data[,-4])[,-1]%>%data.frame()%>%mutate(y=data$dep)%>%cor()

data$A<-relevel(data$A,ref = "2")
data$B<-relevel(data$B,ref = "3")
dum<-model.matrix(dep~A+B+A*B,data[,-4])[,-1]%>%data.frame()%>%mutate(y=data$dep)
dum%>%View
cor(dum)%>%View
fit<-lm(y~.-1,dum)

summary.aov(fit)
summary(fit)
fit$model

data2$A<-relevel(data2$A,ref = "2")
data2$B<-relevel(data2$B,ref = "3")
dum<-model.matrix(dep~A+B+A*B,data2[,-4])%>%data.frame()%>%mutate(y=data2$dep)
dum%>%View
cor(dum)%>%View
