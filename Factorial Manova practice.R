library(tidyverse)
library(psych)
library(heplots)
library(ez)

a1b1y1<-expand.grid(A=1,B=1,y=1,val=c(6,7,9,13,11,17))
a1b1y2<-expand.grid(A=1,B=1,y=2,val=c(10,8,8,16,15,18))
a1b2y1<-expand.grid(A=1,B=2,y=1,val=c(9,8,14,21,18,16))
a1b2y2<-expand.grid(A=1,B=2,y=2,val=c(11,8,9,19,15,13))
a2b1y1<-expand.grid(A=2,B=1,y=1,val=c(11,7,10,10,11,14))
a2b1y2<-expand.grid(A=2,B=1,y=2,val=c(8,6,5,12,13,10))
a2b2y1<-expand.grid(A=2,B=2,y=1,val=c(4,10,11,11,9,8))
a2b2y2<-expand.grid(A=2,B=2,y=2,val=c(12,8,13,10,8,15))

mat<-rbind(a1b1y1,a1b1y2,a1b2y1,a1b2y2,
      a2b1y1,a2b1y2,a2b2y1,a2b2y2)

mat<-mat%>%data.frame()%>%mutate(A=factor(A),
                                 B=factor(B),
                                 y=factor(y))%>%group_by(A,B,y)%>%mutate(idx=1:n())

data<-spread(data = mat,key = y,value = val)

names(data)<-c("A","B","id","y1","y2")


data%>%group_by(A,B)%>%summarize(skew(y1),skew(y2),
                                 kurtosi(y1),kurtosi(y2))


lmA<-lm(cbind(y1,y2)~A,data)
boxM(lmA)

lmB<-lm(cbind(y1,y2)~B,data)
boxM(lmB)


manova(cbind(y1,y2)~A+B+A*B,data)%>%summary.manova(intercept = T,test = "Wilks")


manova(cbind(y1,y2)~A+B+A*B,data)%>%summary.aov(intercept = T)

y1aov<-ezANOVA(dv = y1,wid = id,between = c(A,B),data = data,return_aov = T)$aov


TukeyHSD(y1aov)



