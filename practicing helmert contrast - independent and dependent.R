#Contrast practicing 
library(tidyverse)
library(car)
#Helmert

gp<-rep(c(1,2,3),5)

y1<-c(5,2,4,6,3,6,6,4,3,4,3,5,5,2,5)

y2<-c(6,2,3,7,3,7,7,4,3,5,2,5,4,1,5)

data<-data.frame(cbind(gp=gp,y1=y1,y2=y2))%>%arrange(gp)%>%mutate(gp=factor(gp))


fit<-lm(cbind(y1,y2)~gp-1,data)
summary(fit)

linearHypothesis(fit,hypothesis.matrix = c("gp1=gp2","gp2=gp3"))


#helmert planned comp

helmat<-c(1,-1/2,-1/2,0,1,-1)%>%matrix(nrow = 3)

contrasts(data$gp)<-helmat
str(data)

fit2<-lm(cbind(y1,y2)~ gp-1,data)

linearHypothesis(fit2,type = "III",hypothesis.matrix = c("gp1=.5*gp2+.5*gp3"),icontrasts = "contr.Helmert")
linearHypothesis(fit2,type = "III",hypothesis.matrix = c("gp2=gp3"),icontrasts = "contr.Helmert")

fit2<-lm(cbind(y1,y2)~ gp,data)

summary(fit2)


##########################################3
##########################################

prim<-c(18,5,18,9,17,5,13,3,
13,6,20,5,22,7,9,3,
20,4,17,10,22,5,9,3,
22,8,24,4,13,9,15,5,
21,9,19,4,13,5,13,4,
19,0,18,4,11,5,12,4,
12,6,15,7,12,6,13,5,
10,5,16,7,23,3,12,3,
15,4,16,5,17,7,0,0,
15,5,14,3,18,7,0,0,
14,0,18,2,13,3,0,0,
12,6,14,4,0,0,0,0,
0,0,19,6,0,0,0,0,
0,0,23,2,0,0,0,0)%>%matrix(ncol = 8,byrow = T)

gp1<-prim[-c(13,14),1:2]%>%data.frame%>%mutate(gp=rep(1,12))
gp2<-prim[,3:4]%>%data.frame%>%mutate(gp=rep(2,14))
gp3<-prim[-c(12:14),5:6]%>%data.frame%>%mutate(gp=rep(3,11))
gp4<-prim[-c(9:14),7:8]%>%data.frame%>%mutate(gp=rep(4,8))

data<-rbind(gp1,gp2,gp3,gp4)%>%mutate(gp=factor(gp))

names(data)<-c("y1","y2","gp")


helmat<-c(0 ,1, -1,  0,
          0, 1, -.5, -.5,
          1, 0 , 0 ,-1)%>%matrix(nrow = 4,byrow = F)

contrasts(data$gp)<-helmat
str(data)

fit<-lm(cbind(y1,y2)~gp-1,data)

linearHypothesis(fit,hypothesis.matrix = c("gp3=gp2"))
linearHypothesis(fit,hypothesis.matrix = c("gp2=.5*gp3+.5*gp4"))
linearHypothesis(fit,hypothesis.matrix = c("gp1=gp4"))

fit<-lm(cbind(y1,y2)~gp,data)

summary(fit)
