library(lme4)
library(tidyverse)
set.seed(658)
data<-data.frame(id=1:100,A=rnorm(100,mean = 5,sd = 5),B=rnorm(100,mean = 6,sd = 4),C=rnorm(100,mean = 7,sd = 6),D=rnorm(100,mean = 10,sd = 3),gender=factor(c("male","female")))

data%>%summary

data2<-gather(data = data,"time","value",-c(id,gender))

dum<-model.matrix(~time,data2)[,-1]
data2<-cbind(data2,dum)
data2<-data2%>%mutate(time=as.numeric(factor(time))-1)
male<-model.matrix(~gender,data2)[,-1]
data2<-cbind(data2,male)

#unconditional mean time

mod1<-lmer(value~1+(1|id),data2)
summary(mod1)

coef1<-coef(mod1)


data2%>%ggplot(aes(x=id,y = value))+
        geom_abline(slope = 0,intercept = coef1[["id"]][["(Intercept)"]])+
        ylim(c(-.1,0))

mod2<-lmer(value~time+(1+time|id),data2)

summary(mod2)

confint.merMod(object = mod2,method = "Wald")

pt(q = 8.028,df = 98,lower.tail = F)



coef2<-coef(mod2)


data2%>%ggplot(aes(x=id,y = value))+
        geom_abline(slope = coef2[["id"]][["time"]],intercept = coef2[["id"]][["(Intercept)"]])+
        ylim(c(min(coef2$id$`(Intercept)`),max(coef2$id$`(Intercept)`)))


data2<-data2%>%group_by(time)%>%mutate(malec=male-mean(male))
        
mod3<-lmer(value~malec*time+(1+time|id),data2)
summary(mod3)
coef3<-coef(mod3)


data2%>%ggplot(aes(x=id,y = value))+
        geom_abline(slope = coef3[["id"]][["time"]],intercept = coef3[["id"]][["(Intercept)"]])+
        ylim(c(min(coef2$id$`(Intercept)`),max(coef2$id$`(Intercept)`)))


#wald z for variance
1.6292/1.517
4.6236/3.357
pnorm(q = 1.377301,mean = 0,sd = 1,lower.tail = F)

#CI of parameter

4.6236+c(3.357*1.96,-3.357*1.96)
