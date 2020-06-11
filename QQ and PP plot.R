library(tidyverse)
#####################
#PP plot
#####################
x<-c(27,25,24,24,22,20,21,22,21,25,24,
     26,25,24,23,22,20,21,19,21,25,24,
     26,25,22,23,22,22,21,19,21,23,21,
     26,24,22,23,22,22,20,19,21,23,21,
     26,24,22,23,21,19,20,18,20,20,18)

dat<-x%>%data.frame()

names(dat)<-"x"
dat<-dat%>%arrange(x)%>%
  mutate(F1=pnorm(x,mean = mean(x),sd = sd(x)))

c<-c()
for(i in 1:nrow(dat)){
  c[i]<-i/(nrow(dat)+1)
}

dat$c<-c

plot(x=dat$c,y = dat$F1)

######################
#QQ plot
######################

xhat<-qnorm(p = dat$c,mean = mean(dat$x),sd = sd(dat$x))

dat$xhat<-xhat

dat%>%ggplot(aes(x=x,y = xhat))+
  geom_point()+geom_jitter()
