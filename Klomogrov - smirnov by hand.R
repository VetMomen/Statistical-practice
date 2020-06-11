library(tidyverse)

x<-c(27,25,24,24,22,20,21,22,21,25,24,
26,25,24,23,22,20,21,19,21,25,24,
26,25,22,23,22,22,21,19,21,23,21,
26,24,22,23,22,22,20,19,21,23,21,
26,24,22,23,21,19,20,18,20,20,18)


hist(x)

xbar<-mean(x)
s<-sd(x)

#ascending the data 

x<-x%>%data.frame()%>%arrange(x)

x<-x%>%mutate(F_0=pnorm(q = .,mean = xbar,sd = s),
              F_n=(cumsum(rep(1,nrow(x))))/nrow(x),
              F0_Fn=abs(F_0-F_n))

D=max(x$F0_Fn)

pnorm(q = D,mean = mean(x$F0_Fn),sd = sd(x$F0_Fn),lower.tail = F)



ks.test(x = x$.,y =pnorm,mean(x$.),sd(x$.))


shapiro.test(x = x)

