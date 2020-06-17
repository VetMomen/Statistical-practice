library(tidyverse)
library(lmtest)

dirc<-choose.files()

dat<-read_csv(dirc,col_types = c("dnnnnnnnnn"))

fit1<-dat%>%lm(formula = LogEqPrem~BookMarket+NTIS+DivPrice+EarnPrice+Inflation)
summary(fit1)


fit2<-dat%>%lm(formula = LogEqPrem~BookMarket)
summary(fit2)

anova(fit2,fit1)

y_hat<-fit2$fitted.values

reset<-lm(dat$LogEqPrem~y_hat+I(y_hat^2))

e0<-t(fit2$residuals)%*%fit2$residuals
e1<-t(reset$residuals)%*%reset$residuals

f_reset<-(((e0-e1)/1)/(e1/84))

1-pf(q = f_reset,df1 = 1,df2 = 83)

resettest(formula = fit2,power = 2)


#chow break

er<-fit2$residuals%>%as.matrix()

dat1<-dat%>%filter(Year<=1979)
dat2<-dat%>%filter(Year>1979)

fit2_1<-dat1%>%lm(formula = LogEqPrem~BookMarket)
fit2_2<-dat2%>%lm(formula = LogEqPrem~BookMarket)

eu1<-as.matrix(fit2_1$residuals)

eu2<-as.matrix(fit2_2$residuals)

s0<-t(er)%*%er
s1<-(t(eu1)%*%eu1)
s2<-(t(eu2)%*%eu2)

f_break<-((s0-s1-s2)/2)/((s1+s2)/83)
1-pf(q = f_break,df1 = 2,df2 = 83)

f_forcast<-((s0-s1)/34)/(s1/51)
1-pf(q = f_forcast,df1 = 34,df2 = 51)
