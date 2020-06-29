library(readxl);library(tidyverse);library(lmtest)

dir<-"/home/debian/Statistical-practice/data sets/_bb386db860bf2eb4356cd71338367faf_Case_HousePrices-round1.xls"
dat<-read_xls(dir)

str(dat)
summary(dat)

sapply(dat,unique)

dat<-dat[,-1]

dat<-dat%>%mutate(drv=factor(drv),rec=factor(rec),ffin=factor(ffin),ghw=factor(ghw),
                  ca=factor(ca),reg=factor(reg))
#Fitting the first model and test for linearity

fit1<-dat%>%lm(formula = sell~.)
summary(fit1)

plot(fit1)

#using reset test to test linearity


resettest(fit1)#there are a nonlinear terms must be included

#adding log of sale term

dat<-dat%>%mutate(logsell=log(sell))

#Fitting again and test for linearity

fit2<-dat[,-1]%>%lm(formula = logsell~.)
summary(fit2)

resettest(fit2) #There is no problem with linearity any more 

#fitting with including the logarithm of lotsize
dat<-dat%>%mutate(loglot=log(lot))
fit3<-dat[,-1]%>%lm(formula = logsell~.)
summary(fit3)

fit4<-dat%>%lm(formula = logsell~loglot+reg+gar+ca+ghw+ffin+rec+drv+sty+fb+bdms)
anova(fit2,fit3,fit4) #it is significant to add log of lot , we can use fit4

#model interaction of loglot with other variables 

dat2<-sapply(dat[,c(3:10)],function(x){
        var<-as.numeric(x)
        var<-var*dat$loglot
        return(var)
})%>%data.frame()%>%mutate(logsell=dat$logsell)

fit5<-dat2%>%lm(formula = logsell~dat$loglot+dat$ca+ca+dat$ghw+ghw+dat$ffin+ffin+dat$rec+rec+dat$drv+drv+dat$sty+sty+dat$fb+fb+dat$bdms+bdms)
summary(fit5)

anova(fit4,fit5)


fit6<-dat2%>%lm(formula = logsell~dat$ca+ca+dat$ghw+ghw+dat$ffin+ffin+dat$rec+rec+dat$drv+drv+dat$sty+sty+dat$fb+fb+dat$bdms)
summary(fit6)

fit7<-dat2%>%lm(formula = logsell~dat$ca+ca+dat$ghw+dat$ffin+ffin+dat$rec+rec+dat$drv+drv+dat$sty+sty+dat$fb+fb+dat$bdms)
summary(fit7)

fit8<-dat2%>%lm(formula = logsell~dat$ca+ca+dat$ghw+dat$ffin+dat$rec+rec+dat$drv+drv+dat$sty+sty+dat$fb+dat$bdms)
summary(fit8)

fit9<-dat2%>%lm(formula = logsell~dat$ca+ca+dat$ghw+dat$ffin+dat$rec+rec+dat$drv+drv+dat$sty+dat$fb+dat$bdms)
summary(fit9)

fit10<-dat2%>%lm(formula = logsell~dat$ca+dat$ghw+dat$ffin+dat$rec+rec+dat$drv+drv+dat$sty+dat$fb+dat$bdms)
summary(fit10)

fit11<-dat2%>%lm(formula = logsell~dat$ca+dat$ghw+dat$ffin+dat$rec+dat$drv+drv+dat$sty+dat$fb+dat$bdms)
summary(fit11)

anova(fit5,fit11)
