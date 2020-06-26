library(tidyverse);library(readxl)

dir<-"/home/debian/Statistical-practice/data sets/TrainExer61.xlsx"

dat<-read_xlsx(dir)

str(dat)
#a)
dat%>%ggplot(aes(x = 1:nrow(dat),y = X))+geom_line()+theme_classic() #Xt agains t
dat%>%ggplot(aes(x = 1:nrow(dat),y = Y))+geom_line()+theme_classic() #Yt agains t

dat%>%ggplot(aes(x = X,y = Y))+geom_point()+theme_classic() #scatter of X and Y

#b)
fit0<-dat%>%lm(formula = EPSY~EPSX) #make sure that Ex and Ey is uncorrelated
summary(fit0)


#C)
ExL<-list()
for(i in 1:3){
        ExL[[i]]<-lag(dat$EPSX,n = i)
}
ExL<-bind_cols(ExL)

EyL<-list()
for(i in 1:3){
        EyL[[i]]<-lag(dat$EPSY,n = i)
}
EyL<-bind_cols(EyL)


dat2<-bind_cols(dat,ExL,EyL)


name<-c(names(dat),paste0(rep(c("ExL","EyL"),each=3),1:3))
names(dat2)<-name


fit1<-dat2%>%lm(formula = EPSY~EPSX+ExL1+ExL2+ExL3+EyL1+EyL2+EyL3)
summary(fit1)



#d)

fit2<-dat%>%lm(formula = Y~X)
summary(fit2)

et<-fit2$residuals
et_1<-lag(et)

fit3<-lm(et~et_1)
summary(fit3)
