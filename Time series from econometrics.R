library(tidyverse);library(readxl);library(lmtest);library(tseries);library(aTSA);library(lubridate)

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


#######################################################
#Ex 6.4

dir<-"/home/debian/Statistical-practice/data sets/TrainExer64.xlsx"

dat<-read_xlsx(dir)

#Make granger causality of Dx2 on Dx1

#Firstly , make the two lag of each variable 

#Then Building the first modelusing OLS

dat<-dat%>%mutate(Dx1l1=lag(DX1),
                  Dx1l2=lag(DX1,n = 2),
                  Dx2l1=lag(DX2),
                  Dx2l2=lag(DX2,n = 2))

m1_1<-dat%>%lm(formula = DX2~Dx1l1+Dx1l2+Dx2l1+Dx2l2)
m1_2<-dat%>%lm(formula = DX2~Dx2l1+Dx2l2)

anova(m1_1,m1_2) #reject H0 , Dx1 is granger causality of Dx2

#Make granger causality of Dx1 on Dx2
#H0 : gama1 = gama2 = 0 , Dx2 is not granger causality for Dx1

m2_1<-dat%>%lm(formula = DX1~Dx1l1+Dx1l2+Dx2l1+Dx2l2)

m2_2<-dat%>%lm(formula = DX1~Dx1l1+Dx1l2)

anova(m2_1,m2_2) #CAn't reject H:0


#use a granger function in lmtest library

grangertest(dat$DX1~dat$DX2,order=2)
grangertest(dat$DX2~dat$DX1,order=2)



#making ADF test for x1 and x2

adf.test(x = na.omit(dat$X1),k = 1) #-2.76 > -3.5
adf.test(x = na.omit(dat$X2),k = 1) #-1.207 > -3.5

#make it by hand 

Dmaker<-function(x){return(D=x-lag(x))}

lm(dat$DX1~dat$YEAR+I(lag(dat$X1))+I(lag(dat$DX1)))%>%summary()#the most important coefficient here is :
# rho which is the coefficient of Xt_1 and it is -2.76 > -3.5 , reject H0: it is not stationary 

lm(dat$DX2~dat$YEAR+I(lag(dat$X2))+I(lag(dat$DX2)))%>%summary() # rho which is the coefficient of Xt_2 and it is -1.207 > -3.5 , reject H0: it is not stationary 

#Making Eangle granger test for cointegration

#By hand firstly

#regressing X1 on X2

fit<-dat%>%lm(formula = X2~X1)
summary(fit)

et<-fit$residuals
et_1<-lag(et)
et_2<-lag(et_1)
Det<-et-et_1
Det_1<-et_1-et_2
edat<-cbind(et,et_1,Det,Det_1)

lm(formula = Det~et_1+Det_1)%>%summary()#-3.581 < -3.4 , reject H0: et is not stationary


#####################################################################

#EX 6.5 

########

dir<-"/home/debian/Statistical-practice/data sets/TrainExer65.xlsx"


data<-read_xlsx(dir)
#splitting the data 


dat<-data%>%filter(YEAR!="2003"&YEAR!="2004"&YEAR!="2005"&YEAR!="2006"&YEAR!="2007")
test<-data%>%filter(YEAR=="2003"|YEAR=="2004"|YEAR=="2005"|YEAR=="2006"|YEAR=="2007")


dat%>%ggplot(aes(x = YEAR,y = LOGIP,col="red"))+geom_line()+
        geom_line(aes(x=YEAR,y=LOGCLI,col="blue"))+theme_classic()+
        theme(axis.title.y  = element_blank())+scale_color_discrete(name="Variables",labels=c("logip","logcli")) #trend is obvious here 

dat%>%ggplot(aes(x = YEAR,y = GIP,col="red"))+geom_line()+
        geom_line(aes(x=YEAR,y=GCLI,col="blue"))+theme_classic()+
        theme(axis.title.y  = element_blank())+scale_color_discrete(name="Variables",labels=c("GIP","GCLI")) #here the data is detrended and seems to be stationary


#make ADF test for logip and logcli to test stationarity 
tseries::adf.test(x = dat$LOGIP,k = 2,alternative = "explosive")
tseries::adf.test(x = dat$LOGCLI,k = 2,alternative = "explosive")

#make it by hand 

dat%>%lm(formula = GIP~YEAR+I(lag(LOGIP))+I(lag(GIP))+I(lag(GIP,n = 2)))%>%summary() #-2.898 > -3.5 --> non stationary 

dat%>%lm(formula = GCLI~YEAR+I(lag(LOGCLI))+I(lag(GCLI))+I(lag(GCLI,n = 2)))%>%summary() #-1.876 > -3.5 --> non stationary 

#Engle granger test
aTSA::coint.test(X = dat$LOGIP,y  = dat$LOGCLI)

#make it by hand 

fit<-dat%>%lm(formula = LOGIP~LOGCLI)
summary(fit)
et<-fit$residuals

et_1<-lag(et)
et_2<-lag(et,2)
et_3<-lag(et,3)
det<-et-et_1
det_1<-et_1-et_2
det_2<-et_2-et_3

lm(det~et_1+det_1+det_2)%>%summary() #not conintegrated


#testing the granger causality 

grangertest(dat$GIP~dat$LOGCLI,order=2)
grangertest(dat$GCLI~dat$GIP,order=2)

#showing that first 2 order insignificant 

AR2<-dat%>%lm(formula = GIP~I(lag(GIP))+I(lag(GIP,2)))
AR1<-dat%>%lm(formula = GIP~I(lag(GIP)))
AR0<-dat%>%lm(formula = GIP~1)
pred1<-predict(object = AR1,newdata = test)
pred2<-predict(object = AR0,newdata = test)

ADL22<-dat%>%lm(formula = GIP~I(lag(GIP))+I(lag(GIP,2))+I(lag(GCLI))+I(lag(GCLI,2)))
summary(ADL22)

ADL01<-dat%>%lm(formula = GIP~I(lag(GCLI)))
summary(ADL01)

pred3<-predict(ADL01,test)


RMSE<-function(yhat,y){
        for(i in 1:length(y)){
                if(is.na(yhat[i])){y[i]<-NA}
                else{y[i]<-y[i]}
        }
        return(return(sqrt(mean((y-yhat)^2,na.rm = T))))
}

RMSE(yhat = pred1,y = test$GIP)
RMSE(yhat = pred2,y = test$GIP)
RMSE(yhat = pred3,y = test$GIP)

##########################################################################

#Final Ex. in time series module

dir<-"/home/debian/Statistical-practice/data sets/_b224e748359b3e415f11428f1c98a285_TestExer-6-CPI-round1.xlsx"
dat<-read_xlsx(dir)

str(dat)

#plotting the series
par(mfrow=c(2,2))
dat%>%ggplot(aes(x = TREND,y = CPI_EUR))+geom_line()+
        geom_line(aes(x=TREND,y=CPI_USA,col="red"))+theme_bw()

dat%>%ggplot(aes(x = TREND,y = LOGPEUR))+geom_line()+
        geom_line(aes(x=TREND,y=LOGPUSA,col="red"))+theme_bw()


dat<-dat%>%mutate(DPEUR=as.numeric(DPEUR),DPUSA=as.numeric(DPUSA))
dat%>%ggplot(aes(x = TREND,y = DPEUR))+geom_line()+
        geom_line(aes(x=TREND,y=DPUSA,col="red"))+theme_bw()


#Checking for stationarity

aTSA::adf.test(x = dat$LOGPEUR,nlag = 4)#not stationary

adf1<-dat%>%lm(formula = DPEUR~TREND+I(lag(LOGPEUR))+I(lag(DPEUR))+I(lag(DPEUR,2))+I(lag(DPEUR,3)))
summary(adf1) #The same

aTSA::adf.test(x = dat$LOGPUSA,nlag = 4)#not stationary

adf2<-dat%>%lm(formula = DPUSA~TREND+I(lag(LOGPUSA))+I(lag(DPUSA))+I(lag(DPUSA,2))+I(lag(DPUSA,3)))
summary(adf2) #The same


#Checking for cointegration

coint.test(y = dat$CPI_USA,X = dat$LOGPUSA) #not cointegrate

#auto correlation and partial auto correlation
#adding the lag and spliting the data

dat<-dat%>%mutate(DPEUR_6=lag(DPEUR,6),DPEUR_12=lag(DPEUR,12),DPUSA_1=lag(DPUSA),DPUSA_6=lag(DPUSA,6),DPUSA_12=lag(DPUSA,12))

trainndat<-dat%>%filter(TREND%in%c(1:132))

euroac<-acf(x = na.omit(trainndat$DPEUR),type = "correlation",plot = T)

which(abs(euroac$acf)>=(2/sqrt(nrow(trainndat))))

europac<-acf(x = na.omit(dat$DPEUR),type = "partial",plot = T)

which(abs(europac$acf)>=(2/sqrt(nrow(trainndat))))


AReuro612<-trainndat%>%lm(formula = DPEUR~DPEUR_6+DPEUR_12)
summary(AReuro612)

ADL1<-trainndat%>%lm(formula = DPEUR~DPEUR_6+DPEUR_12+DPUSA_1+DPUSA_6+DPUSA_12)
summary(ADL1) #removing lag 6


ADL2<-trainndat%>%lm(formula = DPEUR~DPEUR_6+DPEUR_12+DPUSA_1+DPUSA_12)
summary(ADL2)

testdat<-anti_join(x = dat,y = trainndat,"TREND")
pred1<-predict(object = AReuro612,newdata = testdat)
pred2<-predict(object = ADL2,newdata = testdat)

RMSE(yhat = pred1,y = testdat$DPEUR)

RMSE(yhat = pred2,y = testdat$DPEUR)

testdat<-testdat%>%mutate(pred1,pred2)
testdat%>%ggplot(aes(x = TREND,y = DPEUR))+geom_line()+geom_line(aes(x=TREND,y = pred1),col="blue")+geom_line(aes(x=TREND,y = pred2),col="red")
