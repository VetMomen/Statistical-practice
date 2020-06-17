#in this practice i want to see if chow_break may help in idetifying the homogeniety 
#also i want to test homogeneity by testing the correlation between residuals and each of independent variables 
#finally , is scatter plot between all variables helps in specifying the instruments ?
#testing of exogeniety of X using Haussman test after correlating x on instruments 

library(tidyverse);library(readxl);library(lmtest)
dir<-"/home/debian/Statistical-practice/data sets/_46d7b6e393ed4cd5a42ba1ec49adede2_TestExer4_Wage-round1.xls"
dat<-read_xls(dir)

#see the structure of the data 

str(dat)

#looking at the scatter between variables 
pairs(dat,lower.panel = NULL)

par(mfrow=c(3,3))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = dat$logw)
})


#i can't determine the best relation 
#i will do from full to specific regression "backward"

fitfull<-dat%>%lm(formula = logw~educ+age+exper+smsa+south+nearc+daded+momed)
summary(fitfull)

par(mfrow=c(3,4))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = fitfull$residuals)
})

#see with wich Var , experience is correlates
par(mfrow=c(3,3))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = dat$exper)
})

#the problem is with both education and age 
#i will remove them and do regression again 

fit2<-dat%>%lm(formula = logw~exper+smsa+south+nearc+daded+momed)
summary(fit2)

par(mfrow=c(3,4))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = fit2$residuals)
})
#inserting age

fit3<-dat%>%lm(formula = logw~exper+age+smsa+south+nearc+daded+momed)
summary(fit3)

par(mfrow=c(3,4))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = fit3$residuals)
})

#instering educ only

fit4<-dat%>%lm(formula = logw~exper+educ+smsa+south+nearc+daded+momed)
summary(fit4)


par(mfrow=c(3,4))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = fit4$residuals)
})

#removing experience also

fit5<-dat%>%lm(formula = logw~smsa+south+nearc+daded+momed)
summary(fit5)

par(mfrow=c(3,4))
sapply(X = dat,FUN = function(x){
        plot(x = x,y = fit5$residuals)
})


#adding educ only

fit6<-dat%>%lm(formula = logw~educ+smsa+south+nearc+daded+momed)
summary(fit6)

#adding age only

fit7<-dat%>%lm(formula = logw~age+smsa+south+nearc+daded+momed)
summary(fit7)


#the scatter between residual and every x doesn't work in case of multiple fit
#try another stratigy and building a model which apperently good

#removing experience , nearc and daded

fit8<-dat%>%lm(formula = logw~educ+age+smsa+south+momed)
summary(fit8)

#see the correlation between variables and residual
#by regressing r on x or simple correlation or scatter

rcorr<-dat%>%lm(formula = fit8$residuals~educ)
summary(rcorr)
#using regression doesn't work ###

cor(cbind(fit8$residuals,dat$educ,dat$age,dat$smsa,dat$south,dat$momed))
#correlation also doesn't work

#looking at scatter all again ###
pairs(dat,lower.panel = NULL)

#try building model from specific to general by correlation
#see the correlation between Xs 
cor(dat)

#we have a comment on educ , age , experience , daded ,momed , smsa

fit9<-dat%>%lm(formula = logw~1)
summary(fit9)

fit10<-dat%>%lm(formula = logw~1+educ)
summary(fit10)

fit11<-dat%>%lm(formula = logw~1+educ+age)
summary(fit11)

fit12<-dat%>%lm(formula = logw~1+educ+age+south)
summary(fit12)

fit13<-dat%>%lm(formula = logw~1+educ+age+south+smsa)
summary(fit13)

fit14<-dat%>%lm(formula = logw~1+educ+age+south+smsa+momed)
summary(fit14)

#testing the difference
anova(fit9,fit10,fit11,fit12,fit13,fit14)
BIC(fit9)
BIC(fit10)
BIC(fit11)
BIC(fit12)
BIC(fit13)
BIC(fit14)

#testing for polynomial
resettest(fit14,2)

#Chow test 
        #split:

set.seed(4444)
y1sample<-sample(x = 1:length(dat$logw),size = .5*length(dat$logw),replace = F)

dat1<-dat[y1sample,]
dat2<-dat[-y1sample,]

chowfity1<-dat1%>%lm(formula = logw~1+educ+age+south+smsa+momed)
summary(chowfity1)

chowfity2<-dat2%>%lm(formula = logw~1+educ+age+south+smsa+momed)
summary(chowfity2)

s0<-(t(fit14$residuals)%*%fit14$residuals)
s1<-t(chowfity1$residuals)%*%chowfity1$residuals
s2<-t(chowfity2$residuals)%*%chowfity2$residuals

f<-((s0-s1-s2)/6)/((s1+s2)/2998)

1-pf(q = f,df1 = 6,df2 = 2998)

#testing the endogeneity of educ , age , momed 

#educ##########################################
educsls1<-dat%>%lm(formula = educ~1+age+south+smsa+momed+daded+exper)
summary(educsls1)

educ2<-predict(educsls1)
educsls2<-dat%>%lm(formula = logw~1+educ2+age+south+smsa+momed)

summary(educsls2)

esls2<-dat$logw-(as.matrix(cbind(rep(1,nrow(dat)),dat[,c("educ","age","south","smsa","momed")]))%*%coef(educsls2))

sargantfit<-dat%>%lm(formula = esls2~1+age+south+smsa+momed+daded+exper)
summary(sargantfit)
nR2<-8.307e-06*nrow(dat)
nR2>qchisq(p = .99,df = (7-6)) #daded & experience not instruments for education


v<-educsls1$residuals
e<-fit14$residuals
H_educ<-dat%>%lm(formula = e~1+educ+age+south+smsa+momed+v)
summary(H_educ)

nR2<-3.472e-05*nrow(dat)

nR2>qchisq(p = .99,df = 1) #education not endogenic 

################################################################

#age##########################################
agesls1<-dat%>%lm(formula = age~1+educ+south+smsa+momed+daded+exper)
summary(agesls1)

age2<-predict(agesls1)
agesls2<-dat%>%lm(formula = logw~1+age2+educ+south+smsa+momed)

summary(agesls2)

esls2<-dat$logw-(as.matrix(cbind(rep(1,nrow(dat)),dat[,c("age","educ","south","smsa","momed")]))%*%coef(agesls2))

sargantfit<-dat%>%lm(formula = esls2~1+educ+south+smsa+momed+daded+exper)
summary(sargantfit)
nR2<-8.307e-06*nrow(dat)
nR2>qchisq(p = .99,df = (7-6)) #daded & experience not instruments for education


v<-agesls1$residuals
e<-fit14$residuals
H_age<-dat%>%lm(formula = e~1+educ+age+south+smsa+momed+v)
summary(H_age)

nR2<-3.472e-05*nrow(dat)

nR2>qchisq(p = .99,df = 1) #age not endogenic 

################################################################