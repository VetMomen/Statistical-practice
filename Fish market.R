library(tidyverse);library(mice);library(MASS);library(lmtest)

dir<-"/home/debian/Statistical-practice/data sets/datasets_229906_491820_Fish.csv"


dat<-read_csv(dir)

datname<-names(dat)
print(datname)

#descrive each variable

#variable species

class(dat$Species)
unique(dat$Species)

#convert it to factor

dat$Species<-factor(dat$Species)

#frequancy
table(dat$Species)%>%barplot()

#variable weight : supposed to be the y variable

class(dat$Weight)

summary(dat$Weight)

#looking at dist

dat$Weight%>%data.frame()%>%ggplot(aes(x=.))+
        geom_density()

#the data is skewed to right , we can't transform it using log as it has zeros
#get the abs center

cweight<-abs(dat$Weight-mean(dat$Weight))

#make sure that the dist doesn't changed

cweight%>%data.frame()%>%ggplot(aes(x=.))+
        geom_density()

#make log
lweihgt<-log(dat$Weight)
lweihgt%>%data.frame()%>%ggplot(aes(x=.))+
        geom_density()

qqnorm(lweihgt)

#it doesn't work well

#try sqrt transform

sqweight<-dat$Weight^(1/2)
qqnorm(sqweight)

sqweight%>%data.frame()%>%ggplot(aes(x=.))+
        geom_density()


#still need some work
#try box-cox , we will generate 10 variable with different lambda and see which is better

boxx<-function(x,lamda){
        return(y=(((x^lamda)-1)/lamda))
}
y<-list()
for(i in c(.1,.2,.3,.4,.5,.6,.7,.8,.9)){
        y[[i*10]]<-boxx(x = dat$Weight,lamda = i)
}
par(mfrow=c(3,4))
sapply(y,hist)
sapply(y,qqnorm)

#box with lambda=.4 is looks good 
tweight<-boxx(x = dat$Weight,lamda = .4)
hist(tweight)

tweight%>%data.frame()%>%ggplot(aes(x=.))+
        geom_density()


dat$tweight<-tweight

boxplot(tweight) #good without outlier

#looking at the rest of variables and the relation with weight

plot(dat$tweight,dat$Length1)
plot(dat$tweight,dat$Length2)
plot(dat$tweight,dat$Length3)
plot(dat$tweight,dat$Height)
plot(dat$tweight,dat$Width)

#testing if there is MV outlier

mahl<-mahalanobis(x = dat[,c("Width","Height","Length3","Length2","Length1")],
                  center = colMeans(dat[,c("Width","Height","Length3","Length2","Length1")]),cov(dat[,c("Width","Height","Length3","Length2","Length1")]))

cut<-qchisq(p = .99,df = 4)
outliers<-which(abs(mahl)>cut)

dat[outliers,]

#i am going to leave them unless they cause any prob.

#building the model

#from full to specific 

#i removed length 2,3 as it is a function of length 1
fit1<-dat%>%lm(formula = tweight~Width+Height+Length1+Species)
summary(fit1)

#species have no effect 

fit2<-dat%>%lm(formula = tweight~Width+Height+Length1)
summary(fit2)

anova(fit1,fit2)

#good , chhose fit2

#Evaluation

#stutentized Vs fitted
stud<-rstudent(fit2)
plot(y = fit2$fitted.values,x=stud)

#we have one outlier in the data , we can detect it using cooksD or hat value or even normalization

which(abs(scale(fit2$residuals))>3)

#removing it and try again

dat<-dat[-41,]
fit2<-dat%>%lm(formula = tweight~Width+Height+Length1)
stud<-rstudent(fit2)
plot(y = fit2$fitted.values,x=stud)

resettest(fit2,power = 2)

