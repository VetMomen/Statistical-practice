#Hypothesis Testing examples

library(tidyverse)

dir<-"/home/debian/Statistical-practice/data sets/Auction Data for Monet Paintings.csv"

dat<-read.csv(dir)

dat$lnarea<-log(dat$HEIGHT*dat$WIDTH)

dat$aspect<-dat$HEIGHT/dat$WIDTH

dat$lnprice<-log(dat$PRICE)

fit<-dat%>%lm(formula = lnprice~lnarea+aspect)



#testing single hypothesis using wald method by deviding the coef on the Se of this coef and get t ratio



X<-bind_cols(c=rep(1,nrow(dat)),dat$lnarea,dat$aspect)%>%as.matrix()
XX<-t(X)%*%X

cof<-solve(XX)%*%t(X)%*%dat$lnprice
I<-matrix(rep(0,430*430),nrow = 430)
diag(I)<-1
M<-I-(X%*%solve(XX)%*%t(X))

e<-M%*%dat$lnprice
n<-nrow(dat)
k<-3
S2<-(t(e)%*%e)/(n-k)
SI<-matrix(rep(0,9),nrow = 3)
diag(SI)<-S2

bcov<-SI%*%solve(XX)

bse<-diag(bcov)%>%sqrt

t<-cof/bse

quant<-qt(p = .975,df = n-k)
abs(t)>quant

############################################################################
############################################################################

#using F statistics

#B2=0
#B3=0

R<-matrix(data = c(0,1,0,0,0,1),nrow = 2,byrow = T)
q<-matrix(c(0,0),nrow = 2)

Rb<-R%*%cof
m<-Rb-q

D<-R%*%bcov%*%t(R)

numerator<-t(m)%*%solve(D)%*%m
j=2

Fvalue<-numerator/j

Fvalue>qf(p = .95,df1 = 3,df2 = n-k)

summary(fit)
##################################################
#using  restricted coef method to calculate the significance of loss of fit 

R_<-matrix(data = c(0,1,0),nrow = 1)
q_<-matrix(data = c(0),nrow = 1)
C<-solve(XX)%*%t(R_)%*%solve(R_%*%solve(XX)%*%t(R_))

m_<-(R_%*%cof)-q_
b_<-cof-(C%*%m_) #restricted coef

b_cov<-bcov-(SI%*%C%*%R_%*%solve(XX))

yfit<-X%*%b_

e_<-dat$lnprice-yfit


e_e_<-t(e_)%*%e_
ee<-t(e)%*%e

Fvalue<-((e_e_-ee)/1)/(ee/(n-k))

qf(p = .95,df1 = 1,df2 = n-k)


fit2<-dat%>%lm(formula = lnprice~aspect)
anova(fit,fit2) #the same F value and RSS


