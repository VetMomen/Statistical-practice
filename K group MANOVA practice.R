library(tidyverse)
library(car)
library(ez)
#the data

g1<-c(2,3,5,2,3,4,4,5)%>%matrix(ncol = 2,byrow = F)%>%
        data.frame()%>%mutate(gp=rep(1,4))%>%as.matrix()

g2<-c(4,5,6,8,6,7)%>%matrix(ncol = 2,byrow = F)%>%
        data.frame()%>%mutate(gp=rep(2,3))%>%as.matrix()

g3<-c(7,8,10,9,7,6,7,8,5,6)%>%matrix(ncol = 2,byrow = F)%>%
        data.frame()%>%mutate(gp=rep(3,5))%>%as.matrix()

data<-rbind(g1,g2,g3)%>%as.matrix()

#group mean calculation

mean1<-colMeans(g1[,1:2])%>%as.matrix()%>%t()
mean2<-colMeans(g2[,1:2])%>%as.matrix()%>%t()
mean3<-colMeans(g3[,1:2])%>%as.matrix()%>%t()

#group deviation calculation
dev1<-apply(g1[,1:2],1,function(x){
        x-mean1
})%>%as.matrix()%>%t()


dev2<-apply(g2[,1:2],1,function(x){
        x-mean2
})%>%as.matrix()%>%t()

dev3<-apply(g3[,1:2],1,function(x){
        x-mean3
})%>%as.matrix()%>%t()

#within sscp calculation

sscpW1<-t(dev1)%*%dev1
sscpW2<-t(dev2)%*%dev2
sscpW3<-t(dev3)%*%dev3

Wsscp<-sscpW1+sscpW2+sscpW3

#########################3

#total sscp calculation

#grand mean calculation

gmean<-colMeans(data[,1:2])%>%as.matrix()%>%t()

#deviation from grand mean 
gdev<-apply(data[,1:2],1,function(x){
        x-gmean
})%>%as.matrix()%>%t()

#total sscp
Tsscp<-t(gdev)%*%gdev

#between sscp
Bsscp<-Tsscp-Wsscp

#lambda calculation 

#calculation of determinants 

detW<-det(Wsscp)
detB<-det(Bsscp)
detT<-det(Tsscp)

lambda<-detW/detT

print(lambda)

#Calculation of chi sq Approximation

P<-2
K<-3
N<-nrow(data)

chi<- -((N-1)-(.5*(P+K)))*log(lambda)

print(chi)

1-pchisq(q = chi,df = P*(K-1))


################################
################################

#MANOVA using multivariate regression

data<-data%>%data.frame()%>%mutate(gp=factor(gp))


fit<-lm(cbind(X1,X2)~.,data)

summary(fit)

linearHypothesis(model = fit,hypothesis.matrix = c("gp2=0","gp3=0"))

#post hoc anovas

#adding index

data<-data%>%group_by(gp)%>%mutate(idx=1:n(),
                                   idx=as.character(idx))

X1fit<-ezANOVA(data = data,dv = X1,wid = idx,between = gp,return_aov = T)$aov

X2fit<-ezANOVA(data = data,dv = X2,wid = idx,between = gp,return_aov = T)$aov

TukeyHSD(X1fit)
TukeyHSD(X2fit)

