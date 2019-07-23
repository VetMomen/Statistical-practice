library(tidyverse)
library(MVN)
library(heplots)
library(effects)
library(psych)
library(MOTE)

mat<-c(1.00,12.00,13.00,3.00,
1.00,10.00,6.00,5.00,
1.00,11.00,17.00,2.00,
1.00,14.00,14.00,8.00,
1.00,13.00,12.00,6.00,
1.00,10.00,6.00,8.00,
1.00,8.00,12.00,3.00,
1.00,8.00,6.00,12.00,
1.00,12.00,12.00,7.00,
1.00,10.00,12.00,8.00,
1.00,12.00,13.00,2.00,
1.00,7.00,14.00,10.00,
1.00,12.00,16.00,1.00,
1.00,9.00,9.00,2.00,
1.00,12.00,14.00,10.00,
2.00,9.00,10.00,6.00,
2.00,16.00,16.00,8.00,
2.00,11.00,17.00,8.00,
2.00,8.00,16.00,21.00,
2.00,10.00,14.00,15.00,
2.00,7.00,18.00,12.00,
2.00,16.00,20.00,7.00,
2.00,9.00,12.00,9.00,
2.00,10.00,11.00,7.00,
2.00,8.00,13.00,4.00,
2.00,16.00,19.00,6.00,
2.00,12.00,15.00,20.00,
2.00,15.00,17.00,7.00,
2.00,12.00,21.00,14.00)%>%matrix(ncol = 4,byrow = T)

colnames(mat)<-c("gp","x","y1","y2")

data<-mat%>%data.frame()%>%mutate(gp=factor(gp))
str(data)


#outlier

Moutlier<-data%>%group_by(gp)%>%mutate(mah=mahalanobis(x = cbind(x,y1,y2),center = colMeans(cbind(x,y1,y2)),cov = cov(cbind(x,y1,y2))))%>%
        select(gp,mah)%>%mutate(out=mah<qchisq(.001,3))%>%
        select(gp,out)
any(Moutlier$out)

#no outlier

#Multivariate normality

mvn(data = data,subset = "gp",multivariatePlot = T)

#we have issue in univariate y2 in gp 2 
#we will transform it 

par(mfrow=c(2,3))
for(i in 1:2){
        for(j in c("x","y1","y2")){
                gp<-data%>%filter(gp==i)%>%select(x,y1,y2)
                hist(gp[,j],main = paste("histogram of variable",j,"group",i,sep=" "))
        }
}

y2trans<-data$y2^(1/2)
hist(y2trans)
shapiro.test(y2trans)

#adding it to the data 

data<-data%>%mutate(y2trans=y2trans)

#run it again

mvn(data = data,subset = "gp",multivariatePlot = T)

#testing the homogeneity (multi - uni)

homodel<-lm(cbind(y1,y2)~gp,data)

boxM(homodel,"gp")%>%summary

#testing the significant effect of the covariate

covmodel<-lm(cbind(y1,y2)~x,data)

Manova(covmodel,type="III",test="Wilks")

#testing the interaction

intermodel<-lm(cbind(y1,y2)~gp*x,data)

Manova(intermodel,type="III",test="Wilks")

#there is no sig interaction

#testing the effect of group after isolating x

gpmodel<-lm(cbind(y1,y2)~gp+x,data)
Manova(gpmodel,type="III",test="Wilks")

#running post hoc anova to know where is the difference

summary.aov(gpmodel)

#the effect is in both variables 

#getting the adjusted mean

effect(term = "gp",mod = gpmodel)




