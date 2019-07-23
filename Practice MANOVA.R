library(tidyverse)
library(car)
library(caret)
library(ez)
mat<-c(2.0,2.5,2.5,1.5,3.5,2.5,1.0,2.0,1.0,
1.5,2.0,1.5,1.0,4.5,2.5,1.0,2.0,1.5,
2.0,3.0,2.5,3.0,3.0,3.0,1.5,1.0,1.0,
2.5,4.0,3.0,4.5,4.5,4.5,2.0,2.5,2.0,
1.0,2.0,1.0,1.5,4.5,3.5,2.0,3.0,2.5,
1.5,3.5,2.5,2.5,4.0,3.0,2.5,3.0,2.5,
4.0,3.0,3.0,3.0,4.0,3.5,2.0,2.5,2.5,
3.0,4.0,3.5,4.0,5.0,5.0,1.0,1.0,1.0,
3.5,3.5,3.5,0,0,0,1.0,1.5,1.5,
1.0,1.0,1.0,0,0,0,2.0,3.5,2.5,
1.0,2.5,2.0,0,0,0,0,0,0)%>%matrix(ncol = 9,byrow = T)

gp1<-mat[,1:3]
gp2<-mat[1:8,4:6]
gp3<-mat[1:10,7:9]

colnames(gp1)<-c("y1","y2","y3")
colnames(gp2)<-c("y1","y2","y3")
colnames(gp3)<-c("y1","y2","y3")

gp1<-gp1%>%data.frame()%>%mutate(gp=1)
gp2<-gp2%>%data.frame()%>%mutate(gp=2)
gp3<-gp3%>%data.frame()%>%mutate(gp=3)

data<-rbind(gp1,gp2,gp3)%>%mutate(gp=factor(gp))


#running manova

fit<-data%>%with(lm(cbind(y1,y2,y3)~gp-1,data))
summary(fit)
coef(fit)

linearHypothesis(fit,hypothesis.matrix =c("gp1=gp2","gp2=gp3") ,type="III")

#posthoc anova

data<-data%>%group_by(gp)%>%mutate(idx=1:n(),
                                   idx=as.character(idx))

y1anova<-ezANOVA(data = data,wid = idx,dv = y1,between = gp,return_aov = T)$aov
y2anova<-ezANOVA(data = data,wid = idx,dv = y2,between = gp,return_aov = T)$aov
y3anova<-ezANOVA(data = data,wid = idx,dv = y3,between = gp,return_aov = T)$aov

#y2 and y3 responsible for difference 

#tukey

TukeyHSD(y2anova)
TukeyHSD(y3anova)
