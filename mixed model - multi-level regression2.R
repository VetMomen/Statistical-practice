library(tidyverse)
library(nlme)
library(lme4)
library(car)
data<-read.csv("./data sets/mlm.csv")

summary(data)
str(data)
        
data%>%ggplot(aes(x=attitude,y=frequency,col=subject))+geom_boxplot()+facet_wrap(.~gender)

mod1<-data%>%with(lmer(frequency~attitude+(1|gender)))

summary(mod1)
coef(mod1)
##########################################################

mat<-c(1,1,23,33,0,
2,1,22,33,0,
3,1,20,27,0,
4,1,19,25,0,
5,2,16,22,0,
6,2,17,21,0,
7,2,18,28,0,
8,2,19,31,0,
9,3,25,28,0,
10,3,28,38,0,
11,3,29,35,0,
12,3,31,34,0,
13,4,27,38,0,
14,4,23,27,0,
15,4,22,28,0,
16,4,21,25,0,
17,5,32,28,0,
18,5,31,37,0,
19,5,28,33,0,
20,5,26,30,0,
21,6,13,27,1,
22,6,12,22,1,
23,6,14,34,1,
24,6,15,28,1,
25,7,16,30,1,
26,7,17,37,1,
27,7,14,27,1,
28,7,12,25,1,
29,8,11,28,1,
30,8,10,23,1,
31,8,20,34,1,
32,8,15,33,1,
33,9,21,29,1,
34,9,18,31,1,
35,9,19,30,1,
36,9,23,39,1,
37,10,18,27,1,
38,10,17,36,1,
39,10,16,36,1,
40,10,23,32,1)%>%matrix(ncol = 5,byrow = T)





colnames(mat)<-c("cleintid","cluid","empathy","contentment","method")

data<-mat%>%data.frame()
data<-data%>%mutate(cleintid=factor(cleintid),cluid=factor(cluid),method=factor(method))


data%>%group_by(method)%>%summarize(mean(empathy),sd(empathy),n(),mean(contentment),sd(contentment))

#adding the other important variables

#adding the cluster contenment mean

clcontenmean<-data%>%group_by(cluid)%>%summarize(CCmean=mean(contentment))

data<-data%>%left_join(clcontenmean,by="cluid")

#adding the group mean centring variable

data<-data%>%group_by(cluid)%>%mutate(groupc=contentment-CCmean)



#adding the grand mean centering variable 


data<-data%>%group_by(cluid)%>%mutate(grandc=contentment-mean(data$contentment))


#client  - level-1 - unconditional model 

uncmodfixed<-data%>%with(gls(empathy~method,data,method = "REML"))
uncmod<-data%>%with(lme(empathy~method,random = ~1|cluid,data,method = "REML"))

summary(uncmodfixed)
summary(uncmod)

anova(uncmodfixed,uncmod)

fixedm<-data%>%with(gls(empathy~method+contentment,data,method = "REML"))

mod3prim<-data%>%with(gls(empathy~method+groupc+CCmean+groupc:method+groupc:CCmean,method = "REML"))

mod3ri<-data%>%with(lme(empathy~method+CCmean+groupc,random = list(~1|cluid),method = "REML"))

mod3rc<-data%>%with(lme(empathy~method+groupc+CCmean+groupc:method+groupc:CCmean,random = list(~method|cluid),method = "REML"))

anova(mod3ri,mod3rc)

summary(mod3ri)

co<-coef(mod3ri)%>%data.frame()

data%>%ggplot(aes(y=groupc,x=method))+
        geom_point()+
        geom_abline(slope = c(co$method1),intercept = c(co$X.Intercept.))


VarCorr(mod3rc)
deviance(mod3rc)
vcov.merMod(mod3ri)
