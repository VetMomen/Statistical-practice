library(tidyverse)
library(car)
library(effects)
library(multcomp)

#Example I
###########

mat<-c(1,15,17,3,
1,10,6,3,
1,13,13,1,
1,14,14,8,
1,12,12,3,
1,10,9,9,
1,12,12,3,
1,8,9,12,
1,12,15,3,
1,8,10,8,
1,12,13,1,
1,7,11,10,
1,12,16,1,
1,9,12,2,
1,12,14,8,
2,9,9,3,
2,13,19,5,
2,13,16,11,
2,6,7,18,
2,10,11,15,
2,6,9,9,
2,16,20,8,
2,9,15,6,
2,10,8,9,
2,8,10,3,
2,13,16,12,
2,12,17,20,
2,11,18,12,
2,14,18,16)%>%matrix(ncol = 4,byrow = T)


colnames(mat)<-c("GPID", "PRECOMP" ,"POSTCOMP", "POSTHIOR")

data<-mat%>%data.frame()%>%mutate(GPID=factor(GPID))

#testing the effect of covariate

fit<-manova(cbind(POSTCOMP,POSTHIOR)~PRECOMP,data)
summary.manova(fit,test = "Wilks")


#testing the interaction of covariate

fit2<-manova(cbind(POSTCOMP,POSTHIOR)~PRECOMP+GPID+PRECOMP*GPID,data)
summary.manova(fit2,test = "Wilks")

#testing the stability 
(1+(2-1))/nrow(data)<.1





#Example II
###########

mat<-c(1,91,81,70,102,1,107,132,121,71,1,121,97,89,76,1,86,88,80,85,
1,137,119,123,117,1,138,132,112,106,1,133,116,126,97,
1,127,101,121,85,1,114,138,80,105,1,118,121,101,113,1,114,72,112,76,
2,107,88,116,97,2,76,95,77,64,2,116,87,111,86,2,126,112,121,106,
2,104,107,105,113,2,96,84,97,92,2,127,88,132,104,2,99,101,98,81,
2,94,87,85,96,2,92,80,82,88,2,128,109,112,118,
3,121,134,96,96,3,140,130,120,110,3,148,123,130,111,3,147,155,145,118,
3,139,124,122,105,3,121,123,119,122,3,141,155,104,139,3,143,131,121,103,
3,120,123,80,77,3,140,140,121,121,3,95,103,92,94)%>%matrix(ncol = 5,byrow = T)

colnames(mat)<-c("GPID", "AVOID", "NEGEVAL", "PREAVOID", "PRENEG")

data<-mat%>%data.frame()%>%mutate(GPID=factor(GPID,levels = unique(GPID),labels = c("Group1","Group2","Group3")))

str(data)

#descriptive

data%>%group_by(GPID)%>%summarize(mean(AVOID),mean(NEGEVAL),mean(PREAVOID),mean(PRENEG),
                                  sd(AVOID),sd(NEGEVAL),sd(PREAVOID),sd(PRENEG))%>%t()

#testing the relationship of pre and post ( covariate and dependent)

fit<-lm(cbind(AVOID,NEGEVAL)~ PREAVOID + PRENEG,data)
Manova(fit,type="III",test = "Wilks")

#testing the interaction

fit2<-lm(cbind(AVOID,NEGEVAL) ~ GPID * (PREAVOID + PRENEG),data)

Manova(fit2,type="III",test="Wilks") #interaction is not sig 


#Tukey and multible combarison

fit3<-lm(AVOID~ GPID + PREAVOID + PRENEG-1,data)

glht(model = fit3,linfct=mcp(GPID="Tukey"))%>%summary

effect("GPID",fit3)

fit4<-lm(NEGEVAL ~ GPID + PREAVOID + PRENEG-1,data)

glht(model = fit4,linfct=mcp(GPID="Tukey"))%>%summary

effect("GPID",fit4)

###################################
#Homogeneity
fitbox<-lm(cbind(AVOID,NEGEVAL) ~ GPID,data)
heplots::boxM(fitbox)

#within multivariate outlier

data<-data%>%group_by(GPID)%>%mutate(mah=mahalanobis(cbind(AVOID,NEGEVAL,PREAVOID,PRENEG),center = colMeans(cbind(AVOID,NEGEVAL,PREAVOID,PRENEG)),cov = cov(cbind(AVOID,NEGEVAL,PREAVOID,PRENEG))))
cut<-qchisq(p = .001,df = 4)
data<-data%>%mutate(out=mah<cut)

