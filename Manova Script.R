#This a tutorial for doing between subject MANOVA

#you are welcome to contact me for any advice or recommendation at: vet.m.mohamed@gmail.com

#for Complete tutorial and data set see [here](http://statstools.com/learn/advanced-statistics/)


#first of all lets load libraries which we are going to use in this Analysis and clearing the workspace

ls()%>%rm

library(tidyverse)
library(car)
library(multcomp)
library(ez)
library(MOTE)
library(psych)

#Then lets import the data set

data<-read.csv("./data sets/9 manova.csv")
head(data)

#lets take the analysis variables out 

data<-data[,c("FEM","MASC","ESTEEM","ATTROLE","NEUROTIC")]

#adding individual index

data<-data%>%mutate(idx=1:n())

#Now lets test the assumptions and the appropiateness of the data to MANOVA

#We will start by accuracy and missing data

summary(data)


#In Neurotic we have a large numbers of NA's 

#So we have to see the percentage of the missing data 

apply(data,2,function(x){
        sum(is.na(x))/length(x)
})

#We have here 17.8% of data in neurotic are missing so we can't do imputation 


#Now lets test for outliers using mahalanobis test

mah<-mahalanobis(x = data[,-c(1:2)],
                 center = colMeans(data[,-c(1:2)]),cov = cov(data[,-c(1:2)]))

#determining the cutoff point through chisquare distribution 

cut<-qchisq(p = .99,df = 2)

#summary of the outliers

summary(mah<cut)

#we have 9 outliers and we have to remove them 

data<-data[which(mah<cut),]

#Now lets move to the second phase :

#testing the normality - linearity - Himogeniety - Homoscdasticity

par(mfrow=c(2,2))

qqnorm(data$ESTEEM%>%jitter(factor = 5),main = "QQplot for self esteem")
qqnorm(data$ATTROLE%>%jitter(factor = 5),main = "QQplot for Attitude to roles")
qqnorm(data$NEUROTIC%>%jitter(factor = 5),main = "QQplot for neurotic")


#Other way : making fake data for fake regression to test the assumption in multivariate way
set.seed(345)
rfake<-rchisq(n = nrow(data),7)

#the df is arbitrary number 

fakelm<-data%>%with(lm(rfake~ESTEEM+ATTROLE+NEUROTIC))

#Now lets Extract the residual and fitted values 

stdresid<-resid(fakelm)%>%scale()
stdfit<-fitted(fakelm)%>%scale()

#start with normality 
#we can test it using shapiro test for the residuals or using the qqnorm plot

#lest try shapiro test for the normality testing 

shapiro.test(resid(fakelm))
hist(stdresid)
#Congrats : we have P > .001
#So we assume the normality here 

#lets try testing the linearity 
#No way to plot qqnorm :D

qqPlot(stdresid)

#fair enough !! got good data here 

#Now its time for homogeniety and homoscdasticity

plot(stdfit,stdresid)
abline(h = 0)
abline(v = 0)

#good , the data here is someking of homogeneic 

# but we still need use test like levene to prove it 

data%>%with(leveneTest(ESTEEM~FEM*MASC),center="mean") #testing for self Esteem
data%>%with(leveneTest(ATTROLE~FEM*MASC),center="mean") #testing for Attitude toword female roles
data%>%with(leveneTest(NEUROTIC~FEM*MASC),center="mean") # testing for neuroticism

#the size of the sample Affect greatly on the signifance level 
#so we will choose p.value =.001

#all Homogeneity tests is p>.001

#so we will assume the homogeneity here 

#now lets take more steps further toword manova analysis 


DV = cbind(data$ESTEEM, data$ATTROLE, data$NEUROTIC)#combining the dependent variables together

man<-data%>%with(lm(DV~FEM*MASC,data = data,
                    contrasts = list(FEM=contr.sum,MASC=contr.sum)))

manout<-Manova(mod = man,"III") #type three SS

summary(manout,multivariate = T)

#here we see the the significant main effect of the IVs 

#the effect size here = 1- wilk's lambda 

1-0.917827 #effect size of FEM
1-0.8237555 #effect size of MASC
1-0.9933737 #effect size of interaction



#here we will do post-hoc test --> single anova for each DV

#self esteem
data%>%with(ezANOVA(data = data,dv = ESTEEM,
                    wid = idx,between = c(FEM,MASC),type = 3))

#Attitude
data%>%with(ezANOVA(data = data,dv = ATTROLE,
                    wid = idx,between = c(FEM,MASC),type = 3))

#Neurotic

data%>%with(ezANOVA(data = na.omit(data),dv = NEUROTIC,
                    wid = idx,between = c(FEM,MASC),type = 3))

#thankfull the IVs is two level only and there is no interaction effect 
#so the p-value in anova is enough and Explain only the mean 

#but we still need to calculate the effect size of eache sig. different means using MOTE library or cohen_d() function in psych package

#using cohen.d()

data[,-c(2,6)]%>%cohen.d(group = "FEM")

data[,-c(1,6)]%>%cohen.d(group = "MASC")

#using d.ind.t()

data%>%group_by(FEM)%>%summarize(mean(ATTROLE,na.rm = T),
                                      sd(ATTROLE,na.rm = T),n())

d.ind.t(m1 = 36.0,m2 = 32.9,sd1 = 6.80,sd2 = 6.41,n1 = 202,n2 = 90)$d

#we here see that the effect size using cohen.d() is soo close to d.ind.t() for attribute on each group of FEM
#but cohen.d() here is more easier 

