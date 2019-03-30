
#this is a tutorial about how to do mediation analysis 

#For any recommendation of advices plz feel free to contact me : vet.m.mohamed@gmail.com

#Need at first to load our libraries 

library(tidyverse)
library(mediation)
library(knitr)
library(caret)
library(lavaan)


#Now lets import our data and inspect it 

download.file(url = 'http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv',destfile = "./data sets/medanalysis.csv")

data<-read.csv("./data sets/medanalysis.csv")

head(data)

#here we have three variables x and y and the mediator M between x and y

#first lets look at their features graphically befor starting analysis 

plot(data)# seeing the corelation and linearity between each pairs


par(mfrow=c(1,3))
for(i in names(data)){
        density(data[,i])%>%plot(main=paste("density plot of variable",i,sep = " "))
} # seeing the normality of each variable

par(mfrow=c(1,3))
for(i in names(data)){
        d<-jitter(data[,paste(i)],factor = 10)
        qqnorm(d,main = paste("Normal QQ plot of",i,sep = " "))
} #seeing the normality and linearity using QQ plot 


#Now lets dive directly to mediation 

#1st model x~y

fit1<-lm(Y~X,data=data)
summary(fit1)

#the first model is significant 

#now lets run the 2nd model m~x

fit2<-lm(M~X,data=data)
summary(fit2)

#great this is also significant 

#Lets run the model 3 y~X+M --> adjusting for X

fit3<-lm(Y~X+M,data)

summary(fit3)

#here , M is significant amd X reduced to be completely non significant 

#So we here in front of case of complete mediation 

#but we still need to test if this mediation effect is significant or not 

#So we will use bootstrabing method to test that 

medeffect<-mediate(model.m = fit2,model.y = fit3,sims = 1000,boot = T,boot.ci.type = "perc",treat = "X",mediator = "M")

summary(medeffect)

#here we see ACME : Average causal mediation Effect is significant ,
#and see that ADE : average direct effect is not significant 

#This indicate that the mediation effect is significant !!


#Now i want to test the nediation effect using Lavaan and structural equation modeling

model<-"

Y~a*X+b*M
M~c*X

ind:=b*c
"


#now run the model through cfa() function

medsem<-cfa(model = model,data = data)

summary(medsem,standardized=T,fit.measures=T,rsquare=T)


#we see here in the summary that the p value of X incase of presence of M is not significant 

#and also see that the indirect Effect b*c is significant and the mediation effect is also significant 


#REGARDS