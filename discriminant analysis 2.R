#practicing discriminant analysis 

#loading the libraries 

library(tidyverse)
library(readxl)
library(caret)
library(MASS)



#importing data 

disc<-read_excel("./data sets/Dataset_Vinayak Oak.xls")%>%data.frame()

str(disc)

disc<-disc%>%mutate(s=factor(s,labels = c(0,1)))
#testing assumptions 

#1- Normality , 2-linearity , 3- outlier

par(mfrow=c(3,4))

for(i in names(disc[,-1])){
        qqnorm(disc[,i],main = i)
        density(disc[,i])%>%plot(main=paste(i))
}


#here we see that we have a serious problem in normality

#lets test if there are any outlier 

mah<-mahalanobis(x = disc[,-1],center = colMeans(disc[,-1]),cov = cov(disc[,-1]))
cutoconfmatupper<-qchisq(.99,ncol(disc[,-1]))
cutoconfmatlower<-qchisq(.01,ncol(disc[,-1]))

which(mah>cutoconfmatupper) #only one record
which(mah<cutoconfmatlower) # no records at all 



#running diconfmaterent types of discriminant analysis with transformations 
set.seed(2662)

control=trainControl(method = "cv",number = 10,savePredictions = T)

trainidx<-createDataPartition(y = disc$s,p = .7,list = F)
traindata<-disc[trainidx,]
testdata<-disc[-trainidx,]


#linear

ldafit<-train(s~.,data=traindata,method="lda",preProcess=c("BoxCox","scale","center"),trControl=control)

ldafit

pred<-predict(ldafit,testdata)

confmat<-table(testdata$s,pred)%>%confusionMatrix()

confmat #0.9286

confmat$byClass



#quadratic
qdafit<-train(s~.,data=traindata,method="qda",preProcess=c("BoxCox","scale","center"),trControl=control)
qdafit

pred<-predict(qdafit,testdata)

confmat<-table(testdata$s,pred)%>%confusionMatrix()
confmat #71
confmat$byClass


#flexible
fdafit<-train(s~.,data=traindata,method="fda",preProcess=c("BoxCox","scale","center"),trControl=control)
fdafit

pred<-predict(fdafit,testdata)

confmat<-table(testdata$s,pred)%>%confusionMatrix()
confmat #85
confmat$byClass

#regularized


rdafit<-train(s~.,data=traindata,method="rda",preProcess=c("BoxCox","scale","center"),trControl=control)
rdafit$bestTune

pred<-predict(rdafit,testdata)

confmat<-table(testdata$s,pred)%>%confusionMatrix()
confmat #0.9286
confmat$byClass

#mixure

mdafit<-train(s~.,data=traindata,method="rda",preProcess=c("BoxCox","scale","center"),trControl=control)
mdafit

pred<-predict(mdafit,testdata)

confmat<-table(testdata$s,pred)%>%confusionMatrix()
confmat #0.9286
confmat$byClass


######
#ROC and AUC as ametrics to get the important variables 

varImp(ldafit,scale = T)%>%plot()

filterVarImp(x = traindata[,-1],y = traindata$s)%>%plot

