#this is a practice for discriminant analysis 


#loading the libraries 

library(tidyverse)
library(caret)
library(MASS)
library(mda)

#loading the iris data 

data(iris)


head(iris)

#here we will predict the type of the spider from sepal and petal width and length 

#firstly we need to scale the data 

preproc<-preProcess(iris,method = c("center","scale"))
iris<-predict(preproc,iris)

par(mfrow=c(2,2))
for(i in 1:4){
        qqnorm(iris[,i],main = paste(i))
}

#partitioning the data 

trainidx<-createDataPartition(y = iris$Species,p = .7,list = F)

traindata<-iris[trainidx,]
testdata<-iris[-trainidx,]

#using MASS function

ldafit<-lda(Species~.,data = traindata)
ldafit

#run prediction

testpred<-predict(ldafit,newdata = testdata)

testpred$class

testpred$posterior

testpred$x
plot(ldafit)


ggdata<-data.frame(traindata,predict(ldafit)$x)

ggdata%>%ggplot(aes(LD1,LD2,color=Species))+geom_point()

#testing the accuracy

mean(testdata$Species==testpred$class)

#accuracy 0.9777 %


#running quadratic DA

qdafit<-qda(Species~.,traindata)
qdafit

qdapred<-predict(qdafit,testdata)

mean(testdata$Species==qdapred$class)

#running Mixure DA

mdafit<-mda(Species~.,traindata)

mdapred<-predict(mdafit,testdata)

mean(testdata$Species==mdapred)
