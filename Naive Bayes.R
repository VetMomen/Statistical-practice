
#this is a practice for naive bayes 

#loading libraries 

library(tidyverse)
library(caret)
library(readxl)

#loading the data 

bank<-read_excel("./data sets/UniversalBank.xls",sheet = 2)

View(bank)

#Correcting the names

names(bank)<-bank[2,]
bank<-bank[-c(1,2),]

#looking at the structure
str(bank)

#correcting the structure 
bank<-bank%>%mutate(ID=as.character(ID),
                    Age=as.numeric(Age),
                    Experience=as.numeric(Experience),
                    Income=as.numeric(Income),
                    `ZIP Code`=as.character(`ZIP Code`),
                    Family=factor(Family),
                    CCAvg=as.numeric(CCAvg),
                    Education=factor(Education),
                    Mortgage=as.numeric(Mortgage),
                    `Personal Loan`=factor(`Personal Loan`),
                    `Securities Account`=factor(`Securities Account`),
                    `CD Account`=factor(`CD Account`),
                    Online=factor(Online),
                    CreditCard=factor(CreditCard))

bank<-bank[,-c(1,5)]

#checking again 

str(bank)


#checking the missing variables 

apply(bank,2,function(x){
        sum(is.na(x))
})

#nomissing 

#we have to categorize the continous variables 

#and to do it i will scale these variables 

contv<-bank%>%dplyr::select(Age,Experience,Income,CCAvg,Mortgage)

contv<-apply(contv,2,function(x){
        scale(x,scale = T)
})%>%data.frame()

#looking at the max and min of each variable

summary(contv)

#the max z value is 6 and minimum is -3

#categorizing the variables

contv<-apply(contv,2,function(x){
        cut(x = x,breaks = seq(-3,6,by=1))
})%>%data.frame()

View(contv)

#replacing the old variables with the new ones 

bank<-bank%>%mutate(Age=contv$Age,
                    Experience=contv$Experience,
                    Income=contv$Income,
                    CCAvg=contv$CCAvg,
                    Mortgage=contv$Mortgage)
str(bank)


#the response variable here is wheather the person well accept personal loan or not 

bank<-bank%>%mutate(`Personal Loan`=factor(`Personal Loan`,labels = c("not accept","accept")))

#run the model 

#part the data 
set.seed(3124)
trainidx<-createDataPartition(y = bank$`Personal Loan` ,p =  .7,list = F)

traindata<-bank[trainidx,]
testdata<-bank[-trainidx,]


trcontrol<-trainControl(method = "cv",number = 10)

nbfit<-train(`Personal Loan`~.,data = traindata,method="naive_bayes",trControl=trcontrol)

testpred<-predict(nbfit,testdata)

xtab<-table(testpred,testdata$`Personal Loan`)

#using oversampling tech. to overcome impalance 

table(traindata$`Personal Loan`)

sampleaccept<-traindata%>%filter(`Personal Loan`=="accept")%>%sample_n(size = 1000,replace = T)


traindata<-rbind(traindata,sampleaccept)

#running the model again

nbfit2<-train(`Personal Loan`~.,data = traindata,method="naive_bayes",trControl=trcontrol)

testpred2<-predict(nbfit2,testdata)

xtab2<-table(testpred2,testdata$`Personal Loan`)

confusionMatrix(xtab2)

