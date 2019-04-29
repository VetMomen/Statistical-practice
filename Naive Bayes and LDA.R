#this code for practicing naive bayes and LDA

#loading libraries

library(tidyverse)
library(caret)
library(MASS)

#importing the data 

loan<-read.csv("./data sets/loan_data.csv")
loan<-loan[,-1]
View(loan)

#knowing the unique values of each variable

apply(loan,2,unique)
str(loan)

#we can convert the continous variables to categorical but i will use the catigorica variables only 

loanf<-loan%>%select(status,term,purpose,home_ownership,outcome,purpose_,home_,emp_len_)

#then i wan to pationtion my data into training and testing data sets 

trainidx<-createDataPartition(y = loan$outcome,p = .7,list = F)

traindata<-loan[trainidx,]
testdata<-loan[-trainidx,]


#trainning control

trcontrol=trainControl(method = "cv",number = 10)


#running the model

nbfit<-train(outcome~.,data = traindata,method="naive_bayes",trControl=trcontrol)
nbfit$results

#prediction

predtest<-predict(nbfit,testdata)

xtab<-table(predtest,testdata$outcome)

confusionMatrix(xtab)


#now i will run discriminant analysis


#taking the numaeric data 

loanN<-loan%>%select(outcome,loan_amnt,annual_inc,dti,payment_inc_ratio,revol_bal,revol_util,delinq_2yrs_zero,pub_rec_zero,open_acc,grade,emp_length,borrower_score)

set.seed(980)
trainidx<-createDataPartition(y = loanN$outcome,p = .7,list = F)

traindata<-loanN[trainidx,]
testdata<-loanN[-trainidx,]


#trainning control

trcontrol=trainControl(method = "cv",number = 10)


#running the model

ldafit<-train(outcome~.,data=traindata,method="lda",trControl=trcontrol)
ldafit$finalModel


xtab<-table(predtest,testdata$outcome)

confusionMatrix(xtab)


