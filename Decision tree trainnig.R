
#this code for practicing KNN

#loading libraries

library(tidyverse)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)

#importing the data 

loan<-read.csv("./data sets/loan_data.csv")
loan<-loan[,-1]%>%data.frame()
View(loan)

str(loan)

loan<-loan%>%mutate(delinq_2yrs_zero=factor(delinq_2yrs_zero),
                    pub_rec_zero=factor(pub_rec_zero),
                    open_acc=as.numeric(open_acc),
                    revol_bal=as.numeric(revol_bal),
                    emp_len_=as.numeric(emp_len_),
                    outcome=factor(outcome,labels = c("default","paid")))
#creating one hot dummy

loandum<-model.matrix(outcome~.-1,data = loan)%>%data.frame()%>%mutate(outcome=loan$outcome)

#creating training and test data sets 

#creating training and test data sets 
set.seed(345)
idx<-createDataPartition(loandum$outcome,p = .75,list = F)

traindata<-loandum[idx,]
testdata<-loandum[-idx,]

#specifying the training control

trControl=trainControl(method = "cv",number = 10,classProbs = T,summaryFunction = twoClassSummary)

#running the model

treefit<-train(outcome~.,data = traindata,method="rpart",metric="ROC",trControl=trControl)

treepred<-predict(treefit,testdata,type = "prob")

predeval<-prediction(treepred$paid,labels = testdata$outcome)

performance(predeval,"auc")

performance(predeval,"tpr","fpr")%>%plot

#########################################################

#titanic data

library(mice)

#loading the train data 

traindata<-read.csv("./data sets/titanic train.csv")%>%data.frame()

View(traindata)

#checking the class of every variables

str(traindata)

#class modification

traindata<-traindata%>%mutate(Survived=factor(Survived,labels = c("not","survived")),
                              Pclass=factor(Pclass),
                              Name=as.character(Name),
                              Ticket=as.character(Ticket),
                              Cabin=factor(Cabin))

#lets look to the data and extract some features 

#combining sibSp and parch in one column which is family size on the ship 

traindata<-traindata%>%mutate(familysize=SibSp+Parch)%>%dplyr::select(-c(SibSp,Parch))

#Extracting the title of each person 

traindata<-traindata%>%mutate(title=str_extract(Name,pattern = "[:alpha:]{1,}(?=\\.)"))%>%
        dplyr::select(-Name)


#calculating fare per member after calculating the family size 

traindata<-traindata%>%
        mutate(fare_per_person=ifelse(familysize==0,Fare,Fare/(familysize+1)))%>%
        dplyr::select(-Fare)

#need to reassign the lables of cabin 
traindata<-traindata%>%
        mutate(Cabin=ifelse(str_detect(Cabin,pattern = "[:alpha:]"),Cabin,NA))%>%
        mutate(Cabin=ifelse(!is.na(Cabin),Cabin,"unknown"))

#checking the missing and accuracy 

summary(traindata)

apply(traindata,2,function(x){
        (sum(is.na(x))/length(x))*100
})



#i want to impute the missing values in ages 

#but firstly i want to remove the variables with unique value for each record 

dataimpute<-traindata%>%dplyr::select(-c(PassengerId,Ticket,Cabin))
imp<-mice(dataimpute)

completedata<-mice::complete(imp)

traindata<-cbind(completedata)%>%data.frame()

str(traindata)

#dummy convert

traindum<-model.matrix(Survived~.-1,traindata)%>%data.frame()%>%mutate(Survived=traindata$Survived)

#removing zerovariance

zero<-nearZeroVar(traindum)

traindum<-traindum[,-zero]

#partitioning
set.seed(76765)
idx<-createDataPartition(traindum$Survived,p = .75,list = F)

traind<-traindum[idx,]
testd<-traindum[-idx,]

#specifying the training control

trControl=trainControl(method = "cv",number = 10,classProbs = T,summaryFunction = twoClassSummary)

#running the model



treefit<-train(Survived~.,data = traind,method="rpart",
               metric="ROC",trControl=trControl,tuneGrid=data.frame(cp=.00001))

treefit

treefit$bestTune

cptable<-treefit$finalModel$cptable

cpbest<-cptable%>%filter(rel.error==min(cptable$rel.error))%>%select(CP)

brunedtree<-prune(tree = treefit$finalModel,cp = 1e-8)

treepred<-predict(brunedtree,testd,type = "prob")



predeval<-prediction(treepred[,2],labels = testd$Survived)

performance(predeval,"auc")@y.values[[1]]

performance(predeval,"tpr","fpr")%>%plot


boxcols <- c("green", "red")[treefit$finalModel$y]


prp(treefit$finalModel,type = 3,box.col = boxcols)

legend(x = .02,y = .88,legend = c("Survived","not"),fill = c("green","red"),xjust = .001,yjust = .001,cex = .5)


