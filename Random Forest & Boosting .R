#random forest training 

library(tidyverse)
library(caret)

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

tgrid<-expand.grid(mtry=1:10)

#running the model

rffit<-train(Survived~.,data = traind,trControl=trControl,method="rf",
             metric="ROC",preprocess=c("center","scale"),tuneGrid=tgrid)

rffit

rffit$bestTune
rffit$finalModel

rfpred<-predict(rffit,testd)
xtab<-table(rfpred,testd$Survived)
confusionMatrix(xtab)

#variable importance

rffit%>%varImp(top=5,scale=T)%>%plot

filterVarImp(traind[,-ncol(traind)],y=traind$Survived)

###########################

#Boosting --> stochastic gradient 


trainc<-trainControl(method = "cv",number  = 10,classProbs = T,summaryFunction = twoClassSummary)


Tgrid<-expand.grid(interaction.depth = c(1, 5, 9), 
                   n.trees = (1:50), 
                   shrinkage = seq(.1,1,.1),
                   n.minobsinnode = 20)

boostfit<-train(Survived~.,data = traind,method="gbm",trControl=trainc,
                preProcess=c("center","scale"),metric="ROC",verbose = FALSE,
                tuneGrid=Tgrid)

boostfit$terms
