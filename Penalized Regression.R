library(caret)
library(tidyverse)
library(MASS)

data(Boston) #importing Boston Data 


set.seed(859726) #setting Seed


trainidx<-createDataPartition(y = Boston$medv,p = .8,list = F) #choosing training IDX


traindata<-Boston[trainidx,] # the training data


testdata<-Boston[-trainidx,] # the test data 


x<-model.matrix(medv~.,traindata)[,-1] #Convering all qualitative variables to dummies and removing the intercept

y<-traindata$medv # the predicted variable 

#chossing the lowest lamda value -- > produce the lowest prediction error in cross validation sample 

lam<-cv.glmnet(x = x,y = y,alpha=0) # 0 here for ridge method 

lamda.mini<-lam$lambda.min

lamda.mini # Lamda which minimize the prediction error in CV sample 

model<-glmnet(x = x,y = y,alpha = 0,lambda = lamda.mini) #Building the ridge regression 

coef(model)

x.test<-model.matrix(medv~.,testdata)[,-1] #converting test qualitatives to dummies 

fitdata<-predict(model,x.test) #predicting using CV data set

error<-data.frame(RMSE=RMSE(fitdata,testdata$medv),
                  R2<-R2(fitdata,testdata$medv)) #dataframe of RMSE and R^2

error


#Now Calculating lasso regression by setting alpha to 1 instead of 0

lam2<-cv.glmnet(x = x,y = y,alpha=1)#choosing the lowest lamda

lamda2.mini<-lam2$lambda.min

model.lasso<-glmnet(x = x,y = y,alpha = 1,lambda = lamda2.mini) #running the model

coef(model.lasso) #Model coefficents 

fit.lasso<-predict(model.lasso,newx = x.test)

error.lasso<-data.frame(RMSE=RMSE(fit.lasso,obs = testdata$medv),
                        R2<-R2(fit.lasso,testdata$medv))

error.lasso

#Now Clculating the elastic net regression
# we Will use Caret to Identify the best value for Aplha and lamda 
# as alpha is a value between 0-1
#no need here to use model.matrix() function 


model.elastic<-train(medv~.,data = traindata,method="glmnet",
                     trControl=trainControl(method = "cv",number = 10),
                     tuneLength=10) # iteration 10 times

model.elastic$bestTune #best alpha and lamda 


coef(model.elastic$finalModel,model.elastic$bestTune$lambda) # the Coeficient 

predict.elastic<-predict(model.elastic,x.test) #predicting the test data set

error.elastic<-data.frame(RMSE=RMSE(predict.elastic,obs = testdata$medv),
                          R2=R2(predict.elastic,obs = testdata$medv))


error.elastic


#Now lets running lasso and ridge using caret package 


set.seed(635)

# 1- Ridge 

#building a lamda grid

lamda<-10^seq(-3,3,length=100)

fit.ridge<-train(medv~.,traindata,method="glmnet",
                 trControl=trainControl(method = "cv",number = 10),
                 tuneGrid=expand.grid(alpha=0,lambda=lamda)) #running with different value of lambda

#Coefficients 

coef(fit.ridge$finalModel,fit.ridge$bestTune$lambda)

#predicting

pred.ridge<-predict(fit.ridge,x.test)

error.ridge<-data.frame(RMSE=RMSE(pred = pred.ridge,obs = testdata$medv),
                        R2=R2(pred = pred.ridge,obs = testdata$medv))

error.ridge

# using caret for lasso 

fit.lasso<-train(medv~.,traindata,method="glmnet",
                 trControl=trainControl(method = "cv",number = 10),
                 tuneGrid=expand.grid(alpha=1,lambda=lamda))

#coef

coef(fit.lasso$finalModel,fit.lasso$bestTune$lambda)


pred.lasso<-predict(fit.lasso,x.test)

error.lasso<-data.frame(RMSE=RMSE(pred.lasso,testdata$medv),
                        R2=R2(pred.lasso,testdata$medv))

error.lasso

allerror<-rbind(error.ridge,error.lasso,error.elastic)

rownames(allerror)<-c("Ridge","Lasso","Elastic")

allerror
