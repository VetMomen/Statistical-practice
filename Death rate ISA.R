
#this project is about predicating the death rate per 100000 person from more than 30 predictors
#to find out more about this data and discription of variables plz visit this [page](https://data.world/exercises/linear-regression-exercise-1/workspace/file?filename=cancer_reg.csv)

#lets start with loading our libraries 

library(tidyverse)
library(knitr)
library(caret)
library(car)
library(psych)
library(mice)
library(progress)
library(DMwR)
library(readr)
library(MASS)
library(pedometrics)

#then importing the data

data<-read.csv("./data sets/cancer.csv")

head(data)

#it is important to see the structure of the data

str(data)

#some of data are factors and others are numeric 
#but there is no need for modification 

#Lets inspect the accuracy

summary(data)

#the summary reveal some issues with outliers and missing data 
#another thing , i thenk that we have to exclude the county name in keep only the name of the state

table(data$geography) #here we see that the number of county is the same of the number of data 
#this wil be problamitic in our analysis 
#So i will keep the state name and remove the county name 

data$geography<-str_remove_all(string = data$geography,
                               pattern = "[:alpha:]{1,}(\\s)|[:alpha:]{1,}(\\,)|(\\s)")

#good, now i think that we have to test for more than 5% missing value 


miss<-apply(data,2,function(x){
        round((sum(is.na(x))/length(x))*100,2)
})

miss

# here we have 3 variable with missing values 
#in my opinion , the most problematic one is the one which got 79% missings
#So i will Exclude it

data<-data%>%dplyr::select(-which(miss>70))

#Ok , lets impute the rest using KNN method

data<-cbind(knnImputation(data = data[,-c(9,13)]),binnedinc=data$binnedinc,
            geography=data$geography)%>%data.frame()


#the next step , is my data got ouliers !!
#I think using Mahalanobis will answer this question 

#first excluding the categorical variable 

num<-data%>%dplyr::select(-geography,-binnedinc)

#getting the mahalanobis value 


mah<-mahalanobis(x = num,center = colMeans(num),cov = cov(num))


#Now calculating the cutoff points 


cutoff<-qchisq(p = .99,df = ncol(num))


#lets open the surprise box :P

summary(mah>cutoff)

#now we have 356 case which considered multivariate outlier 

#lets save it for later use 

outidx<-as.numeric(mah>cutoff)

#testing additivity for multicolinearity is something crucial
#so lets test for correlation more than .9

corr<-cor(num)%>%matrix(nrow = ncol(num),ncol = ncol(num))

addit<-apply(corr,2,function(x){
        ifelse(x>=abs(.9)&x<1,paste(round(x,2),"additive",sep = " ")," ")
})

colnames(addit)<-rownames(addit)<-names(num)
corvar<-names(data)[apply(addit,2,function(x){
        str_detect(x,"additive")
})%>%apply(MARGIN = 2,any)%>%which()]

corvar

addit[corvar,corvar]%>%View


#we have here nmber of reported cancer (avganncount) and avrage reported 
#mortality ( avgdeathsperyear) and number of population is highly correlated 
#and we see that the number of population and average of reported mortality ahving no sence in predicting target
#death rate , so we will exclude them

data<-data%>%dplyr::select(-popest2015,-avgdeathsperyear)

#For the median age of males and females i will combine it together (x1+x2)/2

data<-data%>%mutate(medianagemf=(medianagemale+medianagefemale)/2)%>%dplyr::select(-medianagefemale,-medianagemale)

#For the median average of cavarage and caverage alone and emplyee cavarage i will make a linear function of them 
# = .3*(x1+x2+x3)

data<-data%>%mutate(medcov=.3*(pctprivatecoverage+pctprivatecoveragealone+pctempprivcoverage))%>%dplyr::select(-pctprivatecoverage,-pctprivatecoveragealone,-pctempprivcoverage)

#run the correlation again 

num<-data%>%dplyr::select(-geography,-binnedinc)

corr2<-cor(num)

addit2<-apply(corr2,2,function(x){
        ifelse(x>=abs(.9)&x<1,paste(round(x,2),"additive",sep = " ")," ")
})

colnames(addit2)<-rownames(addit2)<-names(num)
corvar2<-names(data)[apply(addit2,2,function(x){
        str_detect(x,"additive")
})%>%apply(MARGIN = 2,any)%>%which()]

corvar2

#Great, lets dive further in our analysis and run correlation with all variables


fitall<-data%>%with(lm(target_deathrate~.,data=data))

summary(fitall)

#Now we can run regression diagnostics

#firstly we can see the leverage points 

lev<-hatvalues(model = fitall)

#next calculating the cutoff point = (2*K+2)/N
k<-ncol(data)-1
hatcut<-((2*k)+2)/nrow(data)

#lets see how many exceed the cutoff

(lev>hatcut)%>%summary()

#we have 1951 points which exceed the cutpoint 

#lets store it in variable 

levout<-as.numeric(lev>hatcut)

#now we will test cook's D for leverage and dispercien 

cook<-cooks.distance(fitall)

#setting the cutoff point for cook's distance 

cookcut<-(4/(nrow(data)-(ncol(data)-1)-1))
(cook>cookcut)%>%summary()

#here we have 143 cases which considered as infeleuence with some misbehaving values returned to NA

cookout<-as.numeric(cook>cookcut)

#now we need to see the cases which is considered as oulier and inflence 

totalout<-outidx+levout+cookout

table(totalout)

#I think that removing points which got two issues is enough 

idx<-totalout>=2

idx<-sapply(idx,function(x){
        ifelse(is.na(x),TRUE,x)
})

table(idx)

data<-data[-idx,]


#here i think that we have to take a look on VIF for variables cause Inflation 

viftest<-vif(fitall)

viftest<-data.frame(viftest)

summary(viftest$GVIF>10)
summary(viftest$GVIF..1..2.Df..>3.5)

#i think that we face a great problem here regarding multicolinearity 
#but i will delay any action for it to the end of our analysis

#Now , lets test normality , linearity , homoscdasticity !!

stdresid<-rstudent(fitall)
stdfit<-fitted(fitall)%>%scale

plot(fitall,2)
plot(stdfit,stdresid)
abline(v = 0,h = 0)

#plotting of residual carry good news regarding normality and homoscdasticity 
#congrats !!

#here i think that i am ready to run my model and conducting the real analysis 
#but we have to not forget that we have a serious problem in colinearity
#so i will do number of models and compare between them 
#these models are:
##1- Model Contain all variables 
##2- stepwise model regarding the multicolinear variables
##3- stepwise model regarding AIC
##4- Regularised model (elastic model)
#and then comparing between RMSE and R^2

#firstly lets convert the categorical variables to dummy variables 

dummy<-dummyVars(target_deathrate~.,data=data)

data<-cbind(target_deathrate=data$target_deathrate,predict(dummy,data))%>%data.frame()

#then partitioning our data 
set.seed(234)
trainidx<-createDataPartition(data$target_deathrate,p = .7,list = F)
traindata<-data[trainidx,]
testdata<-data[-trainidx,]

#Now lets run all models consequantly and doing 10 cv fold on the training data 


fitcontrol<-trainControl(method = "cv",number = 10)

#all variables model 

allfit<-train(target_deathrate ~.,data = traindata,method="lm",trControl=fitcontrol)

summary(allfit)

#ok , the factor predictor of states is misbehave and rarly be significant
# our data matrix is at least equal to the number of parameters we want to fit. One way to invoke it is having some collinear covariates which exist in our data

#Now lets test stepwise regression 

dtepfit<-train(target_deathrate ~.,data = traindata,method="lmStepAIC",trControl=fitcontrol)

summary(dtepfit)

#Now the final model is penalized model 
#lets run elastic model 

elasticfit<-train(target_deathrate~.,data = traindata,trControl=fitcontrol,tuneLength=10,method="glmnet")

elasticfit$bestTune # best alpha and lambda values 

coef(elasticfit$finalModel,elasticfit$bestTune$lambda)

#finally we need to predict the test data to Extract RMSE and R2

allfitpred<-predict(allfit,testdata)
allfitmeasures<-data.frame(RMSE=RMSE(pred = allfitpred,obs = testdata$target_deathrate),R2=R2(pred = allfitpred,obs = testdata$target_deathrate))

dtepfitpred<-predict(dtepfit,testdata)
dtepfitmeasures<-data.frame(RMSE=RMSE(pred = dtepfitpred,obs = testdata$target_deathrate),R2=R2(pred = dtepfitpred,obs = testdata$target_deathrate))

elasticfitpred<-predict(elasticfit,testdata)
elasticfitmeasures<-data.frame(RMSE=RMSE(pred = elasticfitpred,obs = testdata$target_deathrate),R2=R2(pred = elasticfitpred,obs = testdata$target_deathrate))


#putting all measures together

allmeasures<-rbind(allfitmeasures,dtepfitmeasures,elasticfitmeasures)

rownames(allmeasures)<-c("Model with all variables","stepwise model","elastic penalized model")

#at last 
#we see here that the penalized model is the most accurate model 
#in this model we reduced the coefficients of some model to be near zero and others to be zero (combination between lasso and ridge regression)
#the best alpha value is .2 and lambda value is .407

#this was an effort to acheive the best prediction of death rate using more than 30 variables 

#Regards 