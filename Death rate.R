

# this project from [data.world](https://data.world/exercises/linear-regression-exercise-1/workspace/file?filename=avg-household-size.csv)

# and the aim of this data to use multivariate OLS method to pridect the death rate 

#First lets load our libraries 

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
#download the data


#loading it 

data<-read.csv("./data sets/cancer.csv")

head(data)

#now lets look to the structure of the data 

str(data)

#here we have to note that the out come is count : count of death rate per year 
# so we have to use poisson regression instead of OLS regression 
#but for tutorial reason , I will use OLS instead of posson , then i will use poisson 

#Now lets look at the at the accuracy of the data , and if there any modification need to be acheived 

summary(data)

#some variables need inspection like;:
#avganncount
#avgdeathsperyear
#incidencerate
#studypercap
#popest2015
#medincome
#binnedinc
#medianage
#geography
#pctsomecol18_24
#pctbachdeg18_24
#pctemployed16_over
#pctprivatecoveragealone
#pctasian
#pctblack
#pctotherrace
#birthrate



#converting the factor variable to dummey numbers 
table(data$binnedinc)
data<-data%>%mutate(binnedinc=factor(binnedinc))

table(data$geography)

#removing the states 
data<-data%>%dplyr::select(-geography)
#Most of these variables has issue with outlier ( which may also indicate skewness) and large number of missing data 

#So, lets first test the outlier percent and if we can impute it 
#the outlier per variables must not Exceed 5%

miss<-apply(data,2,function(x){
        round((sum(is.na(x))/length(x))*100,2)
})

miss

#here we have issue with some variables which has a missing variables >5% like:

#pctsomecol18_24 = 74.99%
#pctprivatecoveragealone = 20%

#here i am going to Exclude the variable which with more that 70% missing value 


data<-data%>%dplyr::select(-which(miss>70))

#summary again and outlier checking again 

apply(data,2,function(x){
        round((sum(is.na(x))/length(x))*100,2)
})

#Now i will impute hte missing data using KNN method


data<-cbind(knnImputation(data = data[,-c(9)]),binnedinc=data$binnedinc)%>%data.frame()

#now lets test the missing again 

apply(data,2,function(x){
        round((sum(is.na(x))/length(x))*100,2)
})

#great 

# the next station is checking for outliers
#and the best way is to check for multivariate outlier using mahalanobis test


#choosing the numeric variables 

num<-data%>%dplyr::select(-c(binnedinc))

#getting the mahalanobis value 


mah<-mahalanobis(x = num,center = colMeans(num),cov = cov(num))


#Now calculating the cutoff points 


cutoff<-qchisq(p = .99,df = ncol(num))


#lets open the surprise box :P

summary(mah>cutoff)

#now we have 342 case which considered multivariate outlier 

#lets save it for later use 

outidx<-as.numeric(mah>cutoff)

#testing additivity is the next step

corr<-cor(num)%>%matrix(nrow = ncol(num),ncol = ncol(num))

colnames(corr)<-rownames(corr)<-names(num)
View(corr)

#lets see if there are any correlation above .9

addit<-apply(corr,2,function(x){
        ifelse(x>=abs(.9)&x<1,paste(round(x,2),"additive",sep = " ")," ")
})

colnames(addit)<-rownames(addit)<-names(num)

View(addit)


corvar<-names(data)[apply(addit,2,function(x){
        str_detect(x,"additive")
})%>%apply(MARGIN = 2,any)%>%which()]

corvar
#thesa are the variables which have multicolinearity
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
num<-data%>%dplyr::select(-c(binnedinc))
corr<-cor(num)%>%matrix(nrow = ncol(num),ncol = ncol(num))

colnames(corr)<-rownames(corr)<-names(num)
View(corr)

addit<-apply(corr,2,function(x){
        ifelse(x>=abs(.9)&x<1,paste(round(x,2),"additive",sep = " ")," ")
})

colnames(addit)<-rownames(addit)<-names(num)

View(addit)



#Now it time to dive more further and run the model against all variables for duagnostics

fitall<-data%>%with(lm(target_deathrate~.,data=data))

summary(fitall)

#first of all we will have a look on the infeluence and leverage cases

lev<-hatvalues(model = fitall)

#next calculating the cutoff point = (2*K+2)/N
k<-ncol(data)-1
hatcut<-((2*k)+2)/nrow(data)

#lets see how many exceed the cutoff

(lev>hatcut)%>%summary()

#we have 326 points which exceed the cutpoint 

#lets store it in variable 

levout<-as.numeric(lev>hatcut)

#now we will test cook's D for leverage and dispercien 

cook<-cooks.distance(fitall)

#setting the cutoff point for cook's distance 

cookcut<-(4/(nrow(data)-(ncol(data)-1)-1))
(cook>cookcut)%>%summary()

#here we have 209 cases which considered as infeleuence 

cookout<-as.numeric(cook>cookcut)


#now we need to see the cases which is considered as oulier and inflence 

totalout<-outidx+levout+cookout

table(totalout)

#we have 214 points which have two criteria and i will delet it 

idx<-totalout>=1

table(idx)

data<-data[-idx,]

#great , bu don't forget that we have variables which cause multicolinearity 
#and to deal with those we need to have a look at variance inflation factor which 
#need to be less than 10 

viftest<-vif(fitall)
viftest
summary(viftest>10)

#here we have 6 variables which exceed the cutoff point 10 

#now its time to test normality ,linearity , homoscdasticity 

stdresid<-rstudent(fitall)
stdfit<-fitted(fitall)%>%scale

par(mfrow=c(1,1))

plot(fitall,2)
plot(stdfit,stdresid)
abline(v = 0,h = 0)


#good , it sounds good , linear from 2 to -2 , homogeneic residual with abscence of heteroscdasticity

#The next step is to decide which variable to keep and which to remove 
#this can be done through hirarchical regression and comparing between variables regarding :
#1- change in R^2
#2- Standardized coefficeint 
#3- partial correlation squared 
#4- significant p.value of t test of coefficients

#the comparison done in many ways --> backword , forword , stepwise

#and for easiness i will use stepwise regression to save time and choose the best variable predict the outcome
#but you have to note that stepwise is not preferable unless you don't have a hypothesis to test or theory to follow
genl<-data%>%with(glm(target_deathrate~.,data=data))
step<-stepAIC(genl)
summary(step)

vif(step)
#variables included in the stepwise final 

names(coef(step))

#Ok , we can see here that we succeed in determining 
#1- Outliers
#2- Influence points
#3- MultiColimearity 

#and our data is homogeneic and homoscdasticiv

#when it come to inflation i will compare all over inflated model one by one

#craeting train data 

trainidx<-createDataPartition(y = data$target_deathrate,p = .7,list = F)

traindata<-data[trainidx,]
testdata<-data[-trainidx,]

fitcontrol<-trainControl(method = "repeatedcv",number = 10,repeats = 10)
fit1<-train(target_deathrate~.,data = traindata,trControl = fitcontrol,method = "lm")

summary(fit1)
testpred<-predict(fit1,testdata)
RMSE(pred = testpred,obs = testdata$target_deathrate)

fit2<-train(target_deathrate~.,data = traindata[,-c(4)],trControl = fitcontrol,method = "lm")

summary(fit2)
testpred2<-predict(fit2,testdata)
RMSE(pred = testpred2,obs = testdata$target_deathrate)

fit3<-train(target_deathrate~.,data = traindata[,-c(4,5)],trControl = fitcontrol,method = "lm")

summary(fit3)
testpred3<-predict(fit3,testdata)
RMSE(pred = testpred3,obs = testdata$target_deathrate)


fit4<-train(target_deathrate~.,data = traindata[,-c(4,5,8)],trControl = fitcontrol,method = "lm")

summary(fit4)
testpred4<-predict(fit4,testdata)
RMSE(pred = testpred4,obs = testdata$target_deathrate)

fit5<-train(target_deathrate~.,data = traindata[,-c(4,5,16)],trControl = fitcontrol,method = "lm")

summary(fit5)
testpred5<-predict(fit5,testdata)
RMSE(pred = testpred5,obs = testdata$target_deathrate)


fit6<-train(target_deathrate~.,data = traindata[,-c(4,5,16,17)],trControl = fitcontrol,method = "lm")

summary(fit6)
testpred6<-predict(fit6,testdata)
RMSE(pred = testpred6,obs = testdata$target_deathrate)

fit7<-train(target_deathrate~.,data = traindata[,-c(4,5,16,22)],trControl = fitcontrol,method = "lm")

summary(fit7)
testpred7<-predict(fit7,testdata)
RMSE(pred = testpred7,obs = testdata$target_deathrate)

fit8<-train(target_deathrate~.,data = traindata[,-c(4,5,16,24)],trControl = fitcontrol,method = "lm")

summary(fit8)
testpred8<-predict(fit8,testdata)
RMSE(pred = testpred8,obs = testdata$target_deathrate)
