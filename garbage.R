# garbage 

#Extracting the state name



#here we see that county has levels equal to N of data which will make the model misbehave 
#So, i will remove it and keep only the name of the state 

data$geography<-str_remove_all(string = data$geography,
                               pattern = "[:alpha:]{1,}(\\s)|[:alpha:]{1,}(\\,)|(\\s)")
######################################################

data<-cbind(knnImputation(data = data[,-c(9,13)]),geography=data$geography,binnedinc=data$binnedinc)%>%data.frame()

#######################################################

num<-data%>%dplyr::select(-c(binnedinc,geography))

######################################################

fit3data<-data[,c(names(coef(step))[2:20],"target_deathrate","binnedinc","geography")]
fit3<-fit3data%>%with(lm(target_deathrate~.,data=fit3data))
summary(fit3)

########################################################3

fit1data<-data[,c(names(coef(step))[2:20],"target_deathrate")]
fit1<-fit1data%>%with(lm(target_deathrate~.,data = fit1data))
summary(fit1)

fit2data<-data[,c(names(coef(step))[2:20],"target_deathrate","binnedinc")]
fit2<-fit2data%>%with(lm(target_deathrate~.,data=fit2data))
summary(fit2)



RMSE(pred = fitted(fit1),obs = data$target_deathrate)
RMSE(pred = fitted(fit2),obs = data$target_deathrate)


########################################################

#here we see that even we choosed the stepwise regresion it failed to exclude the categorical variable which since some levels are significant and other are not 
#so i will do heirarchical regression and testing between 3 models:
#1- Model of stepwize without any categorical variables
#2- Adding the median income variables 
#3- Adding the state varaibles 



#after looking at the predictors and the variables which caused high VIF
#we will notice that some of them still in the model after running stepwise regression 
#so i will try to run the model again after centering the variabes to overcome the inflation

centdata<-apply(num[,-3],2,function(x){
        scale(x,center = T,scale = F)
})

centdata<-cbind(centdata,target_deathrate=data$target_deathrate,binnedinc=data$binnedinc,geography=data$geography)%>%
        data.frame()

#running the model again and calculating the VIF again 

fitallcent<-centdata%>%with(lm(target_deathrate~.,data=centdata))
summary(fitallcent)

vif(fitallcent)

#the function throw error:' there are aliased coefficients in the model'
#this mean that there are perfect correlation between some predictors 
#so we can't use this model 

#Ok , i will remove all predictor with vif more than 10 


summary(viftest>=10)




#multicor modeification 
#run models:
##initial model with all V.
##1-without VIF
##2-stepwise
##3-Penalized
