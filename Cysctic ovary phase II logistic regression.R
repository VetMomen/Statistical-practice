#This data is to predicted PCOS in women 

#loading lobraries

library(tidyverse);library(mice);library(car);library(pROC);library(MASS)

#reading the directory

dir<-"/home/debian/Statistical-practice/data sets"

dat1<-read_csv(file = paste(dir,"PCOS_infertility.csv",sep = "/"))
dat2<-read_csv(file = paste(dir,"data without infertility _final.csv",sep = "/"))

#merging the data 

dat<-left_join(dat1,dat2)

#looking at the structure

str(dat) 

#see summary for typos and wrong entry

summary(dat)

#some variables class needs to be modified
dat<-dat%>%mutate(`PCOS (Y/N)`=factor(`PCOS (Y/N)`,levels = c(0,1)),`AMH(ng/mL)`=as.numeric(`AMH(ng/mL)`),`Blood Group`=factor(`Blood Group`),
                  `Cycle(R/I)`=factor(`Cycle(R/I)`),`Pregnant(Y/N)`=factor(`Pregnant(Y/N)`),
                  `Weight gain(Y/N)`=factor(`Weight gain(Y/N)`),
                  `hair growth(Y/N)`=factor(`hair growth(Y/N)`),
                  `Hair loss(Y/N)`=factor(`Hair loss(Y/N)`),
                  `Skin darkening (Y/N)`=factor(`Skin darkening (Y/N)`),
                  `Pimples(Y/N)`=factor(`Pimples(Y/N)`),
                  `Fast food (Y/N)`=factor(`Fast food (Y/N)`),
                  `Reg.Exercise(Y/N)`=factor(`Reg.Exercise(Y/N)`),
                  `Sl. No`=as.character(`Sl. No`),`Patient File No.`=as.character(`Patient File No.`)
)

#look at summary and str again
str(dat)
summary(dat)

#i will exclude a single factor level in cycle R/I as it may cause a problem in the future

dat$`Cycle(R/I)`<-droplevels(dat$`Cycle(R/I)`,exclude = factor(dat$`Cycle(R/I)`,levels = 5))

#lookin at NAs and see if it cause a problems

napattern<-sapply(dat,function(x){
        return(list(count=sum(is.na(x)),
                    percent=sum(is.na(x))/length(x)))
})

print(napattern)


#a single NA is not a big deal , but i will remove X73 , it is a problematic
dat<-dat[,-ncol(dat)]


#the extended names will cause a trouble in the future , so i will reduce it 

nam<-names(dat)
nam<-str_remove_all(string = nam,pattern = "[:blank:]|[:punct:]")

names(dat)<-nam


#after inspecting the model we find some variables which you have to take care about it like:
#BMI -> a function of length and weight
#Waist/Hip ratio 
# i will remove the original variables

data<-dat%>%select(-c("Hipinch","Waistinch","WeightKg","HeightCm"))


#Another issue , Tha max value in some variables seems to be problematic
#do box plot and inspect them

par(mfrow=c(3,7))
boxplot(data$IbetaHCGmIUmL,main="IbetaHCGmIUmL") #contains problematic values
boxplot(data$IIbetaHCGmIUmL,main="IIbetaHCGmIUmL") # the same
boxplot(data$AMHngmL,main="AMHngmL")# the same
boxplot(data$FSHmIUmL,main="FSHmIUmL")# the same
boxplot(data$LHmIUmL,main="LHmIUmL")# the same
boxplot(data$FSHLH,main="FSHLH")# the same
boxplot(data$TSHmIUL,main="TSHmIUL")# the same
boxplot(data$PRLngmL,main="PRLngmL")# the same
boxplot(data$VitD3ngmL,main="VitD3ngmL")# the same
boxplot(data$PRGngmL,main="PRGngmL")# the same
boxplot(data$FollicleNoL,main="FollicleNoL")# the same
boxplot(data$FollicleNoR,main="FollicleNoR")#not bad

#determining the outliers
#extracting the numeric variables
num<-data[,c(sapply(data,is.numeric))]

limits<-function(x){
        q<-quantile(x,na.rm = T)%>%data.frame()
        q1<-q[2,]
        q2<-q[4,]
        IQR<-IQR(x,na.rm = T)
        upper<-q2+(1.5*IQR(x,na.rm = T))
        lower<-q1-(1.5*IQR(x,na.rm = T))
        return(list(q1=q1,q2=q2,IQR=IQR,upper=upper,lower=lower))
}

limitvalues<-sapply(num,limits)%>%data.frame()

print(limitvalues)

#determining the outlier index

uniout<-list()
for(i in 1:ncol(limitvalues)){
        uniout[[i]]<-which(num[,i]>limitvalues[4,i]|num[,i]<limitvalues[5,i])
}

names(uniout)<-names(num)

print(uniout)

#lets look to the percent of outliers per variable and in the whole data

sapply(uniout,function(x){
        round(length(x)/nrow(num),digits = 2)
}) #we have some variables its outliers reached to 17%


round(length((unlist(uniout)%>%unique()))/nrow(num),2) # OMG i can't delete the outlier and make imputation with 77% of data are NAs

#Try another thing , looking at the outliers after rounding the variables by binary cyctic ovary

num2<-data.frame(factor(dat$PCOSYN),num)
splitted<-split(x = num2,f = num2$factor.dat.PCOSYN.)

#determining outliers again

zerolimit<-sapply(splitted[["0"]][,-1],limits)
onelimit<-sapply(splitted[["1"]][,-1],limits)


Zuniout<-list()
for(i in 1:ncol(splitted[["0"]][,-1])){
        Zuniout[[i]]<-which(splitted[["0"]][,-1][,i]>zerolimit[4,i]|splitted[["0"]][,-1][,i]<zerolimit[5,i])
}

print(Zuniout)

Ouniout<-list()
for(i in 1:ncol(splitted[["1"]][,-1])){
        Ouniout[[i]]<-which(splitted[["1"]][,-1][,i]>onelimit[4,i]|splitted[["1"]][,-1][,i]<onelimit[5,i])
}

print(Ouniout)

#here we face the same problem , and we can't delete all of these values at once 
#So, lets try Multivariate outliers
mah<-mahalanobis(x = num,center = colMeans(num,na.rm = T),cov = cov(num,use="pairwise.complete.obs"))
cut<-qchisq(p = .95,df = ncol(num))

outliers<-which(mah>cut) #keep this in your mind for later inspection
length(outliers) #75 value are outlier 

data<-data[-outliers,] #removing them from the data 

####################
#fitting the first model
set.seed(75093)
idx<-sample(1:nrow(data),size = .75*nrow(data),replace = F)

train<-data[idx,]%>%na.omit()

model<-c()

name<-names(data[,-c(1,2,3)])

for( i in 1:length(name)){
        if(i == 1){model<<-name[i]}
        else{model<<-paste(model,name[i],sep = "+")}
}

formula<-paste("PCOSYN",model,sep = "~")


#Do intial model 

fit0<-train%>%glm(formula = formula,family = "binomial")
summary(fit0)

#looking at residual outliers 
rz<-function(model,y){
        phat<-as.numeric(predict(object = model,type="response"))
        (y-phat)/(sqrt(phat*(1-phat)))
}


ri<-rz(model = fit0,y = (as.numeric(train$PCOSYN)-1))

residout<-which(abs(ri)>=2.5)


train2<-train[-residout,]%>%na.omit()#removing outliers

fit1<-glm(formula = formula,family = "binomial",data = train2)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1) #we have a problem in normality 
#Now i will plot residual against each of predictors to see the one which causes the problem
sapply(train2[,-c(1,2,3)],function(x){
        plot(x=x,y=rstudent(fit1),main = paste0(colnames(x)))
}) # here we can see that the relationship between each predictor and y is linear and the only problem is the presence of univariate outliers 




#Now i will evaluate the predictive power of the model

test<-data[-idx,]

#Build a function for evaluation
evaluation<-function(fit,testdata,y){
        fit1_pred<-predict.glm(fit,newdata = testdata,type = "response")
                        
                        
                        auc<-roc(y,fit1_pred)
                        
                        plotauc<-plot(auc)
                        
                        confdat<-cbind(auc$sensitivities,(1-auc$specificities),auc$thresholds)%>%data.frame()
                        
                        bestthreshold<-confdat%>%filter(X1==max(X1))%>%filter(X2==min(X2))
                        bestthreshold2<-c(bestthreshold[,3])
                        
                        predicted<-ifelse(fit1_pred>bestthreshold$X3,1,0)
                        
                        confm<-table(test$PCOSYN,predicted)%>%prop.table()
                        
                        hit_rate<-as.matrix(confm)[1,1]+as.matrix(confm)[2,2]
                        return(list(
                                auc=plotauc,
                                choosed_threshold=bestthreshold2,
                                confusion_matrix=confm,
                                hit_rate=hit_rate
                        ))
                }

fit1_e<-evaluation(fit = fit1,testdata = test,y = test$PCOSYN)
print(fit1_e)

#Now i will remove the irrelevant variables in step-back manner and see its effect on the model accuracy


#removing highest p-value
dropping<-function(fit){
        drop<-names(summary(fit)$coefficients[,4]%>%which.max())%>%str_remove_all(pattern = "[:digit:](?![:alpha:])")
        train2<<-train2%>%select(-drop)
        name<-names(train2[,-c(1,2,3)])
        ss<-paste(list(name),sep = "+")
        ss2<-str_remove_all(string = ss,pattern = "[:punct:]")
        ss2<-str_remove(string = ss2,pattern = "c")
        model<-str_replace_all(string = ss2,pattern = "\\s(?=[:alpha:])",replacement = "+")
        formula<-paste("PCOSYN",model,sep = "~")
        return(list(formula=formula,drop=drop))
}

#dropping from fit1_
drp2<-dropping(fit = fit1)
print(drp2)

fit2<-train2%>%glm(formula = drp2$formula,family = "binomial")
summary(fit2)

fit2_e<-evaluation(fit = fit2,testdata = test,y = test$PCOSYN)


#dropping from fit2
drp3<-dropping(fit = fit2)
print(drp3)

fit3<-train2%>%glm(formula = drp3$formula,family = "binomial")
summary(fit3)

fit3_e<-evaluation(fit = fit3,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp4<-dropping(fit = fit3)
print(drp4)

fit4<-train2%>%glm(formula = drp4$formula,family = "binomial")
summary(fit4)

fit4_e<-evaluation(fit = fit4,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp5<-dropping(fit = fit4)
print(drp5)

fit5<-train2%>%glm(formula = drp5$formula,family = "binomial")
summary(fit5)

fit5_e<-evaluation(fit = fit5,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp6<-dropping(fit = fit5)
print(drp6)

fit6<-train2%>%glm(formula = drp6$formula,family = "binomial")
summary(fit6)

fit6_e<-evaluation(fit = fit6,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp7<-dropping(fit = fit6)
print(drp7)

fit7<-train2%>%glm(formula = drp7$formula,family = "binomial")
summary(fit7)

fit7_e<-evaluation(fit = fit7,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp8<-dropping(fit = fit7)
print(drp8)

fit8<-train2%>%glm(formula = drp8$formula,family = "binomial")
summary(fit8)

fit8_e<-evaluation(fit = fit8,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp9<-dropping(fit = fit8)
print(drp9)

fit9<-train2%>%glm(formula = drp9$formula,family = "binomial")
summary(fit9)

fit9_e<-evaluation(fit = fit9,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp10<-dropping(fit = fit9)
print(drp10)

fit10<-train2%>%glm(formula = drp10$formula,family = "binomial")
summary(fit10)

fit10_e<-evaluation(fit = fit10,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp11<-dropping(fit = fit10)
print(drp11)

fit11<-train2%>%glm(formula = drp11$formula,family = "binomial")
summary(fit11)

fit11_e<-evaluation(fit = fit11,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp12<-dropping(fit = fit11)
print(drp12)

fit12<-train2%>%glm(formula = drp12$formula,family = "binomial")
summary(fit12)

fit12_e<-evaluation(fit = fit12,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp13<-dropping(fit = fit12)
print(drp13)

fit13<-train2%>%glm(formula = drp13$formula,family = "binomial")
summary(fit13)

fit13_e<-evaluation(fit = fit13,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp14<-dropping(fit = fit13)
print(drp14)

fit14<-train2%>%glm(formula = drp14$formula,family = "binomial")
summary(fit14)

fit14_e<-evaluation(fit = fit14,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp15<-dropping(fit = fit14)
print(drp15)

fit15<-train2%>%glm(formula = drp15$formula,family = "binomial")
summary(fit15)

fit15_e<-evaluation(fit = fit15,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp16<-dropping(fit = fit15)
print(drp16)

fit16<-train2%>%glm(formula = drp16$formula,family = "binomial")
summary(fit16)

fit16_e<-evaluation(fit = fit16,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp17<-dropping(fit = fit16)
print(drp17)

fit17<-train2%>%glm(formula = drp17$formula,family = "binomial")
summary(fit17)

fit17_e<-evaluation(fit = fit17,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp18<-dropping(fit = fit17)
print(drp18)

fit18<-train2%>%glm(formula = drp18$formula,family = "binomial")
summary(fit18)

fit18_e<-evaluation(fit = fit18,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp19<-dropping(fit = fit18)
print(drp19)

fit19<-train2%>%glm(formula = drp19$formula,family = "binomial")
summary(fit19)

fit19_e<-evaluation(fit = fit19,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp20<-dropping(fit = fit19)
print(drp20)

fit20<-train2%>%glm(formula = drp20$formula,family = "binomial")
summary(fit20)

fit20_e<-evaluation(fit = fit20,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp21<-dropping(fit = fit20)
print(drp21)

fit21<-train2%>%glm(formula = drp21$formula,family = "binomial")
summary(fit21)

fit21_e<-evaluation(fit = fit21,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp22<-dropping(fit = fit21)
print(drp22)

fit22<-train2%>%glm(formula = drp22$formula,family = "binomial")
summary(fit22)

fit22_e<-evaluation(fit = fit22,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp23<-dropping(fit = fit22)
print(drp23)

fit23<-train2%>%glm(formula = drp23$formula,family = "binomial")
summary(fit23)

fit23_e<-evaluation(fit = fit23,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp24<-dropping(fit = fit23)
print(drp24)

fit24<-train2%>%glm(formula = drp24$formula,family = "binomial")
summary(fit24)

fit24_e<-evaluation(fit = fit24,testdata = test,y = test$PCOSYN)




#dropping from fit3
drp25<-dropping(fit = fit24)
print(drp25)

fit25<-train2%>%glm(formula = drp25$formula,family = "binomial")
summary(fit25)

fit25_e<-evaluation(fit = fit25,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp26<-dropping(fit = fit25)
print(drp26)

fit26<-train2%>%glm(formula = drp26$formula,family = "binomial")
summary(fit26)

fit26_e<-evaluation(fit = fit26,testdata = test,y = test$PCOSYN)




#dropping from fit3
drp27<-dropping(fit = fit26)
print(drp27)

fit27<-train2%>%glm(formula = drp27$formula,family = "binomial")
summary(fit27)

fit27_e<-evaluation(fit = fit27,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp28<-dropping(fit = fit27)
print(drp28)

fit28<-train2%>%glm(formula = drp28$formula,family = "binomial")
summary(fit28)

fit28_e<-evaluation(fit = fit28,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp29<-dropping(fit = fit28)
print(drp29)

fit29<-train2%>%glm(formula = drp29$formula,family = "binomial")
summary(fit29)

fit29_e<-evaluation(fit = fit29,testdata = test,y = test$PCOSYN)



#dropping from fit3
drp30<-dropping(fit = fit29)
print(drp30)

fit30<-train2%>%glm(formula = drp30$formula,family = "binomial")
summary(fit30)

fit30_e<-evaluation(fit = fit30,testdata = test,y = test$PCOSYN)


#dropping from fit3
drp31<-dropping(fit = fit30)
print(drp31)

fit31<-train2%>%glm(formula = drp31$formula,family = "binomial")
summary(fit31)

fit31_e<-evaluation(fit = fit31,testdata = test,y = test$PCOSYN)

#Here we excluded all variables which is not significant in term of t-test

#Now i will arrange all important parameters in one table to ease comparison

#this table consists of 1-fit number 2-AIC 3-auc 7-hit rate 5- best threshold

#fit number
fitnumber<-paste0("fit",1:31)


#AIC
fitlist<-mget(fitnumber)

getaic<-function(x){
        return(x$aic)
}

AICs<-sapply(fitlist,getaic)

#AUC
elist<-mget(paste(fitnumber,"e",sep = "_"))

getauc<-function(x){return(x$auc$auc)}
AUC<-sapply(elist,getauc)

#hit rate

hit<-function(x){return(x$hit_rate)}

hit_rate<-sapply(elist,hit)

#best threshold

threshold<-function(x){return(x$choosed_threshold)}

best_threshlod<-sapply(elist,threshold)


fit_table<-data.frame(
        fit=factor(fitnumber),
        AIC=AICs,
        AUC=AUC,
        hit_rate=hit_rate,
        best_threshlod=best_threshlod
)


par(mfrow=c(2,2))
plot(reorder(fit_table$fit,fit_table$AIC),fit_table$AIC,main="AIC",xlab="fit",ylab="AIC") #fit2
plot(reorder(fit_table$fit,-fit_table$AUC),fit_table$AUC,main="AUC",xlab="fit",ylab="AUC") #fit16
plot(reorder(fit_table$fit,-fit_table$hit_rate),fit_table$hit_rate,main,main="hit_rate",xlab="fit",ylab="hit_rate") #fit2

fit_table[which.max(fit_table$AUC),]
fit_table[which.max(fit_table$hit_rate),]
fit_table[which.min(fit_table$AIC),]

summary(fit30)
