#This data is to predicted PCOS in women 

#loading lobraries

library(tidyverse);library(mice);library(car);library(pROC)

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


#lookin at NAs and see if it cause a problems

napattern<-sapply(dat,function(x){
        return(list(count=sum(is.na(x)),
                    percent=sum(is.na(x))/length(x)))
})

print(napattern)


#a single NA is not a big deal , but i will remove X43 , it is a problematic
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

par(mfrow=c(3,4))
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
num<-na.omit(num)
mah<-mahalanobis(x = num,center = colMeans(num,na.rm = T),cov = cov(num))
cut<-qchisq(p = .95,df = ncol(num))

outliers<-which(mah>cut) #keep this in your mind for later inspection
length(outliers) #45 value are outlier 

data<-data[-outliers,] #removing them from the data 

####################
#fitting the first model
set.seed(453)
idx<-sample(1:nrow(data),size = .75*nrow(data),replace = F)

train<-data[idx,]%>%na.omit()

model<-c()

name<-names(data[,-c(1,2,3)])

for( i in 1:length(name)){
        if(i == 1){model<<-name[i]}
        else{model<<-paste(model,name[i],sep = "+")}
}
print(model)

#make sure that all variables are included

str_count(model,"\\+")# 36 + means 37 variables


#Do stepback regression 

fit1<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Ageyrs+BMI+BloodGroup+Pulseratebpm+RRbreathsmin+Hbgdl+CycleRI+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit1)

#looking at residual outliers 
rz<-function(model,y){
        phat<-as.numeric(predict(object = model,type="response"))
        (y-phat)/(sqrt(phat*(1-phat)))
}


ri<-rz(model = fit1,y = (as.numeric(train$PCOSYN)-1))

residout<-which(abs(ri)>=2.5)


train2<-train[-residout,]#removing outliers

fit1_<-train2%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Ageyrs+BMI+BloodGroup+Pulseratebpm+RRbreathsmin+Hbgdl+CycleRI+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit1_)

par(mfrow=c(2,2))
plot(fit1_) #we have a problem in normality 
#Now i will plot residual against each of predictors to see the one which causes the problem
sapply(train2[,-c(1,2,3)],function(x){
        plot(x=x,y=rstudent(fit1_),main = paste0(colnames(x)))
}) # here we can see that the relationship between each predictor and y is linear and the only problem is the presence of univariate outliers 


#Now lets predict some values

test<-data[-idx,]

fit1_pred<-predict.glm(fit1_,newdata = test,type = "response")


auc<-roc(test$PCOSYN,fit1_pred)

plot(auc)

auc$auc #0.9563

confdat<-cbind(auc$sensitivities,(1-auc$specificities),auc$thresholds)%>%data.frame()

confdat%>%filter(X1==max(X1))%>%filter(X2==min(X2))


#removing highest p-value
summary(fit1)$coefficients[,4]%>%which.max() #bloodgroup

fit2<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Ageyrs+BMI+Pulseratebpm+RRbreathsmin+Hbgdl+CycleRI+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit2)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit2))

LR>=qchisq(p = .99,df = df) #Reject H0: no difference 

#removing highest p-value
summary(fit2)$coefficients[,4]%>%which.max() #BMI

fit3<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Ageyrs+Pulseratebpm+RRbreathsmin+Hbgdl+CycleRI+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit3)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit3))

LR>=qchisq(p = .99,df = df) #Reject H0: no difference 

#removing highest p-value
summary(fit3)$coefficients[,4]%>%which.max() #CycleRI

fit4<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Ageyrs+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit4)
par(mfrow=c(2,2))
plot(fit4)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit4)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit4))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit4)$coefficients[,4]%>%which.max()

fit5<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+RBSmgdl+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit5)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit5))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit5)$coefficients[,4]%>%which.max()

fit6<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPSystolicmmHg+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit6)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit6))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit6)$coefficients[,4]%>%which.max()

fit7<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+BPDiastolicmmHg+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit7)
par(mfrow=c(2,2))
plot(fit7)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit7)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit7))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit7)$coefficients[,4]%>%which.max()

fit8<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+PRLngmL+VitD3ngmL+PRGngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit8)
par(mfrow=c(2,2))
plot(fit8)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit8)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit8))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit8)$coefficients[,4]%>%which.max()

fit9<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+VitD3ngmL+PRGngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit9)
par(mfrow=c(2,2))
plot(fit9)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit9)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit9))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit9)$coefficients[,4]%>%which.max()

fit10<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+FSHLH+WaistHipRatio+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit10)
par(mfrow=c(2,2))
plot(fit10)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit10)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit10))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit10)$coefficients[,4]%>%which.max()

fit11<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+WaistHipRatio+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm+Endometriummm,family = "binomial")
summary(fit11)
par(mfrow=c(2,2))
plot(fit11)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit11)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit11))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit11)$coefficients[,4]%>%which.max()

fit12<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+PregnantYN+Noofaborptions+FSHmIUmL+LHmIUmL+WaistHipRatio+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm,family = "binomial")
summary(fit12)
par(mfrow=c(2,2))
plot(fit12)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit12)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit12))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit12)$coefficients[,4]%>%which.max()

fit13<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+AMHngmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+FSHmIUmL+LHmIUmL+WaistHipRatio+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm,family = "binomial")
summary(fit13)
par(mfrow=c(2,2))
plot(fit13)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit13)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit13))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit13)$coefficients[,4]%>%which.max()

fit14<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+FSHmIUmL+LHmIUmL+WaistHipRatio+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm,family = "binomial")
summary(fit14)
par(mfrow=c(2,2))
plot(fit14)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit14)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit14))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit14)$coefficients[,4]%>%which.max()

fit15<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm+AvgFsizeRmm,family = "binomial")
summary(fit15)
par(mfrow=c(2,2))
plot(fit15)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit15)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit15))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit15)$coefficients[,4]%>%which.max()

fit16<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+RegExerciseYN+FollicleNoL+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit16)
par(mfrow=c(2,2))
plot(fit16)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit16)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit16))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit16)$coefficients[,4]%>%which.max()

fit17<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+FollicleNoL+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit17)
par(mfrow=c(2,2))
plot(fit17)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit17)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit17))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit17)$coefficients[,4]%>%which.max()

fit18<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+RRbreathsmin+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit18)
par(mfrow=c(2,2))
plot(fit18)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit18)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit18))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit18)$coefficients[,4]%>%which.max()

fit19<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+PimplesYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit19)
par(mfrow=c(2,2))
plot(fit19)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit19)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit19))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit19)$coefficients[,4]%>%which.max()

fit20<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+Hbgdl+Cyclelengthdays+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit20)
par(mfrow=c(2,2))
plot(fit20)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit20)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit20))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit20)$coefficients[,4]%>%which.max()

fit21<-train%>%glm(formula = PCOSYN~IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit21)
par(mfrow=c(2,2))
plot(fit21)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit21)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit21))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit21)$coefficients[,4]%>%which.max()

fit22<-train%>%glm(formula = PCOSYN~IIbetaHCGmIUmL+Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit22)
par(mfrow=c(2,2))
plot(fit22)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit22)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit22))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit22)$coefficients[,4]%>%which.max()

fit23<-train%>%glm(formula = PCOSYN~Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit23)
par(mfrow=c(2,2))
plot(fit23)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit23)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit23))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit22)$coefficients[,4]%>%which.max()

fit23<-train%>%glm(formula = PCOSYN~Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit23)
par(mfrow=c(2,2))
plot(fit23)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit23)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit23))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit23)$coefficients[,4]%>%which.max()

fit24<-train%>%glm(formula = PCOSYN~Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit24)
par(mfrow=c(2,2))
plot(fit24)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit24)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit24))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit24)$coefficients[,4]%>%which.max()

fit25<-train%>%glm(formula = PCOSYN~Pulseratebpm+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit25)
par(mfrow=c(2,2))
plot(fit25)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit25)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit25))

LR>=qchisq(p = .99,df = df)



#removing highest p-value
summary(fit25)$coefficients[,4]%>%which.max()

fit26<-train%>%glm(formula = PCOSYN~Pulseratebpm+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit26)
par(mfrow=c(2,2))
plot(fit26)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit26)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit26))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit26)$coefficients[,4]%>%which.max()

fit27<-train%>%glm(formula = PCOSYN~Pulseratebpm+MarraigeStatusYrs+Noofaborptions+LHmIUmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit27)
par(mfrow=c(2,2))
plot(fit27)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit27)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit27))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit27)$coefficients[,4]%>%which.max()

fit28<-train%>%glm(formula = PCOSYN~MarraigeStatusYrs+Noofaborptions+LHmIUmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit28)
par(mfrow=c(2,2))
plot(fit28)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit28)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit28))

LR>=qchisq(p = .99,df = df)

#removing highest p-value
summary(fit28)$coefficients[,4]%>%which.max()

fit29<-train%>%glm(formula = PCOSYN~MarraigeStatusYrs+Noofaborptions+LHmIUmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR,family = "binomial")
summary(fit29)
par(mfrow=c(2,2))
plot(fit29)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit29)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit29))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit29)$coefficients[,4]%>%which.max()

fit30<-train%>%glm(formula = PCOSYN~MarraigeStatusYrs+LHmIUmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR,family = "binomial")
summary(fit30)
par(mfrow=c(2,2))
plot(fit30)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit30)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit30))

LR>=qchisq(p = .99,df = df)


#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit29)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit29))

LR>=qchisq(p = .99,df = df)


#removing highest p-value
summary(fit30)$coefficients[,4]%>%which.max()

fit31<-train%>%glm(formula = PCOSYN~MarraigeStatusYrs+WeightgainYN+hairgrowthYN+SkindarkeningYN+FastfoodYN+FollicleNoR,family = "binomial")
summary(fit31)
par(mfrow=c(2,2))
plot(fit31)

#LRT to examine the significance 

llfull<-logLik(fit1)
llR<-logLik(fit31)

LR<-(-2*(llR-llfull))
df<-length(coef(fit1))-length(coef(fit31))

LR>=qchisq(p = .99,df = df)


#getting AIC
listsn<-paste0("fit",1:31)
lists<-mget(listsn)

getaic<-function(x){return(x$aic)}

AICs<-cbind(sapply(lists,getaic),listsn)%>%data.frame()

names(AICs)<-c("AIC","fit")

AICs<-AICs%>%mutate(fit=factor(fit),
                    AIC=as.numeric(AIC))


AICs%>%ggplot(aes(x=reorder(fit,-AIC),y = AIC))+geom_point() #fit 21 is the lowest AIC value

summary(fit21)

#adding the CycleRI variable again to fit21

fit32<-train%>%glm(formula = PCOSYN~CycleRI+IbetaHCGmIUmL+IIbetaHCGmIUmL+Pulseratebpm+Hbgdl+MarraigeStatusYrs+Noofaborptions+LHmIUmL+TSHmIUL+VitD3ngmL+WeightgainYN+hairgrowthYN+SkindarkeningYN+HairlossYN+FastfoodYN+FollicleNoR+AvgFsizeLmm,family = "binomial")
summary(fit32) #it is got lower AIC
