#logistic regression practice


library(readr)
library(mice)
library(car)
library(reghelper)
library(caret)

#reading the data

data<-read_csv(file = "./data sets/framingham.csv",col_types = c("fnffnffffnnnnnnf"))

summary(data)

#checking the NAs %

rowmiss<-apply(data,1,function(x){
        (sum(is.na(x))/length(x))*100%>%round(digits = 2)
})

rowmiss%>%mean()
rowmiss%>%min()
rowmiss%>%max()

colmiss<-apply(data,2,function(x){
        (sum(is.na(x))/length(x))*100%>%round(digits = 2)
})

colmiss%>%mean()
colmiss%>%min()
colmiss%>%max()

#use multible imputation to impute data 

#numeric data for imputation

set.seed(785)
mi<-mice(data)


data<-complete(mi)

#check rare case
tab<-table(data$TenYearCHD)
prop.table(tab)
#we are facing a rare case problem 

#lets bootstrap it

cardiac<-data%>%filter(TenYearCHD==1)

set.seed(7548)
cardiac<-cardiac%>%sample_n(1000,replace = T)

data<-rbind(data,cardiac)

#testing the zero cell 

data%>%group_by(male,education,currentSmoker,BPMeds,prevalentStroke,prevalentHyp,diabetes)%>%
        filter(TenYearCHD==1)%>%summarize(n())%>%View


#testing colinearity


logfit<-data%>%glm(formula = TenYearCHD~.,family =binomial(link = "logit") )

vif(logfit) #no colinearity

#delta beta test

betas<-beta(logfit)
betacoef<-betas$coefficients%>%data.frame()
betaesti<-betacoef$Estimate

betaest<-function(data){
        logfit<-data%>%glm(formula = TenYearCHD~.,family =binomial(link = "logit"))
        betas<-beta(logfit)
        betacoef<-betas$coefficients%>%data.frame()
        betaestt<-betacoef$Estimate
        return(betaestt)
}


betaestimate<-c()

for(i in 1:nrow(data)){
        data<-data[-i,]
        bet<-betaest(data)
        betaestimate<<-cbind(betaestimate,bet)
}

dif<-apply(betaestimate,2,function(x){
        x-betaesti
})%>%t()%>%data.frame()

apply(dif,2,function(x){
        any(abs(x)>=1)
})

#---------------------------------------------

#testing the overall association between predictors and outcome and pearson residuals


basefit<-data%>%glm(formula = TenYearCHD ~1,family = binomial(link = "logit"))
Pbasic<-predict(basefit,type = "response")
summary(basefit)

logfit<-data%>%glm(formula = TenYearCHD~.,family =binomial(link = "logit") )
Pfull<-predict(logfit,type = "response")
summary(logfit)

pearsr<-function(y,p){
        (y-p)/sqrt(p*(1-p))
}


r<-pearsr(y = (as.numeric(data$TenYearCHD)-1),p = Pfull)

poorfit<-which(abs(r)>=3)


LL<-function(y,p){
        y<-(as.numeric(y)-1)
        LL<-(-2*(sum(y*log(p)+((1-y)*log(1-p)))))
}

Lbasic<-LL(y = data$TenYearCHD,p = Pbasic)

Lfull<-LL(y=data$TenYearCHD,p=Pfull)

chi<-Lbasic-Lfull

qchisq(p = .95,df = 15)

PseudoR2<-chi/Lbasic

pred<-ifelse(Pfull>.5,1,0)
tab<-table(data$TenYearCHD,pred)%>%data.frame()

confusionMatrix(tab)


#the reduction of error for each variable

errorrestimate<-function(y,null,full){
        
        Pbasic<-predict(null,type="response")
        predbas<-ifelse(Pbasic>.5,1,0)
        tabbas<-table(y,predbas)%>%data.frame()
        nullerror<-tabbas%>%filter(y==1,predbas==0)%>%select(Freq)%>%summarize(nullerror=Freq/length(y))
        
        Pfull<-predict(full,type="response")
        predfull<-ifelse(Pfull>.5,1,0)
        tabfull<-table(y,predfull)%>%data.frame()
        
        fullerror1<-tabfull%>%filter(y==1,predfull==0)
        fullerror2<-tabfull%>%filter(y==0,predfull==1)
        
        fullerror1<-fullerror1$Freq
        fullerror1<-ifelse(length(fullerror1)>=1,fullerror1,0)
        fullerror2<-fullerror2$Freq
        fullerror2<-ifelse(length(fullerror2)>=1,fullerror2,0)
        
        fullerror<-((fullerror1+fullerror2)/length(y))
        return(list(nullerror=nullerror$nullerror,fullerror=fullerror))
}

erros<-errorrestimate(y = data$TenYearCHD,null = basefit,full = logfit)

dtest<-function(fullerror,nullerror,n){
        (nullerror-fullerror)/sqrt((nullerror*(1-nullerror))/n)
}


ds<-list()

for(i in 1:15){
        data2<-data[,i]
        data2<-data.frame(data2,TenYearCHD=data$TenYearCHD)
        nullfit<-data%>%glm(formula = TenYearCHD~1,family = binomial(link = "logit"))
        fullfit<-data2%>%glm(formula = TenYearCHD ~.,family = binomial(link = "logit"))
        errors<-errorrestimate(y = data2$TenYearCHD,null = nullfit,full = fullfit)
        ds[[i]]<-dtest(fullerror = errors$fullerror,nullerror = errors$nullerror,n = nrow(data2))
}


ds<-unlist(ds)
prop<-pnorm(q = ds)

prop

table(prop)

idx<-which(prop>.5)

choosed<-data[,c(idx,16)]

choosedfit<-choosed%>%glm(formula = TenYearCHD~.,family = binomial(link = "logit"))
summary(choosedfit)

choosepred<-predict(choosedfit,type = "response")
choosepred<-ifelse(choosepred>.5,1,0)
table(data$TenYearCHD,choosepred)%>%confusionMatrix()
table(data$TenYearCHD,pred)%>%confusionMatrix()

