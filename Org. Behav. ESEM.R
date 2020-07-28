#Measurement invariance Of organizational behavior through two nations using ESEM and MGCFA


library(tidyverse);library(mvnormtest);library(MVN);library(car);library(psych);library(GPArotation)
library(lavaan);library(semPlot);library(semTools)

#importing the data 

dir<-"/home/debian/Statistical-practice/data sets/invariance.csv"

dat<-read.csv(dir)

#Firstly screen the data for typos and str 

str(dat)

#Converting chr to factor and int to num

dat$country<-factor(dat$country,labels = c(1,2),levels= c("CAN","LEB"))

dat[,-1]<-apply(dat[,-1],2,as.numeric)

names(dat)<-c("country",c(paste0("H",1:7)),c(paste0("V",1:5)))

#Make sure 

str(dat)

summary(dat) #No typos and No NAs

head(dat)


#Testing the assumptions of Factor Analysis 

#1- MV outlier
dat2<-split(dat,f = dat$country)

#For Canadian
mah1<-mahalanobis(x = dat2[[1]][,-1],center = colMeans(dat2[[1]][,-1]),cov = cov(dat2[[1]][,-1]))
cutpoint1<-qchisq(p = .99,df = 12)
outidx1<-which(abs(mah1)>cutpoint1)

#For lebanon
mah2<-mahalanobis(x = dat2[[2]][,-1],center = colMeans(dat2[[2]][,-1]),cov = cov(dat2[[2]][,-1]))
cutpoint2<-qchisq(p = .99,df = 12)
outidx2<-which(abs(mah2)>cutpoint2)

#2- MV normality
MVnormality<-mvn(data = dat[,-1],multivariatePlot = "qq",mvnTest = "mardia",bc = T)
MVnormality$BoxCoxPowerTransformation

#Here i am going to store outliers for further investigation later 


#Now i going to test sample adequacy and variables additivity (Multicolinearity)

#Firstly sample adequacy using bartlett test and Kaisar-Mayer-Oklin(KMO) test 

cor1<-cor(dat2[[1]][,-1]) #Correlation matrix of CAN
cor2<-cor(dat2[[2]][,-1]) #Correlation Matrix of LEB
corall<-cor(dat[,-1]) #Whole data Cor matrix

cortest.bartlett(R = corall,n = nrow(dat)) #Significant
KMO(r = corall) #.95 is good 


#Now test additivity

corall%>%apply(MARGIN = 2,FUN = function(x){
        any(x<1&abs(x)>=.8)
}) #we have no additivity at all 



cor1%>%apply(MARGIN = 2,FUN = function(x){
        any(x<1&abs(x)>=.8)
}) #we have no additivity at factor 1


cor2%>%apply(MARGIN = 2,FUN = function(x){
        any(x<1&abs(x)>=.8)
}) #we have no additivity at factor 2



#looking at alpha 
F1<-dat[,c("H1","H2","H3","H4","H5","H6","H7")]
F2<-dat[,c("V1","V2","V3","V4","V5")]

alpha(x = F1,check.keys = T)%>%summary() #For Factor 1
alpha(x = F2,check.keys = T)%>%summary() #For Factor 2
alpha(x = dat[,-1],check.keys = T)%>%summary() #For whole test


#starting doing EFA
#extracting the number of factors

nfa<-fa.parallel(x = dat[,-1],fm = "pa",fa = "fa") #the data MV not normal , so i will use principle axis method
#it suggest two factors by parallel analysis and 1 by screeplot
#lets look at kaisar method 

(nfa$fa.values>.7)%>%sum() #it suggest also one , but i will follow the parallel as it assure the theory 

#Now lets do EFA and also alpha test for rleability

efamodel<-fa(r = cov(dat[,-1]),nfactors = 2,n.obs = nrow(dat),rotate = "geominQ",fm = "pa",scores = "regression")
summary(efamodel)

fa.diagram(efamodel)

load<-efamodel$loadings%>%matrix(ncol = 2,dimnames = list(names(dat[,-1]),paste0("F",1:2)))%>%
        apply(MARGIN = 2,function(x){
        ifelse(x>.3,round(x,digits = 2),"-")
})%>%data.frame()

print(load)

fcor<-efamodel$r.scores%>%matrix(nrow = 2,dimnames = list(paste0("F",1:2),paste0("F",1:2)))

print(fcor)

#here we can't say that the factors are orthogonal , and we suggest a higher order factor 
#but we will continue 

#Building CFA model 

model<-paste(paste0("f1","=~",paste0("H",1:7,collapse = "+")),
             paste0("f2","=~",paste0("V",1:5,collapse = "+")),sep = "\n")

cfamodel<-cfa(model = model,data = dat[,-1],estimator="MLM")

summary(cfamodel,standardized=T,fit.measures=T) #Robust CFI is good ,also robust RMSEA

cfafit<-fitmeasures(object = cfamodel,fit.measures = c("cfi.scaled","rmsea.scaled","rmsea.ci.upper.scaled","rmsea.ci.lower.scaled"))

#plotting it 

semPaths(object = cfamodel,whatLabels = "std",layout = "tree")

#Now i will Build ESEM model to compare between it and CFA model in term of CFI and RMSEA

rawload<-efamodel$loadings%>%matrix(ncol = 2,dimnames =list(names(dat[,-1]),paste0("F",1:2)))


modelesem<-paste(paste0("f1","=~",paste0(rawload[,1],"*",rownames(rawload),collapse = "+")),
                 paste0("f2","=~",paste0(rawload[,2],"*",rownames(rawload),collapse = "+")),sep = "\n")

esemmodel<-cfa(model = modelesem,data = dat[,-1],estimator="MLM")

summary(esemmodel,standardized=T,fit.measures=T)

esemfit<-fitmeasures(object = esemmodel,fit.measures = c("cfi.scaled","rmsea.scaled","rmsea.ci.upper.scaled","rmsea.ci.lower.scaled"))


#Putting Both together 

twomodelfit<-bind_rows(cfafit,esemfit)

print(twomodelfit) #there are an improvement in both CFI and RMSEA , but RMSEA stills not good 

#measurement inv manuallu 

fitB<-cfa(model = model,data = dat,estimator="MLM",group = "country")
fitL<-cfa(model = model,data = dat,estimator="MLM",group = "country",group.equal="loadings")
fitI<-cfa(model = model,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts"))
fitFV<-cfa(model = model,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","lv.variances"))
fitR<-cfa(model = model,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","residuals","residuals"))
fitM<-cfa(model = model,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","residuals","residuals","means"))

compareFit(fitB,fitL,fitI,fitFV,fitR,fitM,indices = T) #fail to prove the invariance of residual


#Determining which parameters to let it free for testing partial invariance 

rvariance<-paste(colnames(dat[,-1]),colnames(dat[,-1]),sep = "~~")

cfa_chidiff<-list()
for (i in rvariance){
        fittest<-cfa(model = model,
                     estimator="MLM",
                     data = dat,group = "country",
                     group.equal=c("loadings","intercepts","lv.variances","residuals"),
                     group.partial=i)
        cfa_chidiff[[i]]<-c(i,fitmeasures(fitR,"chisq")-fitmeasures(fittest,"chisq"))
}

cfachisq_diff<-bind_rows(cfa_chidiff)%>%t()%>%matrix(ncol = 2,dimnames = list(c(NULL),c("Variance","Chisq_Diff")))%>%data.frame()
cfachisq_diff$Chisq_Diff<-as.numeric(cfachisq_diff$Chisq_Diff)
cfachisq_diff<-cfachisq_diff[order(cfachisq_diff$Chisq_Diff,decreasing = T),]

print(cfachisq_diff)

#now fit the residual model with partial invariance

fitRP<-cfa(model = model,
           data = dat,estimator="MLM",group = "country",
           group.equal=c("loadings","intercepts","lv.variances","residuals"),
           group.partial=c("H2~~H2","V1~~V1"))

compareFit(fitB,fitL,fitI,fitFV,fitRP,fitM) #only two variances need to be free





#Now testing the Invariance using ESEM
esemB<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country")
esemL<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country",group.equal="loadings")
esemI<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts"))
esemFV<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","lv.variances"))
esemR<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","lv.variances","residuals"))
esemM<-cfa(model = modelesem,data = dat,estimator="MLM",group = "country",group.equal=c("loadings","intercepts","lv.variances","residuals","means"))

compareFit(esemB,esemL,esemI,esemFV,esemR,esemM)





#Determining Free parameters

chidiff<-list()
for (i in rvariance){
        fittest<-cfa(model = modelesem,
                     data = dat,estimator="MLM",group = "country",
                     group.equal=c("loadings","intercepts","lv.variances","residuals"),
                     group.partial=i)
        chidiff[[i]]<-c(i,fitmeasures(esemR,"chisq")-fitmeasures(fittest,"chisq"))
}

chisq_diff<-bind_rows(chidiff)%>%t()%>%matrix(ncol = 2,dimnames = list(c(NULL),c("Variance","Chisq Diff")))%>%data.frame()
chisq_diff$Chisq.Diff<-as.numeric(chisq_diff$Chisq.Diff)
chisq_diff<-chisq_diff[order(chisq_diff$Chisq.Diff,decreasing = T),]

print(chisq_diff)

#now fit the residual model with partial invariance

esemRP<-cfa(model = modelesem,
            data = dat,estimator="MLM",group = "country",
            group.equal=c("loadings","intercepts","lv.variances","residuals"),
            group.partial=c("H2~~H2","V1~~V1","H5~~H5"))

compareFit(esemB,esemL,esemI,esemFV,esemRP,esemM) 

#Final Note :
#Here we can see that results with both models are so closed to each other 
#ESEM model Show a better fit indecies ( For both CFI and RMSEA , in case of standard , robust , scaled results) when comparing the Two Model structure 
#in case of measurement invariance , CFA and ESEM model Succeed in showing 1-Configural 2-Metric 3-scalar invariance  4-factor variance (sublevel of strict invariance)
#But Both Failed to show Residual invariance 
#in case of partial invariance , CFA showed that there are only Two indicators showing differences in term of residual variance 
#ESEM added to those indicators another one to show the partial invariance 

#These results show that in case of ..... measure , ESEM has no superiority over Regular CFA Model





