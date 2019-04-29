#the aim of this analysis to test the difference type of regression and its accuracy to predict the price of houses 

#firstly lets load the libraries we are going to use 


```{r}
library(tidyverse)
library(caret)
library(DMwR)
library(mice)
library(car)
library(lm.beta)
```

#Next lets import the data 

```{r}
houses<-read_csv(file = "./data sets/kc_house_data.csv",col_types = "cTninnnifffinnddf??nn")

head(houses)
```

#checking the structure of the data 

```{r}
str(houses)
```

#removing id lat long date

```{r}
houses<-houses%>%select(-c(id,date,lat,long))
```

#need to check the condition if it need to be ordered 

```{r}
unique(houses$condition)
```

#i think that is preferable if it is ordered

```{r}
houses<-houses%>%mutate(condition=factor(condition,levels = c("1","2","3","4","5"),labels = c("1","2","3","4","5"),ordered = F))
```


#checking the structure again 

```{r}
str(houses)
```

#now we want to test some assumption 

#the most important one to start with is the missing values , what about it ?

```{r}
summary(houses)
```

#we have a problem only with floor

#lets check the percent of missing 

```{r}
(sum(is.na(houses$floors))/nrow(houses)
)*100
```

#only 10% of missings , so i will impute it using KNN method 

#first i want to extract the numeric variables 

```{r}
num<-houses%>%select(price,bedrooms,bathrooms,sqft_living,sqft_lot,floors,sqft_above,sqft_basement,sqft_living15,sqft_lot15)%>%data.frame()

set.seed(3215)

knn<-(knnImputation(data = num))

houses<-houses%>%mutate(floors=knn$floors,
                        floors=as.integer(round(floors,0)))
```

#check again for missing 

```{r}
(sum(is.na(houses$floors))/nrow(houses)
)*100
```

#great

#now lets run regular regression to test the assumption

```{r}
fit<-houses%>%with(lm(price~.,data=houses))
summary(fit)
```


#here from round 1 we can see that most of predictors are significant 

#also we see that the ares of basement has nothing to do for prediction , so i will remove it and run the analysis again

```{r}
houses<-houses%>%select(-sqft_basement)

fit<-houses%>%with(lm(price~.,data=houses))
summary(fit)
```

#another thing to take care with is that zibcode has alot of levels and we need to round it 

#according to residual i will try to round it to small numbers of area 

```{r}
grouping<-houses%>%mutate(resid=resid(fit))%>%
        group_by(zipcode)%>%summarize(medresid=median(resid),
                                      cnt=n())%>%arrange(medresid)%>%
        mutate(cumcnt=cumsum(cnt),
               group=ntile(cumcnt,5))

houses<-houses%>%left_join(grouping[,c("zipcode","group")])%>%mutate(group=factor(group))
```

#now remove the zip from the data and run fit again 

```{r}
houses<-houses%>%select(-zipcode)

fit<-houses%>%with(lm(price~.,data=houses))
summary(fit)
```

#coefficients here are beutifull

#now what about outlier!!

```{r}
num2<-houses%>%select(price,bedrooms,bathrooms,sqft_living,sqft_lot,floors,sqft_above,sqft_living15,sqft_lot15)

mah<-mahalanobis(x = num2,center = colMeans(num2),cov = cov(num2))

cutmahup<-qchisq(.99,ncol(num2))
cutmahdown<-qchisq(.01,ncol(num2))

outidxup<-mah>cutmahup%>%as.numeric()
outidxdown<-mah<cutmahdown%>%as.numeric()

table(outidxup)
table(outidxdown)

```

#next testing for infeluantial points 

```{r}
inf<-hatvalues(fit)
cutinf<-(2*(ncol(houses)-1)+1)/nrow(houses)

table(inf>cutinf)

```

#this data is horrible !!


```{r}
infidx<-inf>cutinf%>%as.numeric()
```

#testing cooks distance 

```{r}
cook<-cooks.distance(fit)

cutcook<-4/(nrow(houses)-(ncol(houses)-1)-1)

table(cook>cutcook)

cookidx<-cook>cutcook%>%as.numeric()

```

#testing for variance inflation 
```{r}
vif<-vif(fit)
vif
```

#good news , we don't have here any vif value over 10 and this indicate abscence of multicolinearity 

#testing auto correlation 

```{r}
set.seed(5643)
auto<-durbinWatsonTest(fit,simulate = T,reps = 1000,method = "resample")
auto
```

#great , pvalue indicate that there is no autocorrelation

#Now lets see the most bad records inour data 

```{r}
allbad<-outidxup+outidxdown+infidx+cookidx

table(allbad)
```

#i will delete all records which break 2 rules 

```{r}
bad<-allbad>=2

table(bad)

houses<-houses%>%anti_join(houses[bad,])
```

#running the fit again 

```{r}
fit<-houses%>%with(lm(price~.,data=houses))
summary(fit)
```

#extracting the standardized fitted value and standardized residuals

```{r}
stdresid<-rstandard(fit)
stdfit<-predict(fit)%>%scale(scale = T)

diagnostics<-data.frame(stdresid,stdfit,`residual outlier`=abs(stdresid)>=3)

diagnostics%>%ggplot(aes(stdfit,stdresid))+geom_point()+geom_smooth()+
        geom_vline(xintercept = 0,lty="dashed",color="red",lwd=1)+
        geom_hline(yintercept = 0,lty="dashed",color="red",lwd=1)+
        xlab("Standardized fitted values")+
        ylab("Standardized Residuals")+
        ggtitle("Diagnostic plot to test homogeniety , homoscedasticity and linearity ")

```

#wesee here that we have some degree of heteroscedasticity and nonlinearity

#checking the normality 

```{r}
qqPlot(fit)
```

#ok , the qqplot shows that the data has a degree of skewness 

#and to prove it lets see histogram

```{r}
hist(stdresid)
```

#some right skew but the data still good 

#the only issue we have to handle is linearity and heteroscedasticity 

#now i will check partial residual to know the problematic variable

```{r}
resid<-resid(fit)
term<-predict(fit,houses,type = "terms")%>%data.frame()

partial.resid<-resid+term
houses.org<-houses
names(houses.org)<-paste("org",names(houses),sep = ".")
names(partial.resid)<-paste("part",names(partial.resid),sep = ".")
names(term)<-paste("term",names(term),sep = ".")
allpartial<-cbind(houses.org[,-1],partial.resid,term)


allpartial%>%ggplot(aes(org.sqft_living,part.sqft_living))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.sqft_living,y=term.sqft_living),color="red",lwd=1)

allpartial%>%ggplot(aes(org.sqft_lot,part.sqft_lot))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.sqft_lot,y=term.sqft_lot),color="red",lwd=1)


allpartial%>%ggplot(aes(org.sqft_above,part.sqft_above))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.sqft_above,y=term.sqft_above),color="red",lwd=1)

allpartial%>%ggplot(aes(org.sqft_living15,part.sqft_living15))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.sqft_living15,y=term.sqft_living15),color="red",lwd=1)

allpartial%>%ggplot(aes(org.sqft_lot15,part.sqft_lot15))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.sqft_lot15,y=term.sqft_lot15),color="red",lwd=1)

allpartial%>%ggplot(aes(org.yr_built,part.yr_built))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.yr_built,y=term.yr_built),color="red",lwd=1)


allpartial%>%ggplot(aes(org.yr_renovated,part.yr_renovated))+
        geom_point()+
        geom_smooth()+
        geom_line(aes(x = org.yr_renovated,y=term.yr_renovated),color="red",lwd=1)

```

#here we can find that the most problems associated with square foot of lot and the same variable in 2015 and the year of reonivation 

#i will catogrize the renovation variable into 4 categories Not renovated - 1900-1940 , 1941-1980 , 1981-2015

```{r}
houses$yr_renovated%>%table()
```

#removing the zeros and specify the cumulative sum of the number of houses renovated 

```{r}
catig<-function(percent){
        
        ifelse(percent<.5,1,ifelse(percent>=.5&percent<.75,2,3))
}

catigor<-houses%>%filter(yr_renovated!=0)%>%
        group_by(yr_renovated)%>%summarize(cnt=n())%>%mutate(cumcnt=cumsum(cnt),
                                        percent=cumcnt/sum(houses$yr_renovated>0))



catigor<-catigor%>%mutate(renovC=catig(percent = percent))%>%select(yr_renovated,renovC)


houses<-(houses)%>%full_join(catigor)

houses$renovC<-houses$renovC%>%replace_na(0)%>%factor()

houses<-houses%>%select(-yr_renovated)

table(houses$renovC)

```

#recalculating the fit

```{r}
fit<-houses%>%with(lm(price~.,data=houses))
summary(fit)
```

#we can see here that renovation in first 80 years has no effect on the price but after that has 

#now lets try the weighted regression and start with calculating the wheights

```{r}
residl<-rstandard(fit)%>%abs()
fited<-fitted(fit)
residfit<-lm(residl~fited)
fittresid<-fitted(residfit)
wts<-1/fittresid^2

wtsfit<-houses%>%with(lm(price~.,data=houses,weights = wts))
summary(wtsfit)

par(mfrow=c(1,1))
plot(wtsfitted,wtsstdresid)

par(mfrow=c(2,2))
plot(wtsfit)
```

#we see here an improve in the linearity and heteroscedasticity

#comparing the coeficients of OLS model and weighted model

```{r}
data.frame(cbind(lm.beta(fit)$coefficients,lm.beta(wtsfit)$coefficients))

```

#After seeing what we can do to improve our model , lets run the predictive mode using caret

```{r}
houses<-cbind(houses,wts)
trainidx<-createDataPartition(y = houses$price,p = .75,list = F)
traindata<-houses[trainidx,]
testdata<-houses[-trainidx,]

```

#running the model

```{r}
trainfit<-traindata%>%with(lm(price~.,data = traindata[,-length(names(traindata))],weights = traindata$wts))
summary(trainfit)

predicted<-predict(trainfit,testdata)

RMSE(pred = predicted,obs = testdata$price)
```

#trying removing non significant coefs

```{r}
traindata<-traindata%>%select(-c(condition))

trainfit<-traindata%>%with(lm(price~.,data = traindata[,-length(names(traindata))],weights = traindata$wts))
summary(trainfit)

predicted<-predict(trainfit,testdata)

RMSE(pred = predicted,obs = testdata$price)
```

