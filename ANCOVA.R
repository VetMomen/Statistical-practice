#loading the libraries 

```{r}
library(knitr)
library(tidyverse)
library(car)
library(effects)
library(multcomp)
library(MOTE)
```

#loading the data 

```{r}
data<-read.csv("./data sets/bn ancova.csv")

head(data)
```
#selecting the variables 

```{r}
data<-data[,c("ï..subjno","attdrug","phyheal","emplmnt","religion")]

head(data)
```
# the assumptions includes :
##Accuracy
##missing 
##outliers
##aditivity
##Normality
##Linearity
##Homogeniety + homoscdasticity 

#accuracy

```{r}
summary(data) #we have a problem in levels of religion

data<-data%>%mutate(religion=factor(religion,levels = c("CATHOLIC",
                                                        "JEWISH",
                                                        "NONE OR OTHER",
                                                        "PROTESTANT"),labels = c("CATHOLIC",
                                                                                 "JEWISH",
                                                                                 "NONE",
                                                                                 "PROTESTANT")))
summary(data)
```

#missing --> we have only 3 cases in religion 

#outliers -- > mahalanobis of CV and DV
```{r}
mah<-mahalanobis(x = data[,c("attdrug","phyheal")],center = colMeans(data[,c("attdrug","phyheal")]),cov = cov(data[,c("attdrug","phyheal")]))
cut<-qchisq(p = .99,df = 2)

data<-data[which(mah<cut),]
```
#sditivity

```{r}
(cor(data$attdrug,data$phyheal))
```
#normality
```{r}
chi<-rchisq(nrow(data),df = 7)

fake<-lm(chi~data$attdrug+data$phyheal)

stdresid<-rstudent(fake)
stdfit<-scale(fitted(fake))

plot(stdresid,stdfit)%>%abline(v = 0)%>%abline(h= 0)
```
#we might face a problem in homogeniety but the homoscdacity is great 

```{r}
qqPlot(fake) # the linearity is not so bad 

hist(stdresid)
```
#homogeneity

```{r}
leveneTest(data$attdrug~data$emplmnt*data$religion) # the homogeneity is good
```
#now lets start in ANCOVA

```{r}
anc<-data%>%with(lm(attdrug~emplmnt*religion+phyheal,data = data))

summary.aov(anc)
```
#here we see that the Covariance effect of physical health is significant 
#and this is an indicator to the adjust effect of physical health on the attitude 
#another thing is the significance effect of the interaction 
#which mean that the significant effect of employment group is dependent
#on the significance level of the religion group

#main effect of every effect 

#physical health 
#SS Variable/(SS residual + SS Variable)
```{r}
SSR<-566.5
13.9/(SSR+13.9)

#Employment

2.3/(SSR+2.3)

#religion
7/(SSR+7)

#interaction
9.5/(SSR+9.5)
```
#getting the adjusted mean using effects library


```{r}
effect("emplmnt",anc) #adjusted mean fot Employment 
effect("religion",anc)#adjusted mean fot religion
effect("emplmnt*religion",anc)#adjusted mean fot interaction
```
#now we will run post hoc test for interaction
#and to do it you have  to take care of adjusted means 
#so the pairwise  t test is not appropite
#we will use tukyHSD using multcomp package 


#then we have to separate the data sets of each condition and run
#ancova for each part to do paireise comparison 
```{r}
set1<-data%>%filter(religion=="CATHOLIC")
set2<-data%>%filter(religion=="JEWISH")
set3<-data%>%filter(religion=="PROTESTANT")
set4<-data%>%filter(religion=="NONE")

fit1<-set1%>%with(lm(attdrug~phyheal+emplmnt))
fit2<-set2%>%with(lm(attdrug~phyheal+emplmnt))
fit3<-set3%>%with(lm(attdrug~phyheal+emplmnt))
fit4<-set4%>%with(lm(attdrug~phyheal+emplmnt))
```

#Now lets use TukeyHSD for multiple 
#comparison after adjusting for covariate 
#using (general linear hypothesis testing package glht())
```{r}
glht(fit1,linfct=mcp(emplmnt="Tukey"))%>%summary
glht(fit2,linfct=mcp(emplmnt="Tukey"))%>%summary
glht(fit3,linfct=mcp(emplmnt="Tukey"))%>%summary
glht(fit4,linfct=mcp(emplmnt="Tukey"))%>%summary
```
#Now lets look at the effect size of multiple comparisons 
#of the interaction using MOTE package 

```{r}
data%>%group_by(emplmnt,religion)%>%summarize(sd=sd(attdrug),
                                              Num=n())
```
#we will depend on the mean estimated from 
#effect package wich adjusted for Covariance

```{r}
d.ind.t(m1 = 8.04, sd1 = 1.09, n1 = 56,
        m2 = 7.71, sd2 =  .99, n2 = 62,
        a = .05)$d #catholic Employed Vs Catholic unemployed

d.ind.t(m1 = 7.78, sd1 = 1.21, n1 = 47,
        m2 = 7.61, sd2 = 1.11, n2 = 44,
        a = .05)$d #jwish Employed Vs jwish unemployed

d.ind.t(m1 = 7.82, sd1 = 1.19, n1 = 83,
        m2 = 7.50, sd2 =  1.08, n2 = 92,
        a = .05)$d #protestant Employed Vs protestant unemployed

d.ind.t(m1 = 7.12, sd1 = 1.18, n1 = 30,
        m2 = 7.67, sd2 =  1.35, n2 = 46,
        a = .05)$d #none Employed Vs none unemployed
```




