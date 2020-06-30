library(tidyverse);library(lubridate);library(aTSA);library(lmtest)

#here i am trying to understand the price of 500 kg animal from last records and other weights

#load the data 

dat<-read_rds("/home/debian/Statistical-practice/data sets/beef_price.rds")

#rename

renam<-c("Type","Weight","Price","Direction","Date")

names(dat)<-renam

#removing direction

dat<-dat%>%select(-Direction)

#extracting weight value

dat<-dat%>%mutate(Weight=str_extract_all(string = Weight,pattern = "[:digit:]{2,}"))

#extractin gthe date 
        
dat<-dat%>%mutate(Date=str_extract_all(string = Date,pattern = "([:digit:]{4})(\\-)([:digit:]{2})(\\-)([:digit:]{2})"))%>%mutate(Date=ymd(Date))

#renaming the Type Values

unique(dat$Type)


dat<-dat%>%mutate(Type=ifelse(Type=="جمسي بلدى (300 كجم)","B_Balady",
                              ifelse(Type=="جمسي بلدي (350كيلو )","B_Balady",
                                     ifelse(Type=="جمسي بلدي (500كيلو)","B_Balady",
                                            ifelse(Type=="بقري خليط بلدي (200كيلو)","C_Mixed_Balady",
                                                   ifelse(Type=="بقري خليط بلدي (250 كيلو)","C_Mixed_Balady",
                                                          ifelse(Type=="بقري خليط بلدي (300كيلو)","C_Mixed_Balady",
                                                                 ifelse(Type=="بقري خليط  بلدى (400 كيلو)","C_Mixed_Balady",
                                                                        ifelse(Type=="جمسي بلدي(400 كيلو)","B_Balady",
                                                                               ifelse(Type=="بقري مستورد(400كيلو)","C_Imported",

                                                                                      
                                                                                      
                                                                                                                                                                            ifelse(Type=="بقري مستورد(500 كيلو)","C_Imported",NA)))))))))))
#adding variable month to use it in forcast

dat<-dat%>%mutate(month=month(Date),years=year(Date))

#Converting the weights into variables
#merging weight and type

dat2<-dat%>%mutate(Type_w=paste(Type,Weight,sep = "_"))%>%select(-c(Type,Weight))

dat2<-dat2%>%mutate(YM=paste(years,month,"00",sep = "-"))%>%select(-c(month,years))

dat2<-dat2%>%group_by(Type_w,YM)%>%mutate(id=1:n())%>%pivot_wider(names_from = Type_w,values_from=Price)


#checking the data

str(dat2)

summary(dat2)

#knowing the percent of NA

sapply(dat2,function(x){
        (sum(is.na(x))/length(x)%>%round(digits = 2))*100
})

#knowing the dates at which the problematic variables are NAs

naid<-which(is.na(dat2$B_Balady_200))

dat2[naid,] #NA is systematic at 2017 - 2018 , can't impute it 
#i will exclude it temporarily 


#plotting 500 

dat2%>%ggplot(aes(x=Date,C_Mixed_Balady_400))+geom_line()
#take the log of 500 price

dat2<-dat2%>%mutate(logCMIXED400=log(C_Mixed_Balady_400))

theme_set(theme_bw())
dat2%>%ggplot(aes(x=Date,y=logCMIXED400))+geom_line()+geom_smooth()


#best lag auto correlation
logCMIXED400<-dat2$logCMIXED400
ac<-acf(x = na.omit(logCMIXED400),plot = T,type = "partial",lag.max = 120) #lag 1,2,7
which(abs(ac$acf)>=(2/sqrt(nrow(dat2))))
#testing stationarity

adf.test(x = logCMIXED400,nlag = 120) #non stationary

#getting first difference 

DlogCMIXED400<-logCMIXED400-lag(logCMIXED400,1)

ac2<-acf(x = na.omit(DlogCMIXED400),plot = T,type = "partial",lag.max = 120)

which(abs(ac2$acf)>=(2/sqrt(nrow(dat2))))# 1,6

adf.test(x = DlogCMIXED400,nlag = 120) #stationary

#adding the variables

dat2$DlogCMIXED400<-DlogCMIXED400
dat2$DlogCMIXED400_1<-lag(DlogCMIXED400)
dat2$DlogCMIXED400_6<-lag(DlogCMIXED400,6)
#Building model with trend

fit1<-dat2%>%lm(formula = DlogCMIXED400~Date+DlogCMIXED400_1+DlogCMIXED400_6)
summary(fit1)

#Removing trend 


fit2<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_1+DlogCMIXED400_6)
summary(fit2)

#Now i will make ADL model by adding C_MIXED_Balady_300

#is it stationary ? adding log and test

dat2$logCMIXED300<-log(dat2$C_Mixed_Balady_300)

adf.test(x = dat2$logCMIXED300) #No

#get the difference

dat2$DlogCMIXED300<-dat2$logCMIXED300-lag(dat2$logCMIXED300,1)

adf.test(x = dat2$DlogCMIXED300) #good

#Guissing the lags

ac3<-ccf(y = na.omit(dat2$DlogCMIXED400),x=na.omit(dat2$DlogCMIXED300),type = "correlation",plot = T)

ac3$lag[which(abs(ac3$acf)>=(2/sqrt(nrow(dat2))))]

#adding the lead and lag variables

dat2<-dat2%>%mutate(DlogCMIXED300_1=lag(DlogCMIXED300,1),
                    DlogCMIXED300_1L=lead(DlogCMIXED300,1))
dat2$DlogCMIXED300_23<-lag(dat2$DlogCMIXED300,23)
summary(dat2)

#fitting the new vaiable 

fit3<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_1+DlogCMIXED400_6+DlogCMIXED300_1+DlogCMIXED300_1L+DlogCMIXED300_23+DlogCMIXED300)
summary(fit3)# remove DlogCMIXED400_1

fit4<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_6+DlogCMIXED300_1+DlogCMIXED300_1L+DlogCMIXED300_23+DlogCMIXED300)
summary(fit4)# removing DlogCMIXED400_6

fit5<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_6+DlogCMIXED300_1L+DlogCMIXED300_23+DlogCMIXED300)
summary(fit5)#removing DlogCMIXED400_1

fit6<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_6+DlogCMIXED300_23+DlogCMIXED300)
summary(fit6)# removing DlogCMIXED300_1L

fit7<-dat2%>%lm(formula = DlogCMIXED400~DlogCMIXED400_6+DlogCMIXED300)
summary(fit7) #removing intercept

fit8<-dat2%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED400_6+DlogCMIXED300)
summary(fit8)
#fit 8 is good

#Looking on weight 250

#adding log c_mixed_250

dat2<-dat2%>%mutate(logCMIXED250=log(C_Mixed_Balady_250))

adf.test(dat2$logCMIXED250)#not stationary

dat2<-dat2%>%mutate(DlogCMIXED250=logCMIXED250-lag(logCMIXED250))

#stationarity again

adf.test(dat2$DlogCMIXED250)#good

#guissing the bes lag

ac3<-ccf(y = na.omit(dat2$DlogCMIXED400),x=na.omit(dat2$DlogCMIXED250),type = "correlation",plot = T)

ac3$lag[which(abs(ac3$acf)>=(2/sqrt(nrow(dat2))))] #this say that 400 has a predictive power on 250 and not Vs

#what about 200

#adding log c_mixed_200

dat2<-dat2%>%mutate(logCMIXED200=log(C_Mixed_Balady_200))

adf.test(dat2$logCMIXED200)#not stationary

dat2<-dat2%>%mutate(DlogCMIXED200=logCMIXED200-lag(logCMIXED200))

#stationarity again

adf.test(dat2$DlogCMIXED200)#good

#guissing the bes lag

ac3<-ccf(y = na.omit(dat2$DlogCMIXED400),x=na.omit(dat2$DlogCMIXED200),type = "correlation",plot = T)

ac3$lag[which(abs(ac3$acf)>=(2/sqrt(nrow(dat2))))] #400 has a predictive power on 200 not the opposit


#back to fit8
#test for linearity

resettest(fit8)#need non linear term

fit9<-dat2%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED400_6*DlogCMIXED300+I(DlogCMIXED400_6^2)*I(DlogCMIXED300^2))
summary(fit9) #remove DlogCMIXED400_6^2

fit10<-dat2%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED400_6*DlogCMIXED300+I(DlogCMIXED400_6^2*DlogCMIXED300^2)+I(DlogCMIXED300^2))
summary(fit10)

fit11<-dat2%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED300+I(DlogCMIXED400_6^2*DlogCMIXED300^2)+I(DlogCMIXED300^2)+I(DlogCMIXED400_6*DlogCMIXED300))
summary(fit11)


#Now i will try adding imported animal 

#chow forcast test for reliability

#split

dat2_1<-dat2%>%filter(Date<"2018-12-30")
dat2_2<-dat2%>%filter(Date>="2018-12-30")

#Fitting the split
fit11_1<-dat2_1%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED300+I(DlogCMIXED400_6^2*DlogCMIXED300^2)+I(DlogCMIXED300^2)+I(DlogCMIXED400_6*DlogCMIXED300))
fit11_2<-dat2_2%>%lm(formula = DlogCMIXED400~-1+DlogCMIXED300+I(DlogCMIXED400_6^2*DlogCMIXED300^2)+I(DlogCMIXED300^2)+I(DlogCMIXED400_6*DlogCMIXED300))

#getting residuals
eu1<-as.matrix(fit11_1$residuals)
eu2<-as.matrix(fit11_2$residuals)
er<-as.matrix(fit11$residuals)

#getting SS
s0<-t(er)%*%er
s1<-(t(eu1)%*%eu1)
s2<-(t(eu2)%*%eu2)

#df calculation
k<-length(coef(fit11))
n<-nrow(dat2)
n1<-nrow(dat2_1)
n2<-nrow(dat2_2)

f_forcast<-((s0-s1)/n2)/(s1/(n1-k))
1-pf(q = f_forcast,df1 = n2,df2 = (n1-k))


f_break<-((s0-s1-s2)/k)/((s1+s2)/(n-k))
1-pf(q = f_break,df1 = k,df2 = (n-k))

#Coefs are reliable and constant 

#Uncorrelated errors

ace<-acf(x = fit11$residuals,type = "correlation")
ace$lag[which(abs(ace$acf)>=(2/sqrt(nrow(dat2))))] #uncorrelated error 

