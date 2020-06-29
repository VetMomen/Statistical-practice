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


                                                                                      
                                                                                      
                                                                                      
                                                                                       
#Converting the weights into variables
#merging weight and type

dat2<-dat%>%mutate(Type_w=paste(Type,Weight,sep = "_"))%>%select(-c(Type,Weight))
dat2<-dat2%>%group_by(Type_w,Date)%>%mutate(id=1:n())%>%pivot_wider(names_from = Type_w,values_from=Price)

#checking the data

str(dat2)

summary(dat2)

#knowing the percent of NA

sapply(dat2,function(x){
        (sum(is.na(x))/length(x)%>%round(digits = 2))*100
})


#i have a 3 problematic variables will be excluded temporarly from the analysis 


#plotting 500 
theme_set(theme_bw())

dat2%>%ggplot(aes(x=Date,C_Mixed_Balady_400))+geom_line()+geom_smooth()

#making a training data set

set.seed(6876)
idx<-sample(1:nrow(dat2),.75*nrow(dat2),replace = F)

train<-dat2[idx,]

#adding log of c_mixed_400
dat2<-dat2%>%mutate(logCMIXED400=log(C_Mixed_Balady_400))
train$logCMIXED400<-dat2$logCMIXED400[idx]


#guissing the best lag

ac1<-acf(x = na.omit(train$logCMIXED400),type = "partial") #There is no one lag suitable for time series 

#looking at the difference 
DlogCMIXED400<-dat2$logCMIXED400-lag(dat2$logCMIXED400)
dat2$DlogCMIXED400<-DlogCMIXED400
train$DlogCMIXED400<-dat2$DlogCMIXED400[idx]

#guissing the best lag
ac2<-acf(x = na.omit(train$DlogCMIXED400),type = "partial") #There is no one lag suitable for time series 
which(abs(ac2$acf)>=(2/sqrt(nrow(train))))# 15

adf.test(x = train$DlogCMIXED400) #stationary

#adding lag 15

dat2$DlogCMIXED400_15<-lag(dat2$DlogCMIXED400,15)
train$DlogCMIXED400_15<-dat2$DlogCMIXED400_15[idx]

#fitting the first model with trend 

fit1<-train%>%lm(formula = DlogCMIXED400~Date+DlogCMIXED400_15)
summary(fit1)

#removing trend
fit2<-train%>%lm(formula = DlogCMIXED400~DlogCMIXED400_15)
summary(fit2)
