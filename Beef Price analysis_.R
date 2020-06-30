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

theme_set(theme_bw())
dat2%>%ggplot(aes(x=Date,C_Mixed_Balady_400))+geom_line()


#splitting the data 

traindat<-dat2%>%filter(Date<"2020-01-01")

#take the log of 500 price

traindat<-traindat%>%mutate(logCMIXED400=log(C_Mixed_Balady_400))

#best lag auto correlation
ac<-acf(x = na.omit(traindat$logCMIXED400),plot = T,type = "partial",lag.max = 120) #lag 1,2,7
which(abs(ac$acf)>=(2/sqrt(nrow(traindat))))

#testing stationarity

adf.test(x = traindat$logCMIXED400,nlag = 120) #non stationary

#getting first difference 

traindat$DlogCMIXED400<-traindat$logCMIXED400-lag(traindat$logCMIXED400,1)

ac2<-acf(x = na.omit(traindat$DlogCMIXED400),plot = T,type = "partial",lag.max = 120)

which(abs(ac2$acf)>=(2/sqrt(nrow(traindat))))# 1,6

adf.test(x = traindat$DlogCMIXED400,nlag = 120) #stationary


#take another difference 

traindat$DlogCMIXED400_2<-traindat$DlogCMIXED400-lag(traindat$DlogCMIXED400,1)
ac2<-acf(x = na.omit(traindat$DlogCMIXED400_2),plot = T,type = "partial",lag.max = 120)

lgs<-which(abs(ac2$acf)>=(2/sqrt(nrow(traindat))))

print(lgs)

adf.test(x = traindat$DlogCMIXED400_2,nlag = 120) #stationary

#adding the lags 
lags<-list()
for(i in lgs){
        lags[[i]]<-lag(x = traindat$DlogCMIXED400_2,n = i)
}

#rename it 

names(lags)<-paste0("DlogCMIXED400_2","_",1:88)

#adding it to the data 

traindat<-bind_cols(traindat,lags)


#Fitting the first model

model<-paste("DlogCMIXED400_2",lgs,sep = "_")

model<-paste(list(model))

model<-str_remove_all(string = model,pattern = "(\\()")
model<-str_remove_all(string = model,pattern = '\\"')
model<-str_remove_all(string = model,pattern = '\\,')
model<-str_remove_all(string = model,pattern = '\\)')
model<-str_remove(string = model,pattern = 'c')
model<-str_remove_all(string = model,pattern = '\\n')
model<-str_replace_all(string = model,pattern = ' ',replacement = "+")


model<-paste("DlogCMIXED400_2",model,sep = "~")


fit1<-lm(formula = model,data = traindat)
summary(fit1) #wow


#looking at the c_mix_300

traindat$logCMIXED300<-log(traindat$C_Mixed_Balady_300)

#stitionarity

adf.test(x = traindat$logCMIXED300,nlag = 120)

#adding delta 

traindat$DlogCMIXED300<-traindat$logCMIXED300-lag(traindat$logCMIXED300,1)

adf.test(x = traindat$DlogCMIXED300,nlag = 120)

#another delta 

traindat$DlogCMIXED300_2<-traindat$DlogCMIXED300-lag(traindat$DlogCMIXED300,1)
adf.test(x = traindat$DlogCMIXED300_2,nlag = 120)#stationary till lag 60 




ac3<-ccf(y = na.omit(traindat$DlogCMIXED400_2),x=na.omit(traindat$DlogCMIXED300_2),type = "correlation",plot = T,lag.max = 60)


lgs2<-ac3$lag[which(abs(ac3$acf)>=(2/sqrt(nrow(traindat))))]
lgs2<-lgs2[which(lgs2>0)]

lags2<-list()
for(i in lgs2){
        lags2[[i]]<-lag(x = traindat$DlogCMIXED400_2,n = i)
}

#rename it 

names(lags2)<-paste0("DlogCMIXED300_2","_",1:52)

#adding it to the data 

traindat<-bind_cols(traindat,lags2)


#Fitting the first model

model2<-paste("DlogCMIXED300_2",lgs2,sep = "_")

model2<-paste(list(model2))

model2<-str_remove_all(string = model2,pattern = "(\\()")
model2<-str_remove_all(string = model2,pattern = '\\"')
model2<-str_remove_all(string = model2,pattern = '\\,')
model2<-str_remove_all(string = model2,pattern = '\\)')
model2<-str_remove(string = model2,pattern = 'c')
model2<-str_remove_all(string = model2,pattern = '\\n')
model2<-str_replace_all(string = model2,pattern = ' ',replacement = "+")


model2<-paste(model,model2,"DlogCMIXED300_2",sep = "+")


fit2<-lm(formula = model2,data = traindat)
summary(fit2)

fit3<-lm(formula = DlogCMIXED400_2~DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED400_2_88+DlogCMIXED300_2_2+DlogCMIXED300_2_47+DlogCMIXED300_2_51+DlogCMIXED300_2_52+DlogCMIXED300_2,data = traindat)
summary(fit3)


fit4<-lm(formula = DlogCMIXED400_2~DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED400_2_88+DlogCMIXED300_2_47+DlogCMIXED300_2_52+DlogCMIXED300_2,data = traindat)
summary(fit4)

fit5<-lm(formula = DlogCMIXED400_2~DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED400_2_88+DlogCMIXED300_2_47+DlogCMIXED300_2_51+DlogCMIXED300_2,data = traindat)
summary(fit5)

fit6<-lm(formula = DlogCMIXED400_2~DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED400_2_88+DlogCMIXED300_2_47+DlogCMIXED300_2,data = traindat)
summary(fit6)

fit7<-lm(formula = DlogCMIXED400_2~-1+DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED400_2_88+DlogCMIXED300_2_47+DlogCMIXED300_2,data = traindat)
summary(fit7)

fit8<-lm(formula = DlogCMIXED400_2~-1+DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED400_2_13+DlogCMIXED300_2_47+DlogCMIXED300_2,data = traindat)
summary(fit8)


fit9<-lm(formula = DlogCMIXED400_2~-1+DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED400_2_12+DlogCMIXED300_2_47+DlogCMIXED300_2,data = traindat)
summary(fit9)

fit10<-lm(formula = DlogCMIXED400_2~-1+DlogCMIXED400_2_1+DlogCMIXED400_2_2+DlogCMIXED400_2_3+DlogCMIXED400_2_4+DlogCMIXED400_2_5+DlogCMIXED400_2_6+DlogCMIXED400_2_7+DlogCMIXED400_2_8+DlogCMIXED400_2_9+DlogCMIXED400_2_10+DlogCMIXED400_2_11+DlogCMIXED300_2_47+DlogCMIXED300_2,data = traindat)
summary(fit10)

