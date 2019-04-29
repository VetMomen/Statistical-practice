
#Here i will analyze and explore the bnb data in turky 

#loading the libraries 

library(tidyverse)
library(caret)
library(mice)
library(psych)

#importing the data 

bnb<-read_csv(file = "./data sets/AirbnbIstanbul.csv",col_types = "ccccfcnncnnnTnnn")


#looking at the missing values in the data 

Mpercent<-apply(bnb,2,function(x){
        ((is.na(x)%>%sum())/length(x))*100%>%round(digits = 2)
})%>%data.frame()%>%mutate(variables=rownames(apply(bnb,2,function(x){
        ((is.na(x)%>%sum())/length(x))*100%>%round(digits = 2)
})%>%data.frame()))%>%mutate(hundered=rep(100,16))

colnames(Mpercent)<-c("percent missing","variable","all")

Mpercent%>%ggplot(aes(x=variable,y=all))+
        geom_col()+
        geom_col(aes(x=variable,y=`percent missing`),fill="red")+
        geom_label(data=Mpercent,aes(x=variable,label=round(`percent missing`),2))+
        theme(axis.text.x = element_text(angle = 90,vjust=.8,face="bold"))


#i will remove neighbourhood variable 

bnb<-bnb%>%select(-neighbourhood_group)

#then i will use knn to impute the number of reviews per month 
dataimpute<-bnb%>%select(-last_review)%>%mice()%>%complete()%>%data.frame()

bnb<-bnb%>%mutate(reviews_per_month=dataimpute$reviews_per_month)

#To know i have nothing to do with missed dates , so i will use complete obs every time i need it

#Now i want to look at the correlation between all variables 

#Iwill extract the numeric variables 

numv<-bnb%>%select(price,minimum_nights,number_of_reviews,reviews_per_month,calculated_host_listings_count,availability_365)

cor(numv)%>%matrix(nrow = ncol(numv),
                   dimnames = list(names(numv),names(numv)))%>%corPlot(numbers = T)
