#importing the data 

credit<-read_csv('./data sets/CC GENERAL.csv')

#descriping the data

dim(credit)

str(credit)
#removing chr column 
credit<-credit[,-1]


vars<-names(credit)

descr<-function(x){
        mean<-mean(x,na.rm = T)
        median<-median(x,na.rm = T)
        sd<-sd(x,na.rm = T)
        max<-max(x,na.rm = T)
        min<-min(x,na.rm = T)
        lowerlimit<-mean - (3*sd)
        upperlimit<-mean + (3*sd)
        uq<-quantile(x,.95,na.rm = T)
        lq<-quantile(x,.05,na.rm = T)
        na_number<-sum(is.na(x))
        return(c(mean=mean,median=median,sd=sd,max=max,min=min,lower_limit=lowerlimit,
                 upper_limit=upperlimit,uper_quantile=uq,lower_quantile=lq,na_number=na_number))
}


desc<-apply(X = credit,MARGIN = 2,FUN = descr)%>%t()
desc<-data.frame(desc)

#removing NA and outlier 
##outlier

#making function to remove the outlier 
outleirRM<-function(x){meanX<-mean(x,na.rm=T)
sdX<-sd(x,na.rm=T)
upperX<-meanX+3*sdX
lowerX<-meanX-3*sdX
for( i in 1:length(x)){
        if(is.na(x[i])){x[i]}
        else if(x[i]>upperX){
                x[i]<-upperX
        }else{x[i]}
}
for( i in 1:length(x)){
        if(is.na(x[i])){x[i]}
        else if(x[i]<lowerX){
                x[i]<-lowerX
        }else{x[i]}
}
return(x)
}

#applying oulier function on Credit data
credit<-apply(credit,2,outleirRM)

#repeating the discreption 
desc2<-apply(X = credit,MARGIN = 2,FUN = descr)%>%t()
desc2<-data.frame(desc2) #note changing all values

#testing if there are difference 
identical(desc$lower_limit,desc2$lower_limit)
identical(desc$upper_limit,desc2$upper_limit)


##missing Value 
#imputing by KNN Method
library(Hmisc)
?impute

credit<-apply(credit,2,function(x){
        impute(x,median)
})

desc3<-apply(X = credit,MARGIN = 2,FUN = descr)%>%t()
desc3<-data.frame(desc3) #note that there are no longer missing value 

#exploring the data 
credit<-data.frame(credit)

str(credit)

par(mfrow=c(3,6))
apply(credit,2,function(x){
        hist(x,title=names(x))
})
