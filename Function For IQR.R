#function for interquartile range
library(tidyverse)

MoemenIQR<-function(x){
        med<-median(x)
        upper<-x[x>med]
        lower<-x[x<med]
        medupper<-median(upper)
        medlower<-median(lower)
        iqr<-medupper-medlower
        return(IQR=iqr)
}

set.seed(7222)
x<-sample(100:1000,100000,replace = T)
MoemenIQR(x)

IQR(x)

MoemenIQR2<-function(x){
        q<-quantile(x)%>%as.numeric()
        iqr<-q[4]-q[2]
        return(c(IQR=iqr))
}

MoemenIQR2(x)
