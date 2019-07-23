library(tidyverse)
library(car)

mat<-c(1,1,15,11,1,1,9,7,0,0,0,0,
1,2,19,11,1,2,12,9,1,2,12,6,
1,3,14,13,1,3,9,9,1,3,14,15,
1,4,19,14,1,4,7,8,1,4,6,6,
1,5,14,16,1,5,14,8,1,5,18,16,
2,1,18,13,2,1,8,11,2,1,6,6,
2,2,25,24,2,2,24,23,2,2,26,19,
2,3,29,23,2,3,28,26,0,0,0,0,
2,4,11,14,2,4,14,10,2,4,8,7,
2,5,18,17,2,5,11,13,0,0,0,0,
3,1,11,9,3,1,16,15,0,0,0,0,
3,2,13,11,3,2,10,11,0,0,0,0,
3,3,17,10,3,3,7,9,3,3,7,9,
3,4,15,9,3,4,13,13,3,4,7,7,
3,5,17,12,3,5,13,15,3,5,9,12)%>%matrix(ncol = 12,byrow = T)

set1<-mat[,1:4]
set2<-mat[,5:8]
set3<-mat[,9:12]

data<-rbind(set1,set2,set3)

logi<-which(apply(data,1,function(x){
        sum(x)==0
}))


colnames(data)<-c("FACA","FACB","ATTIT","ACHEIV")

data<-data[-logi,]%>%data.frame()%>%mutate(FACA=factor(FACA),
                                           FACB=factor(FACB))
str(data)

fit<-manova(cbind(ATTIT,ACHEIV)~FACA+FACB+FACA*FACB,data = data)

summary.manova(fit,"Wilks")

summary.aov(fit)

Manova(fit,type=3)
