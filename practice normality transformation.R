library(tidyverse)
library(MVN)
library(ggplot2)
library(car)

mat<-c(285, 325, 165 ,168 ,190, 160,
  23, 45, 15, 277, 230 ,63,
  40,85,18,153,80,29,
215,307,60,306,440,105,
110,110,50,252,350,175,
65,105,24,143,205,42,
43,160,44,69,55,10,
120,180,80,177,195,75,
250,335,185,73,57,32,
14,20,3,81,120,7,
0,15,5,63,63,0,
5,23,12,64,53,35,
75,303,95,88,125,21,
27,113,40,132,225,9,
30,25,28,122,60,38,
183,175,100,309,355,135,
47,117,46,147,135,83,
385,520,23,223,300,30,
83,95,26,217,235,130,
87,27,2,74,67,20,
0,0,0,258,185,115,
0,0,0,239,445,145,
0,0,0,78,40,48,
0,0,0,70,50,55,
0,0,0,188,165,87,
0,0,0,157,330,67)%>%matrix(ncol = 6,byrow = T)

gp1<-mat[1:20,1:3]%>%data.frame()%>%mutate(gp=1)
gp2<-mat[,4:6]%>%data.frame()%>%mutate(gp=2)

data<-rbind(gp1,gp2)%>%mutate(gp=factor(gp))

mvn(data[,-4])

apply(data[,-4],2,function(x){
        scale(x,scale = T,center = T)
})%>%apply(MARGIN = 2,FUN = function(x){
        abs(x)>2.5
})%>%apply(MARGIN = 2,FUN = which)

data<-data[-18,]

mah<-mahalanobis(data[,-4],center = colMeans(data[,-4]),cov = cov(data[,-4]))
cut<-qchisq(p = .001,df = 3)

which(mah<=cut)

mvn(data[,-4])

data%>%ggplot(aes(X1))+geom_density()
data%>%ggplot(aes(X2))+geom_density()
data%>%ggplot(aes(X3))+geom_density()

transdata<-apply(data[,-4],MARGIN = 2,FUN = sqrt)%>%data.frame()%>%mutate(gp=data$gp)


mvn(transdata[,-4])

lm(cbind(X1,X2,X3)~gp-1,data)%>%linearHypothesis(hypothesis.matrix=c("gp1=gp2"))
lm(cbind(X1,X2,X3)~gp-1,transdata)%>%linearHypothesis(hypothesis.matrix=c("gp1=gp2"))


