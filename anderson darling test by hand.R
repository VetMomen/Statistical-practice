library(tidyverse)

x<-c(27,25,24,24,22,20,21,22,21,25,24,
     26,25,24,23,22,20,21,19,21,25,24,
     26,25,22,23,22,22,21,19,21,23,21,
     26,24,22,23,22,22,20,19,21,23,21,
     26,24,22,23,21,19,20,18,20,20,18)


#arranging x

dat<-x%>%data.frame()
names(dat)<-"x"

dat<-dat%>%arrange(x)

n<-nrow(dat)

#calculating s

s<-function(y,n){
  #the function is :
  #A^2=-n-s
  #s=sum((((2*i)-1)/n)*(log(F[i])+log(1-F[n+1-i])))
  mu<-mean(y)
  s<-sd(y)
  j<-c()
  f1<-c()
  f2<-c()
  F_1<-c()
  F_2<-c()
  for(i in 1:n){
    j[i]<-(((2*i)-1)/n)
    F_1[i]<-pnorm(q = y[i],mean = mu,sd = s)
    F_2[i]<-pnorm(q = y[n+1-i],mean = mu,sd = s)
    f1[i]<-log(F_1[i])
    f2[i]<-log(1-F_2[i])
  }
  S<-sum(j*(f1+f2))
  A2<-(-1*n)-S
  return(list(A=A2,j=j,f1=f1,f2=f2,F_1=F_1,F_2=F_2))
}

d<-s(y = dat$x,n = nrow(dat))

d$A

