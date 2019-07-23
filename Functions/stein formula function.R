library(tidyverse)


para<-expand.grid(n=2:1000,k=2:30,R2=.1:.9)

stein<-function(n,k,R2){
  P2<-1-(((n-1)/(n-k-1))*((n-2)/(n-k-2))*((n+1)/n)*(1-R2))
  return(P2)
}


stein(n = 100,k = 3,R2 = .76)
