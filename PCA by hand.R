dir<-"/home/debian/Statistical-practice/data sets/Table F4.: Movie Buzz Data (62 Observations) .csv"

library(tidyverse);library(multiColl);library(psych)
dat<-read.csv(dir)

#Take the numerical variable

dat2<-dat[,c("MPRATING","BUDGET","STARPOWR","ADDICT","CMNGSOON","FANDANGO","CNTWAIT3")]%>%as.matrix()

Z<-scale(dat2,center = T,scale = T)%>%as.matrix()

cormat<-(t(Z)%*%Z)*(1/(nrow(dat2-1)))

ei<-eigen(cormat)

eg<-ei$values

info<-eg/ncol(Z)

cumsum(info)

C<-ei$vectors

Cinfo<-C[,1:6]
#multiplying C*dat2

Tdat<-Z%*%Cinfo

pa<-principal(r = cormat,nfactors = 6)
pa$values
