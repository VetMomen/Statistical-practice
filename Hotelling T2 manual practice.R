library(tidyverse)
library(car)
library(Hotelling)

t1<-matrix(c(1,2,3,5,2,9,3,4,4,5),ncol=2,byrow=F)
colnames(t1)<-c("y1","y2")

t1mean<-colMeans(t1)%>%matrix%>%t()
colnames(t1mean)<-c("y1","y2")

t1meanmat<-rbind(t1mean[rep(1,5),])%>%matrix(,ncol=2)

meandevt1<-t1-t1meanmat

transposedmeandvt1<-t(meandevt1)
sscpt1<-transposedmeandvt1%*%meandevt1

St1<-sscpt1/(5-1)
##############################3

t2<-matrix(c(4,5,6,8,6,7),ncol=2,byrow=F)
colnames(t2)<-c("y1","y2")

t2mean<-colMeans(t2)%>%matrix()%>%t()
t2meanmat<-rbind(t2mean[rep(1,3),])%>%matrix(ncol=2)

meandevt2<-t2-t2meanmat

transposedmeandvt2<-t(meandevt2)

sscpt2<-transposedmeandvt2%*%meandevt2

St2<-sscpt2/(3-1)


##########################################3

#pooled,covariance

pooledCov<-(sscpt1+sscpt2)/(5+3-2)
meandiffmat<-t1mean-t2mean

T2<-((5*3)/(5+3))*((meandiffmat)%*%solve(pooledCov)%*%t(meandiffmat))
FT2<-((5+3-2-1)/((5+3-2)*2))*T2

library(pwr)

pwr.t.test(d=.6,power=.7,type="two.sample",sig.level=.1)


######################################3


dat<-c(1,4.2,4.1,3.2,4.2,2.8,3.5,
       1,4.1,4.1,3.7,3.9,3.1,3.2,
       1,4.9,4.7,4.7,5.0,2.9,4.5,
       1,4.4,4.1,4.1,3.5,2.8,4.0,
       1,3.7,2.0,2.4,3.4,2.8,2.3,
       1,3.9,3.2,2.7,3.1,2.7,3.6,
       1,3.8,3.5,3.4,4.0,2.7,3.2,
       1,4.2,4.1,4.1,4.2,3.7,2.8,
       1,3.6,3.8,4.2,3.4,4.2,3.0,
       1,2.6,3.2,1.9,3.5,3.7,3.1,
       1,3.0,2.5,2.9,3.2,3.3,3.1,
       1,2.9,3.3,3.5,3.1,3.6,3.4,
       2,2.1,1.8,1.7,1.7,2.8,1.5,
       2,4.8,4.0,3.5,1.8,3.1,2.2,
       2,4.2,2.9,4.0,1.8,3.1,2.2,
       2,3.7,1.9,1.7,1.6,3.1,1.6,
       2,3.7,2.1,2.2,3.1,2.8,1.7,
       2,3.8,2.1,3.0,3.3,3.0,1.7,
       2,2.1,2.0,2.2,1.8,2.6,1.5,
       2,2.2,1.9,2.2,3.4,4.2,2.7,
       2,3.3,3.6,2.3,4.3,4.0,3.8,
       2,2.6,1.5,1.3,2.5,3.5,1.9,
       2,2.5,1.7,1.7,2.8,3.3,3.1)%>%matrix(ncol=7,byrow=T)

colnames(dat)<-c("GP","INT","TONE","RHY","INTON","TEM","ARTIC")


dat<-dat%>%data.frame()%>%mutate(GP=factor(GP))

fit<-lm(cbind(INT,TONE,RHY,INTON,TEM,ARTIC) ~ GP,data = dat)
summary(fit)

coef(fit)

Anova(fit,type = "III")

linearHypothesis(model = fit,hypothesis.matrix = "GP2 = 0")
################3

hot<-hotelling.test(.~GP,dat)
hot$stats
hot$pval
hot$pval/hot$stats$p

dat%>%group_by(GP)%>%summarize(n())
n1<-hot$stats$nx
n2<-hot$stats$ny

D2<-((n1+n2)/(n1*n2))*hot$stats$statistic

Fstat<-(((nrow(dat)-ncol(dat[,-1]))/(((nrow(dat)-2))*ncol(dat[,-1]))))*hot$stats$statistic


D2_2<-mahalanobis(cov = cov(dat[,-1]),center = colMeans(dat[,-1]),x = dat[,-1])

mean(D2_2)


###########################################################3
#other example


#the data

data<-c(1,1.00,5.80,9.70,8.90,
  2,1.00,10.60,10.90,11.00,
  3,1.00,8.60,7.20,8.70,
  4,1.00,4.80,4.60,6.20,
  5,1.00,8.30,10.60,7.80,
  6,1.00,4.60,3.30,4.70,
  7,1.00,4.80,3.70,6.40,
  8,1.00,6.70,6.00,7.20,
  9,1.00,6.90,9.70,7.20,
  10,1.00,5.60,4.10,4.30,
  11,1.00,4.80,3.80,5.30,
  12,1.00,2.90,3.70,4.20,
  13,2.00,2.40,2.10,2.40,
  14,2.00,3.50,1.80,3.90,
  15,2.00,6.70,3.60,5.90,
  16,2.00,5.30,3.30,6.10,
  17,2.00,5.20,4.10,6.40,
  18,2.00,3.20,2.70,4.00,
  19,2.00,4.50,4.90,5.70,
  20,2.00,3.90,4.70,4.70,
  21,2.00,4.00,3.60,2.90,
  22,2.00,5.70,5.50,6.20,
  23,2.00,2.40,2.90,3.20,
  24,2.00,2.70,2.60,4.10)%>%matrix(ncol = 5,byrow = T)%>%data.frame()

names(data)<-c("idx","GP", "WI", "WC", "PC")

data<-data%>%select(-idx)%>%mutate(GP=factor(GP))

#separate the two groups

gp1<-data%>%filter(GP==1)%>%select(-GP)
gp2<-data%>%filter(GP==2)%>%select(-GP)

#calculating the mean of each group

mean1<-colMeans(gp1)%>%as.matrix()%>%t()
mean2<-colMeans(gp2)%>%as.matrix()%>%t()

#converting the mean into matrix

meangp1<-rbind(rep(mean1[1,],nrow(gp1)))%>%matrix(nrow = nrow(gp1),byrow = T)
meangp2<-rbind(rep(mean2[1,],nrow(gp2)))%>%matrix(nrow = nrow(gp2),byrow = T)

#calculating the deviation from mean in each group

dev1<-gp1-meangp1%>%matrix(ncol = 3)
dev2<-gp2-meangp2%>%matrix(ncol = 3)

#getting the transposed matrix of each deviation matrix

transdev1<-t(dev1)%>%matrix(nrow = ncol(dev1))
transdev2<-t(dev2)%>%matrix(nrow = ncol(dev2))

#calculating SSCP matrix of each group

sscpgp1<-as.matrix(transdev1)%*%as.matrix(dev1)
sscpgp2<-as.matrix(transdev2)%*%as.matrix(dev2)

#calculating the covariance matrix

S<-(sscpgp1+sscpgp2)/(12+12-2)

#the inverse of cov matrix

Sinv<-solve(S)%>%as.matrix()

#the difference bet groups in mean

meandif<-mean1-mean2%>%as.matrix()

#calculating T2
T2<-((12*12)/(12+12))*(meandif%*%Sinv%*%t(meandif))


#calculating F from T2

N<-nrow(data)
P<-ncol(gp1)

Fvalue<-((N-P-1)/((N-2)*P))*T2

D2<-(N*T2)/(nrow(gp1)*nrow(gp2))

P.value<-1-pf(q = 3.31,df1 = P,df2 = (N-1-P))

