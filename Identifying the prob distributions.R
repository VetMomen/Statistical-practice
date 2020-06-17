library(tidyverse)
library(weibullness)
library(MASS)
library(Temporal)
library(FAdist)
library(PearsonDS)
#Global warming data 
warmdir<-"D:\\Stat\\Data sets\\Math stat data sets"
warm<-read.csv(paste(warmdir,"Global warming data.csv",sep = "\\"))

#use godness of fit tests to test H0: the data follow gamma dist

###########
#Klomogrov#
###########
#arrange the data
#calculate alpha and beta ( the parameters ) from:
#mean = alpha*beta , var = alpha*(beta)^2

alpha<-635.31
beta<-.556

warm<-warm%>%arrange(Point.Barrow)

#add F(xi) and Fn(xi)

warm<-warm%>%mutate(c=cumsum(rep(1,nrow(warm))/nrow(warm)),
                    F0=pgamma(q = Point.Barrow,shape = alpha,scale = beta),
                    diff=abs(c-F0))

D=max(warm$diff)
##########
#anderson#
##########
s<-function(y,n){
        #the function is :
        #A^2=-n-s
        #s=sum((((2*i)-1)/n)*(log(F[i])+log(1-F[n+1-i])))
        alpha<-635.31
        beta<-.556
        j<-c()
        f1<-c()
        f2<-c()
        F_1<-c()
        F_2<-c()
        for(i in 1:n){
                j[i]<-(((2*i)-1)/n)
                F_1[i]<-pgamma(q = y[i],shape = alpha,scale = beta)
                F_2[i]<-pgamma(q = y[n+1-i],shape = alpha,scale = beta)
                f1[i]<-log(F_1[i])
                f2[i]<-log(1-F_2[i])
        }
        S<-sum(j*(f1+f2))
        A2<-(-1*n)-S
        return(list(A=A2,j=j,f1=f1,f2=f2,F_1=F_1,F_2=F_2))
}

###
A<-s(y = warm$Point.Barrow,n = nrow(warm))


###################################
#Chi-sq#
########

#segmenting

seg<-seq(min(warm$Point.Barrow),max(warm$Point.Barrow),by = 7)%>%round(digits = 0)

o<-cut(warm$Point.Barrow,seg)%>%table()%>%data.frame()

set.seed(123)
r<-rgamma(n = nrow(warm),shape = alpha,scale = beta)

E<-cut(r,seg)%>%table()%>%data.frame()

dat<-cbind(o,E)
names(dat)<-c("seg1","o","seg2","E")

Q<-sum((dat$o-dat$E)^2/dat$E)

pchisq(q = Q,df = 5)

####################################
#Another method for chisquare 
####################################

#segmenting

seg<-seq(min(warm$Point.Barrow),max(warm$Point.Barrow),by = 7)%>%round(digits = 0)

o<-cut(warm$Point.Barrow,seg)%>%table()%>%data.frame()

ps<-pgamma(q = seg[-1],shape = alpha,scale = beta)

d<-c()
for(i in 1:length(ps)){
        if(i == 1){d[i]<-ps[i]}
        else{d[i]<-ps[i]-ps[i-1]}
}


E<-nrow(warm)*d

Q<-sum((o$Freq-E)^2/E)

pchisq(q = Q,df = 5)


#pplot

plot(warm$c,warm$F0)

#########################################################3
#########################################################

#Hurican data 

katrinadir<-"D:\\Stat\\Data sets\\Math stat data sets"

katrin<-read.csv(paste(katrinadir,"Hurricane Katrina.csv",sep = "\\"))

#looking at histogram

hist(katrin$Wind..mph.)

#Doing klomogrov

#calculating the parameter alpha and gamma
#using weibullness library
par<-weibull.mle(x = katrin$Wind..mph.,threshold = 0)

#adding F(xi) and Fn(xi) and difference

katrin<-katrin%>%arrange(Wind..mph.)%>%
        mutate(Fx=pweibull(q = Wind..mph.,shape = par$shape,scale = par$scale),
               Fn=cumsum(rep(1,nrow(katrin))/nrow(katrin)),
               diff=abs(Fx-Fn))

D<-max(katrin$diff)
####################################################################
#Anderson
###############
Anderson<-function(y,n){
        #the function is :
        #A^2=-n-s
        #s=sum((((2*i)-1)/n)*(log(F[i])+log(1-F[n+1-i])))
        alpha<-par$shape
        beta<-par$scale
        j<-c()
        f1<-c()
        f2<-c()
        F_1<-c()
        F_2<-c()
        for(i in 1:n){
                j[i]<-(((2*i)-1)/n)
                F_1[i]<-pweibull(q = y[i],shape = alpha,scale = beta)
                F_2[i]<-pweibull(q = y[n+1-i],shape = alpha,scale = beta)
                f1[i]<-log(F_1[i])
                f2[i]<-log(1-F_2[i])
        }
        S<-sum(j*(f1+f2))
        A2<-(-1*n)-S
        return(list(A=A2,j=j,f1=f1,f2=f2,F_1=F_1,F_2=F_2))
}

A<-Anderson(y = katrin$Wind..mph.,n = nrow(katrin))
A$A
#############################################################
#chi-sq

seg<-seq(min(katrin$Wind..mph.),max(katrin$Wind..mph.),by = 10)%>%round(digits = 0)

o<-cut(katrin$Wind..mph.,seg)%>%table()%>%data.frame()

ps<-pweibull(q = seg[-1],shape = par$shape,scale = par$scale)

d<-c()
for(i in 1:length(ps)){
        if(i == 1){d[i]<-ps[i]}
        else{d[i]<-ps[i]-ps[i-1]}
}


E<-nrow(katrin)*d

Q<-sum((o$Freq-E)^2/E)

pchisq(q = Q,df = length(E)-1)


dd<-density(x = katrin$Wind..mph.)
plot(x = dd$x,y = dd$y)
lines(x = dd$x,y = dd$y)

plot(x = katrin$Wind..mph.,y = dweibull(x = katrin$Wind..mph.,shape = par$shape,scale = par$scale))
lines(x = katrin$Wind..mph.,y = dweibull(x = katrin$Wind..mph.,shape = par$shape,scale = par$scale))

plot(x = katrin$Wind..mph.,y = pweibull(q =  katrin$Wind..mph.,shape = par$shape,scale = par$scale))
lines(x = katrin$Wind..mph.,y = pweibull(q =  katrin$Wind..mph.,shape = par$shape,scale = par$scale))

#################################################################
#################################################################

#National Unemployment data 

#read the data 

nationaldir<-"D:\\Stat\\Data sets\\Math stat data sets"

nat<-read.csv(paste(nationaldir,"National Unemployment.csv",sep = "\\"))

em<-empMoments(nat$Annual.Av....)

par<-pearsonFitM(mean = em[1],variance = em[2],skewness = em[3],kurtosis = em[4])

nat<-nat%>%arrange(Annual.Av....)%>%
        mutate(Fx=ppearson(q = Annual.Av....,params = par),
               Fn=cumsum(rep(1,nrow(nat))/nrow(nat)),
               diff=abs(Fx-Fn))

D<-max(abs(nat$diff))

##################################3

Anderson<-function(y,n){
        #the function is :
        #A^2=-n-s
        #s=sum((((2*i)-1)/n)*(log(F[i])+log(1-F[n+1-i])))
        em<-empMoments(nat$Annual.Av....)
        par<-pearsonFitM(mean = em[1],variance = em[2],skewness = em[3],kurtosis = em[4])
        j<-c()
        f1<-c()
        f2<-c()
        F_1<-c()
        F_2<-c()
        for(i in 1:n){
                j[i]<-(((2*i)-1)/n)
                F_1[i]<-ppearson(q = y[i],params = par)
                F_2[i]<-ppearson(q = y[n+1-i],params = par)
                f1[i]<-log(F_1[i])
                f2[i]<-log(1-F_2[i])
        }
        S<-sum(j*(f1+f2))
        A2<-(-1*n)-S
        return(list(A=A2,j=j,f1=f1,f2=f2,F_1=F_1,F_2=F_2))
}

A<-Anderson(y =nat$Annual.Av....,n = nrow(nat))
A$A
####################################################################

#chi-sq

seg<-seq(min(nat$Annual.Av....),max(nat$Annual.Av....),by = 2)%>%round(digits = 0)

o<-cut(nat$Annual.Av....,seg)%>%table()%>%data.frame()

ps<-ppearson(q = seg[-1],params = par)

d<-c()
for(i in 1:length(ps)){
        if(i == 1){d[i]<-ps[i]}
        else{d[i]<-ps[i]-ps[i-1]}
}


E<-nrow(katrin)*d

Q<-sum((o$Freq-E)^2/E)

pchisq(q = Q,df = length(E)-1)

################################################################
#colon cancer
#use godness of fit tests to test H0: the data follow 2 parameters weibull dist

colondir<-"D:\\Stat\\Data sets\\Math stat data sets"
colon<-read.csv(paste(colondir,"seercoloncancerdata.csv",sep = "\\"))

###########
#Klomogrov#
###########


#parameters estimates 


#split male from female 

colonS<-split(x = colon,f = as.factor(colon$SEX))

colonM<-colonS$`1`
colonF<-colonS$`0`

#sampling from mail 25 sample 
set.seed(3244)
m<-sample(x = colonM$CS_SIZE,size = 25)
f<-sample(x = colonF$CS_SIZE,size = 25)

set.seed(32546)
cssize<-sample(c(m,f),size = 50,replace = F)

#parameters estimate

par<-fitdistr(x = cssize,densfun = "weibull")

cssize<-cssize%>%data.frame()

names(cssize)<-"cs"


#add F(xi) and Fn(xi)

cssize<-cssize%>%arrange(cs)%>%
        mutate(c=cumsum(rep(1,nrow(cssize))/nrow(cssize)),
                    F0=pweibull(q = cs,shape = par$estimate[1],scale = par$estimate[2]),
                    diff=abs(c-F0))

D=max(cssize$diff)

ks.test(x = cssize$cs,y = pweibull,par$estimate[1],par$estimate[2])

##########
#anderson#
##########
s<-function(y,n){
        #the function is :
        #A^2=-n-s
        #s=sum((((2*i)-1)/n)*(log(F[i])+log(1-F[n+1-i])))
        alpha<-par$estimate[1]
        beta<-par$estimate[2]
        j<-c()
        f1<-c()
        f2<-c()
        F_1<-c()
        F_2<-c()
        for(i in 1:n){
                j[i]<-(((2*i)-1)/n)
                F_1[i]<-pweibull(q = y[i],shape = alpha,scale = beta)
                F_2[i]<-pgamma(q = y[n+1-i],shape = alpha,scale = beta)
                f1[i]<-log(F_1[i])
                f2[i]<-log(1-F_2[i])
        }
        S<-sum(j*(f1+f2))
        A2<-(-1*n)-S
        return(list(A=A2,j=j,f1=f1,f2=f2,F_1=F_1,F_2=F_2))
}

###
A<-s(y = cssize$cs,n = nrow(cssize))
##########################################

#chi-sq

seg<-seq(min(cssize$cs),max(cssize$cs),by = .2)

o<-cut(cssize$cs,seg)%>%table()%>%data.frame()

ps<-pweibull(q = seg[-1],shape = par$estimate[1],scale = par$estimate[2])

d<-c()
for(i in 1:length(ps)){
        if(i == 1){d[i]<-ps[i]}
        else{d[i]<-ps[i]-ps[i-1]}
}


E<-nrow(cssize)*d

Q<-sum((o$Freq-E)^2/E)

pchisq(q = Q,df = length(E)-1)
