library(tidyverse)


x<-c(-1,0,2,-2,5,6,8,11,12,-3)
y<-c(-5,-4,2,-7,6,9,13,21,20,-9)

xbar<-mean(x)
ybar<-mean(y)

sxx<-sum((x-xbar)^2)
sxy<-sum((x-xbar)*(y-ybar))
syy<-sum((y-ybar)^2)

beta1<-sxy/sxx
beta0<-ybar-(beta1*xbar)
################################################
x<-c(63,70,74,82,60,44,80,71,71,41)
y<-c(151,149,164,157,144,130,157,160,121,125)

xbar<-mean(x)
ybar<-mean(y)

sxx<-sum((x-xbar)^2)
syy<-sum((y-ybar)^2)
sxy<-sum((x-xbar)*(y-ybar))

beta1<-sxy/sxx
beta0<-ybar-(beta1*xbar)

regfunction<-function(x,y,beta1,beta0){
        yhat<-beta0+(beta1*x)
        res<-y-yhat
        return(list(yhat=yhat,res=res))
}

fit<-regfunction(x,y,beta1,beta0)






dat<-data.frame(x=x,y=y,yhat=fit$yhat,res=fit$res)

fit2<-lm(dat$y~dat$x)
pr<-predict(fit2,interval = "confidence",level = .9)%>%data.frame()

dat<-data.frame(x=x,y=y,yhat=fit$yhat,res=fit$res,upper=pr$upr,lower=pr$lwr)



dat%>%ggplot(aes(x=x,y=y))+
        geom_point()+
        geom_line(data=dat,aes(x=x,y=yhat))+
        geom_line(data=dat,aes(x=x,y=upper),lty="dashed")+
        geom_line(data=dat,aes(x=x,y=lower),lty="dashed")

meanE<-mean(dat$res)
see<-sum((dat$y-dat$yhat)^2)
mse<-sum((dat$y-dat$yhat)^2)/(nrow(dat)-2)

R2<-(sum((dat$y-mean(dat$y))^2)-see)/sum((dat$y-mean(dat$y))^2)

yhat<-beta0+beta1*x


################

#inference 

tb<-beta1/sqrt(mse/sxx)
1-pt(q = tb,df = (nrow(dat)-2))

tb0<-beta0/(mse*((1/nrow(dat))+(xbar^2/sxx)))^(1/2)
1-pt(q = tb0,df = nrow(dat)-2)


summary.aov(fit2)

##########################################################

#confidence Interval of prediction

ci<-function(y,yhat,n,x,xbar,sx){
        sy<-sqrt((sum((y-yhat)^2)) / (n-2))
        ci<-list(upper=yhat+(qt(p = .9,df = n-2)*sy*sqrt((1/n)+(((x-xbar)^2))))/((n-1)*sx^2),
lower=yhat-(qt(p = .9,df = n-2)*sy*sqrt((1/n)+(((x-xbar)^2)/((n-1)*sx^2)))))
        return(ci)
}

ci(y = y,yhat = yhat,n = nrow(dat),x = x,xbar = xbar,sx = sd(x))
pr$upr

##########################################################

#Correlation

cortest<-function(sxx,syy,sxy,n){
        r<-sxy/sqrt(sxx*syy)
        zr<-(1/2)*log((1+r)/(1-r))
        zrho<-(1/2)*log((1+0)/(1-0))
        sr<-1/sqrt(n-3)
        z<-(zr-zrho)/sr
        pvalue<-(pnorm(q = z))/2
        return(list(r=r,zr=zr,sr=sr,z=z,pvalue=pvalue))
}
cortest(sxx = sxx,syy = syy,sxy = sxy,n = 10)


##########################################################3


#multivariate regression


all<-c(100,1,1,
80,5,1,
104,5,2,
94,10,2,
130,20,3)

yi=seq(1,15,3)

y=all[yi]

x1i=seq(2,15,3)

x1=all[x1i]

x2i=seq(3,15,3)

x2=all[x2i]

dat<-data.frame(y=y,ones=rep(1,length(y)),x1=x1,x2=x2)


X<-as.matrix(x = dat[,2:4])
Y<-as.matrix(dat$y)

#calculate X'X

XX<-t(X)%*%X

#calculate X'y

XY=t(X)%*%Y

betas<-solve(XX)%*%XY
yhat<-X%*%betas
