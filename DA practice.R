#data for practicind DDA

library(tidyverse)
library(DiscriMiner)
library(MASS)
mat<-c(1,5.8,9.7,8.9,1,10.6,10.9,11,1,8.6,7.2,8.7,
1,4.8,4.6,6.2,1,8.3,10.6,7.8,1,4.6,3.3,4.7,
1,4.8,3.7,6.4,1,6.7,6.0,7.2,1,7.1,8.4,8.4,
1,6.2,3.0,4.3,1,4.2,5.3,4.2,1,6.9,9.7,7.2,
1,5.6,4.1,4.3,1,4.8,3.8,5.3,1,2.9,3.7,4.2,
1,6.1,7.1,8.1,1,12.5,11.2,8.9,1,5.2,9.3,6.2,
1,5.7,10.3,5.5,1,6.0,5.7,5.4,1,5.2,7.7,6.9,
1,7.2,5.8,6.7,1,8.1,7.1,8.1,1,3.3,3.0,4.9,
1,7.6,7.7,6.2,1,7.7,9.7,8.9,
2,2.4,2.1,2.4,2,3.5,1.8,3.9,2,6.7,3.6,5.9,
2,5.3,3.3,6.1,2,5.2,4.1,6.4,2,3.2,2.7,4.0,
2,4.5,4.9,5.7,2,3.9,4.7,4.7,2,4.0,3.6,2.9)%>%matrix(ncol = 4,byrow = T)


colnames(mat)<-c("gp","dv1","dv2","dv3")

data<-mat%>%data.frame()%>%mutate(gp=factor(gp))

QDA<-quaDA(variables = data[,-1],group = data$gp)

############################################################


mat<-c(2.0,2.5,2.5,1.5,3.5,2.5,1.0,2.0,1.0,
1.5,2.0,1.5,1.0,4.5,2.5,1.0,2.0,1.5,
2.0,3.0,2.5,3.0,3.0,3.0,1.5,1.0,1.0,
2.5,4.0,3.0,4.5,4.5,4.5,2.0,2.5,2.0,
1.0,2.0,1.0,1.5,4.5,3.5,2.0,3.0,2.5,
1.5,3.5,2.5,2.5,4.0,3.0,2.5,3.0,2.5,
4.0,3.0,3.0,3.0,4.0,3.5,2.0,2.5,2.5,
3.0,4.0,3.5,4.0,5.0,5.0,1.0,1.0,1.0,
3.5,3.5,3.5,NA,NA,NA,1.0,1.5,1.5,
1.0,1.0,1.0,NA,NA,NA,2.0,3.5,2.5,
1.0,2.5,2.0,NA,NA,NA,NA,NA,NA)%>%matrix(ncol = 9,byrow = T)

g1<-mat[,1:3]%>%data.frame()%>%mutate(gp="gp1")
g2<-mat[,4:6]%>%na.omit()%>%data.frame()%>%mutate(gp="gp2")
g3<-mat[,7:9]%>%na.omit()%>%data.frame()%>%mutate(gp="gp3")

data<-rbind(g1,g2,g3)

names(data)<-c("y1","y2","y3","gp")


#running manova

#outlier

data<-data%>%group_by(gp)%>%mutate(mahal=mahalanobis(x = cbind(y1,y2,y3),center = colMeans(cbind(y1,y2,y3)),cov = cov(cbind(y1,y2,y3))))

cutoff<-qchisq(p = .99,df = 3)

data<-data%>%group_by(gp)%>%mutate(outlier=abs(mahal)>cutoff)

#normality

library(MVN)

mvn(data = data[,1:4],subset = "gp",desc = T,multivariatePlot = T)

#testing MV homo 

library(heplots)

fit<-lm(cbind(y1,y2,y3)~gp,data)
homo<-boxM(fit)
covEllipses(homo)
plot(homo)

#run manova

man<-Manova(fit,type="III",test="Wilks")
summary(man)

summary.aov(fit)

#running DDA

DDA<-desDA(variables = data[,1:3],group = data$gp,covar = "total")


DDA

data<-cbind(data,f1=DDA$scores[,1],f2=DDA$scores[,2])

data%>%ggplot(aes(x = f1,y=f2,col=gp))+
        geom_point()+
        geom_hline(yintercept = 0,lty="dashed")+
        geom_vline(xintercept = 0,lty="dashed")+
        geom_point(data=data.frame(data%>%group_by(gp)%>%summarize(mean(f1),mean(f2))),aes(x=mean.f1.,y=mean.f2.),pch=15,col="black")+
        geom_label(data=data.frame(data%>%group_by(gp)%>%
                                           summarize(mean(f1),mean(f2))),
                   aes(x=mean.f1.,y=mean.f2.,label=gp),size=3)











