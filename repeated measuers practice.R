#,practicing,repated,measures,

library(tidyverse)
library(car)
library(ez)

mat<-c(1,30,28,16,34,
2,14,18,10,22,
3,24,20,18,30,
4,38,34,20,44,
5,26,28,14,30)%>%matrix(ncol=5,byrow=T)

colnames(mat)<-c("id","dg1","dg2","dg3","dg4")

dgmean<-mat[,-1]%>%colMeans()

idmean<-mat[,-1]%>%rowMeans()

gmean<-mat[,-1]%>%mean


data<-mat%>%data.frame()%>%mutate(id=factor(id))

#one,way,anova,(by,hand)
#calculating,between,SS

SSw<-((data$dg1-dgmean[1])^2+(data$dg2-dgmean[2])^2+(data$dg3-dgmean[3])^2+(data$dg4-dgmean[4])^2)%>%sum()

ss<-function(x,gmean)(
(mean(x)-mean(gmean))^2
)

SSb<-apply(data[,-1],MARGIN=2,function(x){
(mean(x)-gmean)^2
})%>%sum*5


f<-(SSb/3)/(SSw/16)

pf(q=f,df1=3,df2=16,lower.tail=F)


#repeated,measures,(by,hand)

#SSb,is,the,same,of,SSb,but,instead,of,number,of,participants,,the,number,of,measures,and,instead,of,substracting,grand,mean,from,g,mean,,substracting,it,from,participants,mean,

ssblock<-apply(X=data[,-1],1,function(x){
ss(x=x,gmean=gmean)
})%>%sum()*4

SSe<-SSw-ssblock

mse<-SSe/((nrow(data)-1)*(ncol(data[,-1])-1))


f2<-(SSb/3)/(mse)

pf(f2,df1=3,df2=12,lower.tail=F)


########################################################3
#multivariate,t,test,(by,hand,)-->,the,multivariables,is,the,difference,between,measures,

#calculating,the,K-1,difference,

meandiff<-function(x){
meandif<-c()
sddif<-c()
dif2<-list()
for(i in 2:ncol(x)){
dif<-x[,i]-x[,i-1]
dif2[[i-1]]<-dif
meandif[i-1]<-mean(dif)
sddif[i-1]<-var(dif)
}
datdif<-data.frame(dif2)
covar<-cov(datdif)
dimnames(covar)<-list(c(1:ncol(covar)),c(1:ncol(covar)))
return(list(mean=meandif,var=sddif,covar=covar))
}


mean_var_cov<-meandiff(data[,-1])

T2<-function(n,meanv,covmat){
n*(t(meanv)%*%solve(covmat)%*%meanv)
}

T2<-T2(n=nrow(data),meanv=mean_var_cov$mean,covmat=mean_var_cov$covar)

Fappr<-function(n,k,T2){
f<-((n-k+1)/((n-1)*(k-1)))*T2
f<-c(f)
}

f3<-Fappr(n=nrow(data),k=ncol(data[,-1]),T2=T2)


##############################################
#one,way,by,computer,

#gathering,the,data,

dataL<-gather(data=data,key="measure",value="score",-id)

anov<-dataL%>%with(aov(score~measure,data))
summary(anov)

#repeated,by,computer
anov2<-aov(score~measure+Error(id),dataL)

summary(anov2)

###############################################

#multivariate,by,computer,
#calculating,the,diff,

man2<-manova(cbind(dg1,dg2,dg3,dg4)~1,data)

Anova(man2)

idata<-c("dg1","dg2","dg3","dg4")%>%data.frame()

names(idata)<-c("test")

manov<-Manova(man2,idata=idata,idesign=~test,type="III")

summary(manov)


pairt<-pairwise.t.test(x=dataL$score,g=dataL$measure,p.adjust.method="bonferroni",paired=T)

pairt

####################################

#trend,analysis,

mat<-c(26,20,18,11,10,
34,35,29,22,23,
41,37,25,18,15,
29,28,22,15,13,
35,34,27,21,17,
28,22,17,14,10,
38,34,28,25,22,
43,37,30,27,25,
42,38,26,20,15,
31,27,21,18,13,
45,40,33,25,18,
29,25,17,13,8,
39,32,28,22,18,
33,30,24,18,7,
34,30,25,24,23,
37,31,25,22,20)%>%matrix(ncol=5,byrow=T)

colnames(mat)<-c(paste0("y",1:5))

data<-data.frame(mat)
data$id<-1:nrow(data)

dataL<-gather(data,"measure","score",-id)

dataL$measure<-ordered(dataL$measure)

aov(score~measure+Error((id:measure)/measure),dataL,contrasts="contr.poly")%>%summary

ezANOVA(data=dataL,dv=score,wid=id,within=measure)

mlva<-manova(cbind(y1,y2,y3,y4,y5)~1,data)

idata<-ordered(names(data[,-6]))%>%data.frame()
names(idata)<-c("date")


Manova(mod=mlva,idata=idata,idesign=~date,
type="III",univariate=T,multivariate=F)%>%
summary()



###################################################

#multivariate,repeated,measures

mat<-c(62,67,72,66,67,35,
66,66,96,87,74,63,
70,74,69,73,85,63,
85,99,99,71,91,60,
78,62,79,69,54,65,
85,99,99,75,66,61,
82,83,69,99,63,66,
55,61,52,74,55,67,
91,99,99,99,99,87,
95,87,99,96,82,82,
87,91,87,82,98,85,
96,99,96,76,74,61,
54,60,69,80,66,71,
69,60,87,80,69,71,
87,87,88,99,95,82,
78,72,66,76,52,74,
72,58,74,69,59,58)%>%matrix(ncol=6,byrow=T)

colnames(mat)<-c("read1","read2","lang1","lang2","math1","math2")

data<-data.frame(mat)
data$id<-1:nrow(data)


mano<-manova(cbind(read1,read2,lang1,lang2,math1,math2)~1,data)

measure<-rep(c("read","lang","math"),each=2)
repp<-rep(c(1,2),3)%>%ordered()

idata<-data.frame(measure,repp)

Ma<-Manova(mano,type="III",idata=idata,idesign=~measure*repp)%>%summary()

#########################3
#two,way,rm,manova,(,between,and,within,supjects)

mat<-c(1,4,3,3,
1,4,4,3,
1,4,3,1,
1,3,2,1,
1,5,3,2,
1,6,5,4,
1,6,5,4,
1,5,4,1,
1,3,3,2,
1,5,4,1,
1,4,2,2,
1,5,2,1,
2,6,3,2,
2,5,4,1,
2,7,6,3,
2,6,4,2,
2,3,2,1,
2,5,5,4,
2,4,3,1,
2,4,2,1,
2,6,5,3,
2,7,6,4,
2,4,3,2,
2,7,4,3,
3,8,4,2,
3,3,6,3,
3,7,7,4,
3,4,7,1,
3,9,7,3,
3,2,4,1,
3,3,5,1,
3,6,5,2,
3,6,6,3,
3,9,5,2,
3,7,9,4,
3,8,6,1)%>%matrix(ncol=4,byrow=T)

colnames(mat)<-c("diet","wt1","wt2","wt3")

data<-mat%>%data.frame()%>%
group_by(diet)%>%mutate(id=1:n())%>%ungroup(diet)%>%mutate(diet=factor(diet))

str(data)

man1<-data%>%with(manova(cbind(wt1,wt2,wt3)~diet))


idata<-data.frame(wt=ordered(c("wt1","wt2","wt3")))

Manova(man1,type=3,idata=idata,idesign=~wt)%>%summary()


dataL<-gather(data,"wt","score",-c(diet,id))%>%data.frame()%>%mutate(wt=factor(wt))

x<-ezANOVA(data=dataL,dv=score,wid=id,within=wt,between=diet,return_aov=T)


means<-dataL%>%group_by(diet,wt)%>%summarize(intrmean=mean(score))

means%>%ggplot(aes(x=wt,y=intrmean))+
geom_point(size=2.5)+
geom_line(aes(group=diet,col=diet),lwd=1)

d1<-ezANOVA(data=dataL[dataL$wt=="wt1",],dv=score,wid=id,between=diet,return_aov=T)
d2<-ezANOVA(data=dataL[dataL$wt=="wt2",],dv=score,wid=id,between=diet,return_aov=T)
d3<-ezANOVA(data=dataL[dataL$wt=="wt3",],dv=score,wid=id,between=diet,return_aov=T)

pairwise.t.test(x=dataL$score[dataL$wt=="wt2"],g=dataL$diet[dataL$wt=="wt2"],p.adjust.method="bonferroni")

#within,subject,pairwise,comparison,at,each,level,of,diet,

pairwise.t.test(x=dataL$score[dataL$diet==1],g=dataL$wt[dataL$diet==1],p.adjust.method="bonferroni")
pairwise.t.test(x=dataL$score[dataL$diet==2],g=dataL$wt[dataL$diet==2],p.adjust.method="bonferroni")
pairwise.t.test(x=dataL$score[dataL$diet==3],g=dataL$wt[dataL$diet==3],p.adjust.method="bonferroni")



###################################################

#one,between,-,two,within

mat<-c(1,19,22,28,16,26,22,
1,11,19,30,12,18,28,
1,20,24,24,24,22,29,
1,21,25,25,15,10,26,
1,18,24,29,19,26,28,
1,17,23,28,15,23,22,
1,20,23,23,26,21,28,
1,14,20,29,25,29,29,
2,16,20,24,30,34,36,
2,26,26,26,24,30,32,
2,22,27,23,33,36,45,
2,16,18,29,27,26,34,
2,19,21,20,22,22,21,
2,20,25,25,29,29,33,
2,21,22,23,27,26,35,
2,17,20,22,23,26,28)%>%matrix(ncol=7,byrow=T)

colnames(mat)<-c("gp",paste("drug1",1:3,sep="_"),paste("drug2",1:3,sep="_"))

data<-mat%>%data.frame()%>%mutate(gp=factor(gp))

man<-manova(cbind(drug1_1,drug1_2,drug1_3,drug2_1,drug2_2,drug2_3)~gp,data)
summary(man)

drug<-rep(c("drug1","drug2"),each=3)
dose<-ordered(rep(c(1:3),2))

idata<-data.frame(drug,dose)%>%mutate(drug=factor(drug))

Man<-Manova(man,type=3,idata=idata,idesign=~dose+drug*dose,test="Wilks")
summary(Man)

datd1<-data%>%dplyr::select(gp,drug1_1,drug1_2,drug1_3)%>%group_by(gp)%>%mutate(id=1:n())%>%
ungroup()%>%gather(key="dose",value="score",-c(gp,id))

d1anova<-ezANOVA(data=datd1,dv=score,wid=id,between=gp,return_aov=T)



datd2<-data%>%dplyr::select(gp,drug2_1,drug2_2,drug2_3)%>%group_by(gp)%>%mutate(id=1:n())%>%
ungroup()%>%gather(key="dose",value="score",-c(gp,id))

d2anova<-ezANOVA(data=datd2,dv=score,wid=id,between=gp,return_aov=T)


datd2%>%group_by(gp)%>%summarize(mean=mean(score))


means<-data%>%group_by(gp)%>%mutate(id=1:n())%>%ungroup()%>%gather("dose","score",-c(id,gp))%>%
group_by(gp,dose)%>%summarize(mean=mean(score))%>%arrange(dose)%>%separate(col=dose,into=c("drug","dose"))%>%mutate(dose=factor(dose))%>%
group_by(gp,drug,dose)%>%summarize(mean=mean(mean))


means%>%ggplot(aes(x=dose,y=mean,col=drug))+
geom_point()+
geom_line(aes(group=drug))+
facet_wrap(gp~.)+
theme(panel.background=element_blank(),
axis.line=element_line())
#################################################################################

#two,between,and,one,within

mat<-c(1,1,4,3,3,
1,1,4,4,3,
1,1,4,3,1,
1,1,3,2,1,
1,1,5,3,2,
1,1,6,5,4,
1,2,6,5,4,
1,2,5,4,1,
1,2,3,3,2,
1,2,5,4,1,
1,2,4,2,2,
1,2,5,2,1,
2,1,6,3,2,
2,1,5,4,1,
2,1,7,6,3,
2,1,6,4,2,
2,1,3,2,1,
2,1,5,5,4,
2,2,4,3,1,
2,2,4,2,1,
2,2,6,5,3,
2,2,7,6,4,
2,2,4,3,2,
2,2,7,4,3,
3,1,8,4,2,
3,1,3,6,3,
3,1,7,7,4,
3,1,4,7,1,
3,1,9,7,3,
3,1,2,4,1,
3,2,3,5,1,
3,2,6,5,2,
3,2,6,6,3,
3,2,9,5,2,
3,2,7,9,4,
3,2,8,6,1)%>%matrix(ncol=5,byrow=T)


colnames(mat)<-c("gp","age","wt1","wt2","wt3")

data<-mat%>%data.frame()%>%mutate(gp=factor(gp),
age=factor(age,ordered=T))

man1<-manova(cbind(wt1,wt2,wt3)~gp*age,data)
summary(man1)

idata<-data.frame(wt=ordered(c("wt1","wt2","wt3")))

Man1<-Manova(man1,idata=idata,idesign=~wt,type=3)

summary(Man1)

dataL<-data%>%group_by(gp)%>%mutate(id=1:n())%>%
gather("wt","score",-c(id,gp,age))

ezANOVA(data=dataL,dv=score,wid=id,within=wt,between=gp)

pairwise.t.test(x=dataL$score[dataL$gp==1],g=dataL$wt[dataL$gp==1],p.adjust.method="bonferroni")
pairwise.t.test(x=dataL$score[dataL$gp==2],g=dataL$wt[dataL$gp==2],p.adjust.method="bonferroni")

####################################################################################

mat<-c(1,1,2,1,0,2,1,1,-7,-2,-2,-5,1,1,-3,-1,-3,-1,
1,1,1,1,0,-3,1,1,1,-1,-4,-2,1,1,-7,1,-4,-3,
1,2,0,-4,-9,-7,1,2,-1,-9,-9,-4,1,2,-6,-6,3,-4,
1,2,-2,-4,-4,-5,1,2,-2,-1,-3,-3,1,2,-9,-9,-3,1,
2,1,3,4,2,-3,2,1,-1,-1,-3,-3,2,1,2,2,2,0,
2,1,2,0,-2,0,2,1,0,-1,2,2,2,1,3,3,-4,-2,
2,1,-1,2,2,-1,2,1,-3,-2,3,-2,
2,2,-3,-2,5,2,2,2,2,3,-2,-3,2,2,2,4,1,3,
2,2,3,2,-5,-5,2,2,-4,-3,-3,-3,2,2,6,4,-9,-9,
2,2,2,1,-3,0,2,2,-1,-4,-2,0,2,2,-2,-1,2,-2,
2,2,-2,4,-1,0)%>%matrix(ncol = 6,byrow = T)

colnames(mat)<-c("grade","iq","fd1","fd2","fa1","fa2")

data<-mat%>%data.frame()%>%mutate(grade=factor(grade,ordered = T,labels = c("fourth","seventh")),
                                  iq=factor(iq,ordered = T,labels = c("low","high")))

data<-data%>%group_by(grade,iq)%>%mutate(id=1:n())

str(data)

man1<-lm(cbind(fd1,fd2,fa1,fa2)~grade+iq+grade:iq,data)
summary(man1)

fac<-factor(rep(c("fd","fa"),each=2))
time<-ordered(rep(c(1,2),2))

idata<-data.frame(fac,time)

Man1<-Manova(man1,type=3,idata=idata,idesign= ~fac*time)
summary(Man1)

#########################################################################

A<-c(.6,1.3,2.5,2.1,3,1.4,3.8,4.4,4.7,4.5,5.8,4.7,
6.2,6.1,6.1,6.7,3.2,6.6,7.6,8.3,2.5,6.2,8,8.2,
2.8,3.6,4.4,4.3,1.1,1.1,5.7,5.8,2.9,4.9,6.3,6.4,
5.5,4.3,5.6,4.8)%>%matrix(ncol = 4,byrow = T)


colnames(A)<-c("y1","y2","y3","y4")

data<-A%>%data.frame()%>%mutate(id=1:n())

man<-lm(cbind(y1,y2,y3,y4)~1,data,contrasts = "contr.Helmert")

coef(man)

idata<-paste0("y",1:4)%>%factor()%>%data.frame()
names(idata)<-c("time")

Man<-Manova(man,type=3,idata=idata,idesign=~time,icontrast="contr.Helmert")
summary(Man)

####################################################
#profile analysis example 

library(haven)
data <- read_sas("data sets/three_stooges.sas7bdat", 
                          NULL)
data<-data%>%data.frame()%>%mutate(favorite=factor(favorite))

gmean<-data%>%group_by(favorite)%>%summarize(mean(brain_damage),mean(stupidity_index))%>%
        gather("dv","mean",-favorite)

gmean%>%ggplot(aes(x = dv,y = mean,col=favorite))+
                       geom_point(aes(shape=favorite),col="black",lwd=2)+
                       geom_line(aes(group=favorite,lty=favorite))

man<-manova(cbind(brain_damage,stupidity_index)~favorite,data)

idata<-data.frame(depv=factor(c("dv1","dv2")))

Manova(man,type="III",idata=idata,idesign=~depv)%>%summary()


#####################################################3
#excersise1


X<-c(1,5,6,1,
2,3,4,2,
3,3,7,1,
4,6,8,3,
5,6,9,3,
6,4,7,2,
7,5,9,2)%>%matrix(ncol = 4,byrow = T)

colnames(X)<-c("id","y1","y2","y3")

data<-X%>%data.frame()%>%mutate(id=factor(id))

dataL<-data%>%gather("time","score",-id)%>%mutate(time=factor(time,ordered = T))

ezANOVA(data = dataL,dv = score,wid = id,within = time)

pairwise.t.test(x = dataL$score,g = dataL$time,p.adjust.method = "bonferroni",paired = T)

#excersize 2

X<-c(1,41,38,46,35,
2,48,41,47,50,
3,34,33,39,36,
4,31,40,28,38,
5,26,23,35,19,
6,37,31,40,30,
7,44,32,46,45,
8,53,47,58,53,
9,46,41,47,48,
10,34,38,39,39,
11,33,39,36,41,
12,50,45,54,40)%>%matrix(ncol = 5,byrow = T)

colnames(X)<-c("id","stat2","trait2","stat1","trait1")

data<-X%>%data.frame()%>%mutate(id=factor(id))

data<-data%>%dplyr::select(id,stat1,stat2,trait1,trait2)

idata<-data.frame(factor=factor(c(rep(c("fac1","fac2"),each=2))),
                  time=ordered(rep(c(1,2),2)))

man<-manova(cbind(stat1,stat2,trait1,trait2)~1,data)

Man<-Manova(man,type="III",idata=idata,idesign=~factor*time)%>%summary()

Man




gstat<-data%>%dplyr::select(id,stat1,stat2)%>%gather("time","score",-id)

gstat%>%group_by(time)%>%summarize(mean(score))

gtrait<-data%>%dplyr::select(id,trait1,trait2)%>%gather("time","score",-id)

gtrait%>%group_by(time)%>%summarize(mean(score))

pairwise.t.test(x = gstat$score,g = gstat$time,paired = T)

pairwise.t.test(x = gtrait$score,g = gtrait$time,paired = T)

