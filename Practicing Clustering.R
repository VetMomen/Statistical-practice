#This is a practicing for customer segmentation using cluster analysis 

#loading libraries 

library(tidyverse)
library(caret)
library(factoextra)
library(NbClust)
library(gridExtra)
library(mclust)
library(cluster)

#importing the data 

seg<-read.csv("./data sets/Mall_Customers.csv")

head(seg)

names(seg)

str(seg)

seg<-seg%>%mutate(Age=as.numeric(Age),
                  Annual.Income..k..=as.numeric(Annual.Income..k..),
                  Spending.Score..1.100.=as.numeric(Spending.Score..1.100.))

summary(seg)

#converting to dummy

dumseg<-model.matrix(CustomerID~.,data = seg)[,-1]%>%data.frame()


#near zero variance check

nearZeroVar(dumseg)

#Exploring the data through some plotting 

#lets scattering the income and spending the score in term of age 

dumseg%>%ggplot(aes(x = Annual.Income..k..,y = Spending.Score..1.100.,col=Age))+
        geom_point()+
        xlab("Annual Icome")+
        ylab("Spending Score")+
        scale_colour_gradient(low = "blue",high = "red")

#major of high age low in spending score 

#see the difference between male and female 


dumseg%>%ggplot(aes(x = Annual.Income..k..,y = Spending.Score..1.100.,col=Age))+
        geom_point()+
        xlab("Annual Icome")+
        ylab("Spending Score")+
        scale_colour_gradient(low = "blue",high = "red")+
        facet_wrap(.~GenderMale)


#the same obs in both

#comparing between male and female in each variable

box<-list()
seg%>%group_by(Gender)%>%summarize(Spending.Score..1.100.=mean(Spending.Score..1.100.))%>%ggplot(aes(x = Gender,y = Spending.Score..1.100.))+geom_col(width = .5,col="black",fill="white")

seg%>%group_by(Gender)%>%summarize(Annual.Income..k..=mean(Annual.Income..k..))%>%ggplot(aes(x = Gender,y = Annual.Income..k..))+geom_col(width = .5,col="black",fill="white")

seg%>%group_by(Gender)%>%summarize(Age=mean(Age))%>%ggplot(aes(x = Gender,y = Age))+geom_col(width = .5,col="black",fill="white")

#checking the tendency to cluster and number of clusters

scaled<-scale(dumseg)
set.seed(1334)

grcol<-list(low="white",high="black")
get_clust_tendency(data = scaled,n = nrow(scaled[-1,]),graph = T,gradient = grcol)

#we need hopkins stats to be far a way form .5 
#here we see that we have a loer tendency to cluster 
#but some what we have in ordered dissimilarity matrix one big cluster and anohter small one 

#number of clusters



Nclust<-NbClust(data = scaled,distance  = "euclidean",method = "average")

Nclust$Best.nc

Nclust$Best.nc%>%t()%>%data.frame()%>%group_by(Number_clusters)%>%summarize(n=n())%>%arrange(desc(n))

#here we see that most of indecies suggest 2 clusters or 5 clusters or 15 clusters

#i will test the 3 sugestions
tot.with<-c()

for(i in 1:20){
        clust<-kmeans(x = scaled,centers = i,nstart = 20)
        tot.with[i]<-clust$tot.withinss
}

plot(1:20,tot.with)
lines(1:20,tot.with)

#we see here that the great difference here is between 1 and 2 clusters not 5 or 15 

#so i will use 2 clusters kmean
seg<-seg[,-1]
dist_mat<-daisy(seg,metric = "gower")


kclust<-kmeans(dist_mat,centers = 2,nstart = 20)


#adding the clusters to the data 

seg<-seg%>%mutate(kcluster=kclust$cluster,
                  kcluster=factor(kcluster))

plot(seg,col=seg$kcluster)

#table of clusters

table(seg$kcluster)

seg%>%group_by(kcluster)%>%summarize(meanage=mean(Age),sdage=sd(Age),
                                     meanspen=mean(Spending.Score..1.100.),
                                     sdspen=sd(Spending.Score..1.100.),
                                     meaninc=mean(Annual.Income..k..),
                                     sdinc=sd(Annual.Income..k..))

#compairing between the two clusters in term of each of other variables

sp<-seg%>%group_by(kcluster)%>%summarize(mean=mean(Spending.Score..1.100.))%>%
        ggplot(aes(kcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean spending rate")

inc<-seg%>%group_by(kcluster)%>%summarize(mean=mean(Annual.Income..k..))%>%
        ggplot(aes(kcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean income")

age<-seg%>%group_by(kcluster)%>%summarize(mean=mean(Age))%>%
        ggplot(aes(kcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean age")

gen<-seg%>%group_by(kcluster,Gender)%>%summarize(No=n())%>%
        ggplot(aes(kcluster,No,fill=Gender))+geom_col(width = .3,position = "dodge")+
        geom_label(aes(label=No),position = position_dodge(width = .3))+
        ggtitle(label = "number of gender")

grid.arrange(sp,inc,
             age,gen)

#from clusters we are extracted 

#in cluster 1 , itis charchaterized with high spending rate and lower age mean with high frequancy of women 
#in cluster2 , it is charchterized with low spending rate and high age mean and a so close of men and women frequancies 

#i will use hirarchical clustring method also

#finding the distance matrix using gower method


#clustring

hclst<-hclust(d = dist_mat,method = "ward.D2")

plot(hclst)

rect.hclust(hclst,k = 2,border = c("red","blue"))

cutclust<-cutree(hclst,k = 2)



#adding the clusters to the data 

seg<-seg%>%mutate(hcluster=cutclust,
                  hcluster=factor(hcluster))

table(seg$kcluster,seg$hcluster)

#revealing the secrets by graphing again

plot(seg,col=seg$hcluster)


sp<-seg%>%group_by(hcluster)%>%summarize(mean=mean(Spending.Score..1.100.))%>%
        ggplot(aes(hcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean spending rate")

inc<-seg%>%group_by(hcluster)%>%summarize(mean=mean(Annual.Income..k..))%>%
        ggplot(aes(hcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean income")

age<-seg%>%group_by(hcluster)%>%summarize(mean=mean(Age))%>%
        ggplot(aes(hcluster,mean))+geom_col(width = .3,fill="blue")+
        geom_label(aes(label=mean%>%round(digits = 2)))+
        ggtitle(label = "mean age")

gen<-seg%>%group_by(hcluster,Gender)%>%summarize(No=n())%>%
        ggplot(aes(hcluster,No,fill=Gender))+geom_col(width = .3,position = "dodge")+
        geom_label(aes(label=No),position = position_dodge(width = .3))+
        ggtitle(label = "number of gender")

grid.arrange(sp,inc,
             age,gen)


#now lets try model based clustering 

mclus<-Mclust(dist_mat)

summary(mclus)


prd<-predict(mclus)

mclustr<=prd$classification

mclustr<-factor(mclustr)

plot(mclus,what="density")

seg<-seg%>%mutate(mclustr=mclustr)

