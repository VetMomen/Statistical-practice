data(ruspini)
d1<-dist(ruspini)
hc<-hclust(d1,method = 'average')
plot(hc)
d2<-cophenetic(hc)
cor(d1,d2)

#applting it to all type of clustering 

methods<-c("ward.D", "ward.D2", "single", "complete", "average","mcquitty","median","centroid")

cor<-list()
for( i in methods){
  d1<-dist(ruspini)
  hc<-hclust(d1,method = i)
  d2<-cophenetic(hc)
  cor[[i]]<-cor(d1,d2)
}

df<-data.frame(methods,unlist(cor))
par(las=2)
plot(df)
#############################################################
#other data set
data(USArrests)

scaled<-scale(USArrests)
dist<-dist(scaled)
methods<-c("ward.D", "ward.D2", "single", "complete", "average","mcquitty","median","centroid")
cor<-list()
for(i in methods){
  d1<-dist
  hc<-hclust(d = d1,method = i)
  d2<-cophenetic(hc)
  cor[[i]]<-cor(d1,d2)
}
df<-data.frame(methods,unlist(cor))
par(las=2)
plot(df)
