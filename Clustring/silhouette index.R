

data(ruspini)

plot(ruspini)

#silhouette for Kmean

km<-kmeans(x = ruspini,centers = 4,nstart = 25)

scaled<-scale(ruspini)

dist<-dist(scaled)

sil<-silhouette(x = km$cluster,dist=dist)

summary(sil)

plot(sil)

#silhouette for heirarchical clustering

hc<-hclust(d = dist,method = 'average')
cut<-cutree(tree = hc,k = 4)

plot(hc)
rect.hclust(hc,4,border = 2:8)

sil2<-silhouette(x = cut,dist = dist)
plot(sil2)

sum$avg.width

#using silhouette to determine the number of clusters

aver<-list()

for(i in 1:74){
  cut<-cutree(tree = hc,k = i)
  sil<-silhouette(x = cut,dist = dist)
  sam<-summary(sil)
  aver[i]<-sam["avg.width"]
}

plot(1:74,unlist(aver))
lines(1:74,unlist(aver))
abline(v = 4,lty='dashed')
