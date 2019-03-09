bone<-read_csv(file = './data sets/Bones.csv')

str(bone)

head(bone)

aggr(bone)

bone<-bone%>%drop_na()

#################################
##try 1 after without nstart####
#################################
ss1<-list()
set.seed(2)
for(i in 2:100){
  cluster<-kmeans(bone[,2:11],centers = i)
  cluster$cluster
  ss1[[i]]<-cluster$tot.withinss
}

bone$clusters<-cluster$cluster

#plotting number of clusters against tot.withinss
ss1<-unlist(ss1)
num_clust<-data.frame(x=1:99,y=ss1)

plot(num_clust,xlab='Number of Clusters',ylab='Total within sum squared error',
     main='Number of Clusters Vs Tot.withinss')
abline(v = 4.5,lty='dashed')

########################3
######## try 2 #########
########################

ss2<-list()
set.seed(2)
for(i in 2:100){
  cluster<-kmeans(bone[,2:11],centers = i,nstart = 30)
  cluster$cluster
  ss2[[i]]<-cluster$tot.withinss
}

bone$clusters<-cluster$cluster

#plotting number of clusters against tot.withinss
ss2<-unlist(ss2)
num_clust<-data.frame(x=1:99,y=ss2)

plot(num_clust,xlab='Number of Clusters',ylab='Total within sum squared error',
     main='Number of Clusters Vs Tot.withinss')
abline(v = 4.5,lty='dashed')



###############################
########## try 3 ##############
##############################

ss3<-list()
set.seed(2)
for(i in 2:100){
  cluster<-kmeans(bone[,2:11],centers = i,nstart = 100)
  cluster$cluster
  ss3[[i]]<-cluster$tot.withinss
}

bone$clusters<-cluster$cluster

#plotting number of clusters against tot.withinss
ss3<-unlist(ss3)
num_clust<-data.frame(x=1:99,y=ss3)

plot(num_clust,xlab='Number of Clusters',ylab='Total within sum squared error',
     main='Number of Clusters Vs Tot.withinss')
abline(v = 4.5,lty='dashed')

###############################
########## try 4 ##############
##############################

ss4<-list()
set.seed(2)
for(i in 2:100){
  cluster<-kmeans(bone[,2:11],centers = i,nstart = 200)
  cluster$cluster
  ss4[[i]]<-cluster$tot.withinss
}

bone$clusters<-cluster$cluster

#plotting number of clusters against tot.withinss
ss4<-unlist(ss4)
View(ss4)
num_clust<-data.frame(x=1:99,y=ss4)

plot(num_clust,xlab='Number of Clusters',ylab='Total within sum squared error',
     main='Number of Clusters Vs Tot.withinss')
abline(v = 4.5,lty='dashed')

##################################################################

# so i have 4 clusters according to length and wide of each bone 

Clusters<-kmeans(x = bone[,2:11],centers = 4,nstart = 100)

bone$clusters<-Clusters$cluster

splom(bone[,2:11],groups = bone$clusters)

Clusters$centers

parallelplot(bone[,2:11],groups = bone$clusters)

