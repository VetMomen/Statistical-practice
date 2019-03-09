#calculating Hobkins statistics

#1- getting original data set
data('faithful')
df<-faithful
df%>%ggplot(aes(x = eruptions,y = waiting))+
  geom_point()+
  geom_density_2d()

# 2 - creating random uniform data set 

random_df<-apply(df,2,function(x){
  runif(n = length(x),min = min(x),max = max(x))
})

random_df<-data.frame(random_df)

random_df%>%ggplot(aes(x = eruptions,y=waiting))+
  geom_point()


# 3 - calculating Hopkins statistics for df and random_df

hopkins(data = df,n = nrow(df)-1)

hopkins(data = random_df,n = nrow(random_df)-1)

# if Hopkins statistics is closer and arround 0.5 ,then it has no tendency to clustering 
# but if it closer to 1 or -1 so it has a tendency to clustering 


# our data has tendency to cluster so lets do all types of clustering as a practice 

# 1- Kmeans clustering 

km<-kmeans(x = df,centers = 2,nstart = 25)

kdf<-cbind(df,km$cluster)

kdf%>%ggplot(aes(x = eruptions,waiting,color=factor(`km$cluster`)))+
  geom_point()

# 2- H-clutering 

scaled<-scale(df)
dist<-dist(scaled)
clustered<-hclust(d = dist,method = 'average')
plot(clustered)
rect.hclust(tree = clustered,k = 2,border = 5:9)

cutted<-cutree(tree = clustered,k = 2)

kdf<-cbind(kdf,cutted)


# 3- DBSCAN clustering 

kNNdistplot(x = df,k = 4)
abline(h = 1.5)

db<-dbscan(x = df,eps = 1.5,minPts = 4)
fviz_cluster(object = db,data = df,geom = 'point')

kdf<-cbind(kdf,db$cluster)

kdf%>%ggplot(aes(x = eruptions,waiting,color=factor(`db$cluster`)))+
  geom_point()+
  geom_density_2d()


# assessment by " Visual inclination tendancy 'VIM' "
dissplot(dist)
dissplot(dist,labels = kdf$`km$cluster`)

rand_df_scaled<-scale(random_df)
rand_df_dist<-dist(rand_df_scaled)

dissplot(rand_df_dist)

# there are function in factoextra called get_clust_tendency for both Hopkins and VIM
?get_clust_tendency

gredient_col<-list(low='black',high='white')

get_clust_tendency(data = df,n = 50,gradient = gredient_col)
