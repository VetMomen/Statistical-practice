download.file(url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt',
              destfile = './data sets/seeds.txt')

file <- './data sets/seeds.txt'

feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')

seeds<-read.csv(file = file,sep = '\t',col.names = feature_name)


str(seeds)

aggr(seeds)

seeds<-seeds%>%drop_na()

seeds_clust<-seeds[,1:7]

#Scaling all variables 

scaled_seeds<-data.frame(scale(seeds_clust))

#Extracting Dist Matrix

dist_mat<-dist(scaled_seeds,method = 'euclidean')

#running cluster

hclust_averg<-hclust(d = dist_mat,method = 'average')

#ploting dendrogram

plot(hclust_averg)
#cutting tree

cut_avg<-cutree(hclust_averg,k = 3)

plot(cut_avg)

#see cluster on dendo 
plot(hclust_averg)
rect.hclust(hclust_averg,k = 3,border = 2:3)

#coloring branches 
install.packages('dendextend')
library(dendextend)

hclust_obj<-as.dendrogram(hclust_averg)
new_colored_hclust<-color_branches(hclust_obj)
plot(new_colored_hclust)


#adding the cluster to seeds df

seeds<-seeds%>%mutate(cluters=cut_avg)

#some works
str(seeds)
seeds%>%ggplot(aes(x = area,y = perimeter,color=factor(cluters)))+
  geom_point()

table(seeds$cluters,seeds$type.of.seed)

#goodness of clustering

dun<-list()
for(i in 2:10){
  cut_avg<-cutree(hclust_averg,k = i)
  dun[[i]]<-dunn(distance = dist_mat,clusters = cut_avg)
}

plot(x=1:10,y=dun)
