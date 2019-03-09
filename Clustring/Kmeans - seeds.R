download.file(url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt',
              destfile = './data sets/seeds.txt')

file <- './data sets/seeds.txt'

feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')

seeds<-read.csv(file = file,sep = '\t',col.names = feature_name)


str(seeds)

aggr(seeds)

seeds<-seeds%>%drop_na()

seeds_clust<-seeds[,1:7]

clusters<-kmeans(x = seeds_clust,centers = 3,nstart = 100)

seeds<-seeds%>%mutate(clusters=clusters$cluster)

table(seeds$clusters,seeds$type.of.seed)


clust<-list()
for( i in 2:100){
  clustr<-kmeans(x = seeds_clust,centers = i,nstart = 100)
  clust[[i]]<-clustr$tot.withinss

}

clust<-unlist(clust)
plot(x=seq(1,99,length.out = 99),y=clust,,pch=4)
abline(v = 3.5,lty='dashed')
