nba<-read_csv('D:/mo2men/DATA SCIENCE/R/data sets/NBA_player_of_the_week.csv')

str(nba)
nba<-nba%>%mutate(Date=mdy(Date))

nba<-nba%>%mutate(Height=str_replace_all(string = Height,pattern = '\\-','.'),
                  Height=as.numeric(Height))


nba%>%ggplot(aes(x = Weight,y=Height))+
        geom_jitter()+
        geom_smooth(method = 'lm')

#DBSCAN scan

clust<-nba[,c("Height","Weight")]%>%drop_na()
kNNdistplot(x = clust,k = 4)
abline(h = 1)

db<-dbscan(x = clust,eps = .3,minPts = 4)

#kmeans cluster

cl2<-list()
for( i in 1:100){
        cl<-kmeans(x = clust,centers = i,nstart = 25)
        cl2[i]<-cl$tot.withinss
}
cl<-unlist(cl)
plot(x=1:100,y=cl2)

km<-kmeans(x = clust,centers = 3,nstart = 25)
clust$cluster<-km$cluster
clust%>%ggplot(aes(x = Height,y=Weight,color=cluster))+
        geom_point()


