if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

library(factoextra)
data("multishapes")

#running k mean to illustrate the diff between the two methods

df<-multishapes[,1:2]
set.seed(123)
km.res<-kmeans(x = df,centers = 5,nstart = 25)
fviz_cluster(km.res,df,geom = 'point')


install.packages("fpc")
install.packages("dbscan")

library(fpc)
library(dbscan)

#running DBSCAN
set.seed(123)
db<-dbscan(x = df,eps = .15,minPts = 5)

plot(db,df)

print(db)

df<-df%>%mutate(db_clust=db[[1]])
plot(df[,1:2],col=df[,3])

#determinig the best epi value 
#using k-nearest neighbor distances method
kNNdistplot(x = df,k = 5)
abline(h = .15)

# Cluster predictions with DBSCAN algorithm
?predict.dbscan

#Application of DBSCAN on a real data

data("iris")
iris<-as.matrix(iris[,1:4])

kNNdistplot(x = iris,k = 4)
abline(h = .4)

db<-dbscan(x = iris,eps = .39,minPts = 4)

fviz_cluster(db,iris,geom = 'point')

