##K-means 
#downloading file
download.file(url = 'https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city/downloads/uber-pickups-in-new-york-city.zip/2',destfile = './data sets/Uber.zip')

#multiple file importing 

tempfill<-dir('./data sets',pattern = '.csv')
data14<-list()

for( i in 1 : length(tempfill)){
  data14[[i]]<-read_csv(file = paste0('./data sets/',tempfill[i]))
}

#binding the data

data14<-bind_rows(data14)

#talking sample of data due to memory limits 
sampled_data<-data14%>%sample_n(size = 500000,replace = F)
rm(data14)
#information about data structure and summary 
str(sampled_data)

object.size(sampled_data)

summary(sampled_data)

#Visualization of missing data 

library(VIM)

aggr(sampled_data)

#separating every date Element 

head(sampled_data)

sampled_data<-sampled_data%>%mutate(
  `Date/Time`=mdy_hms(`Date/Time`),
  year=year(`Date/Time`),
  month=month(`Date/Time`),
  day=day(`Date/Time`),
  week_day=wday(`Date/Time`),
  hour=hour(`Date/Time`),
  minute=minute(`Date/Time`),
  second=second(`Date/Time`)
)

###################################3
# K= 5
?kmeans

set.seed(5)


clusters<-kmeans(x = sampled_data[,2:3],centers = 5)

sampled_data<-sampled_data%>%mutate(Borough =clusters$cluster)



####################################
#plotting the clusters
?RColorBrewer 
colorrrr<-colorFactor(palette = 'Accent',domain = sampled_data$Borough)
mapppp<-sampled_data%>%leaflet()%>%addPolygons(lng = ~Lon,lat = ~Lat,color = ~colorrrr(Borough))

