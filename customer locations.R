#extracting Customer locations 

library(rvest)
gpx<-read_html(choose.files())

loc<-gpx%>%html_nodes(xpath = '//wpt')%>%html_attrs()

cols<-bind_cols(loc)

loc2<-t(cols)

class(loc2)

rownames(loc2)<-NULL

colnames(loc2)<-c('lat','lon')

locations<-loc2%>%data.frame()

nam_nod<-gpx%>%html_nodes(xpath = '//wpt/name')
names<-nam_nod%>%html_text()

dir.create('./data sets')

writeNamedRegionToFile(file = './data sets/Locations.xlsx',data = names,name = 'location',formula = 'names!$A$1')
writeNamedRegionToFile(file = './data sets/Locations.xlsx',data = locations,name = 'location2',formula = 'names!$B$1')
