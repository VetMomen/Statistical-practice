gpx<-read_excel('./data sets/locations.xlsx',col_types = c('text','numeric','numeric'))

stemmed_gpx<-sapply(gpx$Name,function(x){
        stem(x,transliteration = T)
})
stemmed_gpx%>%View()

idx<-str_detect(stemmed_gpx,'mzr3')

farms<-gpx[idx,]
str(farms)

farms%>%leaflet()%>%addTiles()%>%addCircleMarkers(label = farms$Name,radius = .6,opacity = 1,fill = 'black')
