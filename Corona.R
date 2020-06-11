library(tidyverse)
library(lubridate)
library(leaflet)

#importing the data of corona virus

#choosing the directory

dirr=choose.dir()

print(dirr)

corona<-read_csv(file = paste(dirr,"2019_nCoV_data.csv",sep = "/"),col_types = c('c?cc?nnn'))

for(i in 1:ncol(corona)){
        col<-unlist(c(corona[,i]))
        class(col)%>%print()
}

#correcting the class of date by using lubridate package 
corona$Date<-mdy_hms(corona$Date)
class(corona$Date)

#do the same for the rest of date variables
#but this variable contain different formats which affect of data correction 
#so i see that removing time part will be useful 

corona$`Last Update`<-str_extract(string = corona$`Last Update`,pattern = "([0-9]{2})([:punct:])([0-9]{2})([:punct:])([0-9]{2,})")
class(corona$`Last Update`)
corona$`Last Update`<-str_replace_all(string = corona$`Last Update`,pattern = "\\/","\\-")
#separating the two format
st_format<-str_detect(corona$`Last Update`,pattern = "([:punct:])(\\d{4})")
st_format<-ifelse(is.na(st_format),FALSE,st_format)
st_d<-corona[st_format,]
st_d$`Last Update`<-mdy(st_d$`Last Update`)

nd_d<-corona[!st_format,]
nd_d$`Last Update`<-as_date(nd_d$`Last Update`)
nd_d$`Last Update`<-ymd(nd_d$`Last Update`)

unique(nd_d$`Last Update`)

#binding all together again

coronaC<-bind_rows(st_d,nd_d)
year(coronaC$`Last Update`)

#removing sno variable 

coronaC<-coronaC[,-1]

#importing the rest of info 

#making directory 

dirr2<-choose.dir()

corona2<-read.csv(paste(dirr2,"COVID19_line_list_data.csv",sep = "/"))

#importing corona 3

#making directory 

dirr3<-paste(dirr2,"COVID19_open_line_list.csv",sep = "/")

corona3<-read.csv(file = dirr3)

#taking the location

coronaloc<-corona3%>%select(province,longitude,latitude)

#making a variable with no of cases 

ncases<-coronaloc%>%group_by(longitude,latitude)%>%summarize("ncases"=n())

#what is the total number of cases :

sum(ncases$ncases)

#merge the n cases with locations

coronaloc<-left_join(coronaloc,ncases)


coronaloc$latitude<-as.character(coronaloc$latitude)
coronaloc$latitude<-as.numeric(coronaloc$latitude)

coronaloc$longitude<-as.character(coronaloc$longitude)
coronaloc$longitude<-as.numeric(coronaloc$longitude)

#removing non locations 
coronaloc<-coronaloc[!is.na(coronaloc$longitude),]

coronaloc%>%leaflet()%>%addTiles()%>%addMarkers(data =coronaloc, lng = coronaloc$longitude,lat = coronaloc$latitude,clusterOptions = markerClusterOptions(),label = paste(coronaloc$province,ncases$ncases,sep = "\t"))

######################################################

world<-map_data(map = "world")

names(coronaloc)<-c("province","long","lat","ncases")

world<-left_join(x=world,y = coronaloc)

world%>%ggplot(aes(x = long,y=lat,group=group))+geom_polygon(fill="white",col="black")+
        geom_point(data=coronaloc,aes(x=longitude,y=latitude))

