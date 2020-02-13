library(tidyverse)
library(lubridate)

#importing the data of corona virus

corona<-read_csv(file = "/home/salsa/Downloads/2019_nCoV_data.csv",col_types = c('c?cc?nnn'))

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
