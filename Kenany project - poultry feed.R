
library(rvest);library(tidyverse);library(lubridate)



#Coding to harvest the data related to poultry feed price in the market

dateall<-c(as.Date("2020-06-25"):as.Date("2017-10-01")) #date serialing

dateall<-as.Date(dateall,origin="1970-01-01") #convert it into real date

#creating urls
urls<-paste0("https://elkenanygroup.com/%D8%A8%D9%88%D8%B1%D8%B5%D8%A9-%D8%A7%D9%84%D9%83%D9%86%D8%A7%D9%86%D9%8A/?term=%D8%A7%D8%B3%D8%B9%D8%A7%D8%B1-%D8%A7%D9%84%D8%A7%D8%B9%D9%84%D8%A7%D9%81&search_date=",dateall,"&term=%D8%A7%D8%B3%D8%B9%D8%A7%D8%B1-%D8%A7%D9%84%D8%A7%D8%B9%D9%84%D8%A7%D9%81&pa=")

#adding navigating pages to the urls
urls2<-sapply(urls,function(x){
        paste0(x,1:10)
})%>%t()


dimnames(urls2)<-list(NULL,NULL) #removing dimensions names

urls2<-data.frame(urls2) #converting into data frame

#Now testing scrabbing some dates

#getting the table node

tablenode<-'//*[@id="keywords"]'

#prepairing the urls

day4<-list()
for(i in 46:999){
        day4[[i]]<-read_html(urls2[i,4])%>%html_node(xpath = tablenode)%>%html_table()
}

Pfeed_price4<-bind_rows(day4)

saveRDS(object = Pfeed_price4,"/home/debian/Statistical-practice/data sets/poultry feed price64.rds")

################################################################################################
#Getting the price of Beef

beefdates<-c(as.Date("2020-06-25"):as.Date("2017-10-01")) #date serialing

beefdates<-as.Date(beefdates,origin="1970-01-01") #convert it into real date

beefurls<-paste0("https://elkenanygroup.com/%d8%a8%d9%88%d8%b1%d8%b5%d8%a9-%d8%a7%d9%84%d9%83%d9%86%d8%a7%d9%86%d9%8a/?term=%D8%A3%D8%B3%D8%B9%D8%A7%D8%B1-%D8%A7%D9%84%D8%B9%D8%AC%D9%88%D9%84-%D8%A7%D9%84%D9%82%D9%86%D9%8A%D8%A9&search_date=",beefdates)

beeftablenode<-'//*[@id="keywords"]'

beef_table<-list()
for(i in 601:999){
        beef_table[[i]]<-read_html(beefurls[i])%>%html_node(xpath = beeftablenode)%>%html_table()
}

#I detected a problem that some values in price column is not numeric , i am going to detect it and isolate it

cls<-list()
for(i in 1:508){
        cls[[i]]<-sapply(beef_table[[i]],is.numeric)%>%print
        
} #assign it

cls<-bind_rows(cls) #arranging 
Fls<-which(cls$السعر==F)#determining it

beef_table2<-beef_table[-Fls]#isolating it

beef_price<-bind_rows(beef_table2) #converting into data frame


#i detect another problem , in which the missed records caused a problem appeared as an issue in the class of some variables 
#i will remove it

cls2<-list()
for(i in 1:399){
        cls2[[i]]<-sapply(beef_table2[[i]],is.character)%>%print
        
}


cls2<-bind_rows(cls2) #arranging 
fls2<-which(cls2$النوع==FALSE)

beef_table3<-beef_table2[-fls2]

beef_price<-bind_rows(beef_table3)

#renaming it 
beef_price<-read_rds("/home/debian/Statistical-practice/data sets/beef_price.rds")
names(beef_price)<-c("Type","weight","price","direction","Date")

#removing direction

beef_price<-beef_price%>%select(-direction)
#extracting the weight alone 

beef_price$weight<-str_extract(beef_price$weight,"[:digit:]{2,}")

#extracting date

beef_price$Date<-str_extract(beef_price$Date,pattern = "([:digit:]{4})(\\-)([:digit:]{2})(\\-)([:digit:]{2})")
beef_price$Date<-ymd(beef_price$Date)

#renaming the type
unique(beef_price$Type)
beef_price$Type<-ifelse(beef_price$Type=="جمسي بلدى (300 كجم)","Balady_Baff",
                        ifelse(beef_price$Type=="جمسي بلدي (350كيلو )","Balady_Baff",
                               ifelse(beef_price$Type=="جمسي بلدي (500كيلو)","Balady_Baff",
                                      ifelse(beef_price$Type=="بقري خليط بلدي (200كيلو)","Cow_Balady_Mixed",
                                             ifelse(beef_price$Type=="بقري خليط بلدي (250 كيلو)","Cow_Balady_Mixed",
                                                    ifelse(beef_price$Type=="بقري خليط بلدي (300كيلو)","Cow_Balady_Mixed",
                                                           ifelse(beef_price$Type=="بقري خليط  بلدى (400 كيلو)","Cow_Balady_Mixed",
                                                                  ifelse(beef_price$Type=="جمسي بلدي(400 كيلو)","Balady_Baff",
                                                                         ifelse(beef_price$Type=="بقري مستورد(400كيلو)","imported_Cow",
                                                                                ifelse(beef_price$Type=="بقري مستورد(500 كيلو)","imported_Cow",NA))))))))))


#Adding month variable

beef_price<-beef_price%>%mutate("Month"=month(Date))
