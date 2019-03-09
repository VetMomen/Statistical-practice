# 1st url which will work with 

url<-'https://olx.com.eg/en/properties/properties-for-sale/apartments-for-sale/alexandria/'

title_css<-'.ads__item__title'

location_css<- '.ads__item__location'

price_css<-'.price'

#scrabing the information from 500 page 
#creating urls list 
urls<-list()

#creating a loop for generation of 500 pages url

for( i in 1:500){
        urls[[i]]<-paste0('https://olx.com.eg/en/properties/properties-for-sale/apartments-for-sale/alexandria/?page=',i)
}

print(urls)

#creating object to store the information of each page on it 

olx_ads<-list()

#creating loop to scrap the info from all pages

for( i in 1:length(urls)){
        olx<-read_html(x = urls[[i]])
        title<-olx%>%html_nodes(css = title_css)%>%html_text(trim = T)
        location<-olx%>%html_nodes(css = location_css)%>%html_text(trim = T)
        price<-olx%>%html_nodes(css = price_css)%>%html_text(trim = T)
        price<-price[-c(1,2)]
        url<-olx%>%html_nodes(css = title_css)%>%html_attr('href')
        olx_ads[[i]]<-data.frame(title,location,price,url)
        gc()
}

#converting list to data frame and binding its rows

olx_ads<-bind_rows(olx_ads)

# getting r to read arabic language 

Sys.setlocale(category = 'LC_ALL',locale = 'Arabic')

# getting basic info with urls in another way

rs<-rsDriver(browser = 'chrome')
remdr<-rs$client

#getting the url of each ad

olx_ad<-list()
for(i in 1:length(urls)){
        remdr$navigate(urls[[i]])
        
        title_element<-remdr$findElements(using ='css',value = title_css)
        
        location_element<-remdr$findElements(using ='css',value = location_css)
        
        price_element<-remdr$findElements(using ='css',value = price_css)
        
        title<-sapply(title_element,function(y){
                y$getElementText()
        })
        title<-unlist(title)
        
        location<-sapply(location_element,function(y){
                y$getElementText()
        })
        location<-unlist(location)
        
        price<-sapply(price_element,function(y){
                y$getElementText()
        })
        price<-unlist(price)
        price<-price[-c(1,2)]
        
        url_elements<-remdr$findElements(using = 'css',value = title_css)
        url<-sapply(url_elements,function(y){
                y$getElementAttribute('href')
        })
        url<-unlist(url)
        
        olx_ad[[i]]<-data.frame(title,location,price,url)
        
        gc()
        
}

olx_ads<-bind_rows(olx_ad)

#removing duplicate from data

olx_adss<-olx_ads%>%distinct()

#writing it to Excel & rds file 

writeWorksheetToFile(file = './data sets/olx data.xlsx',data = olx_adss,sheet = 'olx(1)')

saveRDS(object = olx_adss,file = './data sets/olx data.rds')

#####################################
#testing single page
rs<-rsDriver(browser = 'firefox')
remdr<-rs$client

more_info<-function(url){
        remdr$navigate(url)
        area_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[2]/td[2]/table/tbody/tr/td/strong')
        area<-sapply(area_element,function(x){
                x$getElementText()
        })
        
        bath_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[2]/td[1]/table/tbody/tr/td/strong/a')
        bath_number<-sapply(bath_element,function(x){
                x$getElementText()
        })
        
        bed_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[1]/td[2]/table/tbody/tr/td/strong/a')
        
        bed_number<-sapply(bed_element,function(x){
                x$getElementText()
        })
        
        amin_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[1]/td[1]/table/tbody/tr/td/strong')
        aminities<-sapply(amin_element,function(x){
                x$getElementText()
        })%>%str_replace_all('[\\n]',' - ')
        
        fur_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[3]/td[1]/table/tbody/tr/td/strong/a')
        
        furr<-sapply(fur_element,function(x){
                x$getElementText()
        })
        
        level_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[2]/div[3]/table/tbody/tr[3]/td[2]/table/tbody/tr/td/strong/a')
        
        level<-sapply(level_element,function(x){
                x$getElementText()
        })
        
        phone_click<-remdr$findElement(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[2]/form/fieldset/div/div[1]/ul/li/div[2]/span/span')
        phone_click$clickElement()
        
        phone_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[2]/form/fieldset/div/div[1]/ul/li/div[2]/strong')
        phone<-sapply(phone_element,function(x){
                x$getElementText()
        })
        
        date_element<-remdr$findElements(using = 'xpath',value = '/html/body/div[4]/section/div/div/div[2]/div[1]/div[1]/div[1]/p/small/span')
        date<-sapply(date_element,function(x){
                x$getElementText()
        })
        
        info<-list(Date=date,Aminities=aminities,Area=area,Level=level,Furnitured=furr,Bed_rooms=bed_number,Bath_rooms=bath_number,Phone_number=phone)
        info<-sapply(info,function(x){
                if(length(x)<1){NA}
                else{print(x)}
        })
        return(info)
}

more_info('https://olx.com.eg/ad/85-ID9Eloj.html')
##################################################
