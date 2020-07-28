library(tidyverse);library(rvest);library(RSelenium)

rs<-rsDriver(browser = "firefox",port = 8355L)
rsD<-rs$client



rsD$navigate("https://www.otlob.com/egypt/restaurants/7425/mokattam---uptown-cairo")



body<-rsD$findElement(using = "xpath",value = "/html/body")


get_text<-function(xpath){
        elem<-rsD$findElements(using = "xpath",value = xpath)
        sapply(elem,function(x){
                x$getElementText()
        })
}

x<-1
repeat{
        body$sendKeysToElement(list(key="end"))
        Sys.sleep(time = 3)
        print(x)
        x<-x+1
        if(x>5){break}
}


Names<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/h4")

types<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/div[1]")

rate<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/div[2]")

Dtime<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/div[3]/span[1]")

Dprice<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/div[3]/span[4]/span")

minorder<-get_text(xpath = "/html/body/div[3]/ui-view/section/div/div[3]/div[2]/div[3]/div/section/div[1]/div/a/div/div[2]/div[3]/span[9]")


data<-bind_cols(unlist(Names),unlist(types),unlist(Dtime),unlist(Dprice),unlist(minorder),unlist(rate))
names(data)<-c("Name","Type","Delivary time","Delivary price","Min order","Rate")

write.csv(x = data,file = "~/data.csv")


#getting one of restaurants info

url<-"https://www.otlob.com/egypt/restaurant/514348/mega-sandwiches-mokattam--el-nafora-square?aid=7425"

rsD$navigate(url)

meal_name<-get_text(xpath = "/html/body/div[3]/ui-view/section/div[1]/div[4]/div[1]/section/div/div[2]/div[2]/uib-accordion/div/div/div[2]/div/div/div/div[2]/div/div[1]/p")
meal_price<-get_text(xpath = "/html/body/div[3]/ui-view/section/div[1]/div[4]/div[1]/section/div/div[2]/div[2]/uib-accordion/div/div/div[2]/div/div/div/div[2]/div/div[2]/div[1]/span")
meal_rate<-get_text(xpath = "/html/body/div[3]/ui-view/section/div[1]/div[4]/div[1]/section/div/div[2]/div[2]/uib-accordion/div/div/div[2]/div/div/div/div[2]/div/div[2]/div[1]/div")


mega_sand<-bind_cols(unlist(meal_name),unlist(meal_price),unlist(meal_rate))
names(mega_sand)<-c("name","price","rate")

write.csv(x = mega_sand,file = "/home/debian/mega.csv")
