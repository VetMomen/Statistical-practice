#unfriend

library(RSelenium)
library(tidyverse)

element<-function(xpath){
        rmDr$findElements(using = "xpath",xpath)
}

attrelement<-function(x,attr){
        x$getElementAttribute(attr)
}

rs<-rsDriver(port = 3942L,browser = "chrome",chromever =  "75.0.3770.8",version = "latest")

rmDr<-rs$client

rmDr$navigate("https://www.facebook.com/vet.m.mohamed/friends?lst=100003277077323%3A100003277077323%3A1563059201&source_ref=pb_friends_tl")


load<-rmDr$findElement("xpath","/html/body")

for(i in 1:2){
        Sys.sleep(1)
        load$sendKeysToElement(list(key="end"))
        
}

load$sendKeysToElement(list(key="home"))

bottom<-element('//*[@class="FriendButton"]')

#id attributes extraction


ids<-sapply(bottom,function(x){
        attrelement(x,"id")
})

newpath<-paste0('//div[@id="',ids,'"]')

unfriendE<-sapply(newpath,function(x){
        rmDr$findElement("xpath",x)
})

unfriendE<-rmDr$findElement("xpath",newpath[1])

rmDr$mouseMoveToLocation(webElement = unfriendE)


rmDr$sendKeysToActiveElement(list(key="enter"))

