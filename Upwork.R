library(RSelenium)
library(tidyverse)
library(stringr)
library(rvest)


#################

naput<-function(x){
        ifelse(length(x)<1,NA,x)
}
nullremove<-function(x){
        ifelse(str_detect(x,"NULL"),NA,x)
}

text<-function(x){
        x$getElementText()
}

element<-function(xpath){
        rmdr$findElements("xpath",xpath)
}

attrib<-function(x,attr){
        x$getElementAttribute(attr)
}
#################



rs<-rsDriver(port = 3077L,browser = "firefox")
rmdr<-rs$client

rmdr$navigate("https://www.upwork.com/")

emailE<-rmdr$findElement("xpath","/html/body/div[1]/div[2]/div/up-header-visitor-primary-nav/nav/div/div[2]/ul/li[2]/a")

emailE$highlightElement()

emailE$clickElement()

emailfE<-rmdr$findElement("xpath","//*[@id='login_username']")

emailfE$highlightElement()

emailfE$sendKeysToElement(list("vet.m.mohamed@gmail.com"))

cont1E<-rmdr$findElement("xpath","/html/body/div/div[2]/div/main/div/div[4]/form/div[1]/div/div/button")

cont1E$clickElement()

passE<-rmdr$findElement("xpath","//*[@id='login_password']")

passE$sendKeysToElement(list("salsa2762013"))

logE<-rmdr$findElement("xpath",'/html/body/div/div[2]/div/main/div/div[4]/form/div[2]/div/div/button')

logE$clickElement()

 #loading more pages

bodyE<-rmdr$findElement("xpath","/html/body")
loadE<-rmdr$findElement("xpath","//*[@id='load-more-button']")

for(i in 1:100){
        Sys.sleep(3)
        loadE$clickElement()
        bodyE$sendKeysToElement(list("end"))
}


#clicking on more

moreE<-element(xpath = "/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/div[2]/div/div/span[1]/a")


moreE[[3]]$highlightElement()


for(i in 1:length(moreE)){
        moreE[[i]]$clickElement()
}


#getting the job decription


titleE<-rmdr$findElements("xpath",'/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div/h4/a')



titlename<-sapply(titleE,function(x){
        x$getElementText()
})

titlelink<-sapply(titleE,function(x){
        x$getElementAttribute("href")
})

jobtypeE<-rmdr$findElements("xpath",'/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div/div/div/div/section/div/div/div/div/div/small[1]/span[1]')

jobType<-sapply(jobtypeE,text)

joblevelE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div/div/div/div/section/div/div/div/div/div/small[1]/span[2]")

jobLevel<-sapply(joblevelE,text)

budgetE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div/div/div/div/section/div/div/div/div/div/small[1]/span[3]")

budget<-sapply(budgetE,text)

timeE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div/div/small/span/span/time")

time<-sapply(timeE,function(x){
        attrib(x,"data-eo-relative")
})




jobdescE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/div[2]/div/div")

jobdesc<-sapply(jobdescE,text)

tagE<-list()

for(i in 1:length(jobdesc)){
        tagE[[i]]<-element(paste("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section[",i,"]/div/div/div/div[2]/div/div[2]/div/span/div/span/a"))
}

tags<-list()

for(i in 1:length(tagE)){
        tags[[i]]<-sapply(tagE[[i]],text)
}


nflancerE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/div[3]/span[2]")



nflancer<-sapply(nflancerE,text)



#client descriptopn 

plusE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/small[2]/span[2]/span/span[1]")


plus<-sapply(plusE,text)


payE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/small[2]/span[2]/span/span[2]/span/span")


payV<-sapply(payE,function(x){
        attrib(x = x,attr = "class")
})


custmscoreE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/small[2]/span[2]/span/span[3]/span/span")

customscore<-sapply(custmscoreE,function(x){
        attrib(x,"data-eo-popover-html-unsafe")
})

spentE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/small[2]/span[2]/span/span[4]/span")


spent<-sapply(spentE,text)

countryE<-element("/html/body/div[1]/div[2]/div/div[2]/div[5]/div[2]/div/div[2]/div/div/div/section/div/div/div/div[2]/div/small[2]/span[2]/span/span[5]")

country<-sapply(countryE,text)
#####################################################################



title<-c(unlist(titlename))

titlelink<-c(unlist(titlelink))

jobType<-c(unlist(jobType))

level<-c(unlist(jobLevel))

budget<-c(unlist(budget))

date<-c(unlist(time))%>%str_extract("[:digit:]*(\\-)([:digit:]*)(\\-)([:digit:]*)")

content<-c(unlist(jobdesc))

tag2<-lapply(tags,function(x){
        unlist(x)
})

tag<-cbind(tag2)%>%data.frame()

suppressWarnings(tag<-tag$tag2%>%str_remove_all(pattern = "(c)([\\(])"))

suppressWarnings(tag<-tag%>%str_remove_all(pattern = "\\)"))

nfreelance<-c(unlist(nflancer))

spent<-c(unlist(spent))%>%str_remove_all(pattern = "spent")

customscore<-sapply(customscore,function(x){
        ifelse(length(x)<1,NA,x)
})

score<-c(unlist(customscore))

country<-c(unlist(country))

plus<-c(unlist(plus))

data<-data.frame(jobtitle=title,
                 joblink=titlelink,
                 jobLevel=level,
                 budget=budget,
                 date=date,
                 content=content,
                 tags=tag,
                 numberOFfreelancers=nfreelance,
                 totalspent=spent,
                 customerscore=score,
                 country=country,
                 customertype=plus
                 )






data<-data%>%mutate(budget=str_remove_all(budget,"\\,"),
                    budget=str_extract(budget,"(?<=\\$)[:digit:]+"),
                    numberOFfreelancers=str_remove_all(numberOFfreelancers,"(\\s)"),
                    numberOFfreelancers=str_extract(numberOFfreelancers,"(?<=\\:)[1-9]+"),
                    jobLevel=str_remove_all(jobLevel,"(\\s)"),
                    jobLevel=str_extract(jobLevel,"[a-z|A-Z]+"),
                    tags=str_remove_all(tags,"(\")"),
                    tags=str_remove_all(tags,"(?<=\\s)(\\,)"),
                    tags=nullremove(tags),
                    score=str_extract(customerscore,"([:digit:])(\\.)([:digit:]*)"),
                    Nfeedback=str_extract(customerscore,"(?<=\\s)([:digit:]+)"),
                    customertype=str_extract(customertype,"([:alpha:]+)"))

str(data)

data<-data%>%mutate(jobtitle=as.character(jobtitle),
                    joblink=as.character(joblink),
                    jobLevel=factor(jobLevel),
                    budget=as.numeric(budget),
                    date=as.Date(date),
                    content=as.character(content),
                    tags=as.character(tags),
                    numberOFfreelancers=as.numeric(numberOFfreelancers),
                    totalspent=factor(totalspent),
                    country=factor(country),
                    customertype=factor(customertype),
                    score=as.numeric(score),
                    Nfeedback=as.numeric(Nfeedback))

write.csv(data,file = paste0("./data sets/upwork","/",Sys.Date(),"-",19,".csv"))
saveRDS(object = data,file =paste0("./data sets/upwork","/",Sys.Date(),"-",19,".rds"))

#########################################################

#########################################################

files<-list.files("./data sets/upwork",pattern = ".rds")

data<-list()

for(i in files){
        data[[i]]<-readRDS(file = paste0("./data sets/upwork/",i))
}

dataall<-bind_rows(data)

dub<-which(duplicated(dataall$jobtitle))

dataall<-dataall[-dub,]

dataall<-dataall%>%arrange(desc(date))

saveRDS(object = dataall,file = paste0("./data sets/upwork","/",Sys.Date(),".rds"))
