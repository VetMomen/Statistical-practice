library(RSelenium)
Sys.setlocale(category = 'LC_ALL',locale = 'Arabic')

rD<-rsDriver(browser = 'firefox')

remDr<-rD$client

remDr$navigate('https://www.facebook.com/')

email<-remDr$findElement(using ='css',value = 'html#facebook body.fbIndex.UIPage_LoggedOut._-kb._61s0._605a.b_c3pyn-ahh.gecko.win.x1.Locale_en_GB.cores-lt4._19_u.hasAXNavMenubar div#u_0_b._li div#pagelet_bluebar div#blueBarDOMInspector div._53jh div.loggedout_menubar_container div.clearfix.loggedout_menubar div.menu_login_container.rfloat._ohf form#login_form table tbody tr td input#email.inputtext' )

email$sendKeysToElement(list('vet.m.mohamed@gmail.com'))

pass<-remDr$findElement(using = 'css',value = 'html#facebook body.fbIndex.UIPage_LoggedOut._-kb._61s0._605a.b_c3pyn-ahh.gecko.win.x1.Locale_en_GB.cores-lt4._19_u.hasAXNavMenubar div#u_0_b._li div#pagelet_bluebar div#blueBarDOMInspector div._53jh div.loggedout_menubar_container div.clearfix.loggedout_menubar div.menu_login_container.rfloat._ohf form#login_form table tbody tr td input#pass.inputtext')

pass$sendKeysToElement(list('salsa2762013s'))

logclick<-remDr$findElement(using = 'css',value = 'html#facebook body.fbIndex.UIPage_LoggedOut._-kb._61s0._605a.b_c3pyn-ahh.gecko.win.x1.Locale_en_GB.cores-lt4._19_u.hasAXNavMenubar div#u_0_b._li div#pagelet_bluebar div#blueBarDOMInspector div._53jh div.loggedout_menubar_container div.clearfix.loggedout_menubar div.menu_login_container.rfloat._ohf form#login_form table tbody tr td label#loginbutton.uiButton.uiButtonConfirm')

logclick$clickElement()

remDr$navigate('https://www.facebook.com/groups/fieldawy.animals/permalink/2072854956126529/')

replay<-remDr$findElements(using = 'css',value = '.UFIReplySocialSentenceVerified')
sapply(replay,function(x){
        x$clickElement()
})

actor_name<-remDr$findElements(using = 'css',value = '.UFICommentActorName')

Names<-sapply(actor_name,function(x){
        x$getElementText()
})%>%unlist()


actor_com<-remDr$findElements(using = 'css',value = '.UFICommentBody')
Comments<-sapply(actor_com,function(x){
        x$getElementText()
})%>%unlist()

remDr$refresh()

react_frame<-remDr$findElements(using = 'css',value = '._10la')

react_child<-sapply(react_frame,function(x){
        tryCatch(x$findChildElement(using='css',value='.UFISutroLikeCount'),error=function(err){NA})
})

Reaction_numbers<-sapply(react_child,function(x){
        tryCatch(x$getElementText(),error=function(err){NA})
})%>%unlist()

typereactelement<-remDr$findElements(using = 'xpath',value = '/html/body/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/form/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/div/a/div/span/span')
typeofreact<-sapply(typereactelement,function(x){
        tryCatch(x$getElementAttribute('aria-label'),error=function(err){NA})
})%>%unlist()





#################################################
data<-data.frame(Names,Comments,Reaction_numbers)
library(arabicStemR)
data<-data%>%mutate(Comments=cleanChars(Comments),
                     Names=cleanChars(Names))

#################################################3


