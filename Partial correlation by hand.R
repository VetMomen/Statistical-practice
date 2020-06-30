library(tidyverse);library(ppcor);library(readxl)

dir<-"C:\\Users\\Salsa\\Documents\\Statistical-practice\\data sets\\APPLICATION AN INVESTMENT EQUATION.xlsx"


dat<-read_xlsx(dir)
names(dat)<-c("Investment","Constant","Trend","GNP","Interest","Inflation")

#simple correlation

cor(dat[,-2])

#partial correlation using pcor

pcor(dat[,-2])

#doing it manually is overwhelmin , but i will do it 

#1- p-cor investment(I) and trend (T)

#Making residual matrix M

I<-diag(x = 1,nrow = 15,ncol = 15)
X<-dat[,c("Constant","GNP","Interest","Inflation")]%>%as.matrix()

M<-I-(X%*%solve(t(X)%*%X)%*%t(X))

z<-M%*%dat$Trend
y<-M%*%dat$Investment

pr2<-function(z,y){
        return(((t(z)%*%y)^2)/((t(z)%*%(z))*(t(y)%*%(y))))
}

pr_trend<-sqrt(pr2(z = z,y = y)) #the same value , the sign is the sign of respective coefficient when regressing y on z



#2- p-cor investment and GNP


X<-dat[,c("Constant","Trend","Interest","Inflation")]%>%as.matrix()
M<-I-(X%*%solve(t(X)%*%X)%*%t(X))


z<-M%*%dat$GNP
y<-M%*%dat$Investment

pr_GNP<-sqrt(pr2(z = z,y = y)) #the same as pcor




#3- p-cor investment and interest


X<-dat[,c("Constant","Trend","GNP","Inflation")]%>%as.matrix()
M<-I-(X%*%solve(t(X)%*%X)%*%t(X))


z<-M%*%dat$Interest
y<-M%*%dat$Investment

pr_interest<-sqrt(pr2(z = z,y = y)) #the same as pcor



#4- p-cor investment and inflation


X<-dat[,c("Constant","Trend","GNP","Interest")]%>%as.matrix()
M<-I-(X%*%solve(t(X)%*%X)%*%t(X))


z<-M%*%dat$Inflation
y<-M%*%dat$Investment

pr_inflation<-sqrt(pr2(z = z,y = y)) #the same as pcor

