#I Want to Show you the power of matrix algebra in estimating regression coefficients in R and coefficient of determination R2
#The data taken from Econometric Analysis , Ch3 application 

library(tidyverse);library(readxl) 


#importing and prepairing the data 

dir<-"/home/debian/Statistical-practice/data sets/Ch3 application econometric analysis.xlsx"

dat<-read_xlsx(dir)

str(dat)

minus<-which(str_detect(dat$Ability,pattern = "−"))

dat$Ability<-str_remove_all(dat$Ability,"−")

dat$Ability<-as.numeric(dat$Ability)

dat$Ability[minus]<-dat$Ability[minus]*-1

names(dat)<-c("Person","Education","Wage","Experience","Ability","M_education","F_education","Siblings")

#splitting the data 
X1<-cbind(rep(1,nrow(dat)),dat[,c("Education","Experience","Ability")])%>%as.matrix()
X2<-dat[,c("M_education","F_education","Siblings")]%>%as.matrix()
y<-dat$Wage%>%as.matrix()

X<-cbind(X1,X2)%>%as.matrix()

#regressing y on X1 and estimating coef. using OLS
coef1<-solve(t(X1)%*%X1)%*%t(X1)%*%y

#regressing y on X and estimating coef
coef2<-solve(t(X)%*%X)%*%t(X)%*%y

#Regressing each of X2 on X2 and get the fitted value

#make identity matrix
I<-diag(1,nrow = nrow(dat),ncol = nrow(dat))

#make a projection matrix of X1
PX1<-(X1%*%solve(t(X1)%*%X1)%*%t(X1))

#Make a residual matrix of X1
MX1<-I-(X1%*%solve(t(X1)%*%X1)%*%t(X1))

#calculate Fitted Value of each Value of X2
X2starfitted<-PX1%*%X2


#Calculate R2 of y on both X1 and X2

#Make centering matrix
i<-rep(1,nrow(dat))
M0<-I-(i%*%solve(t(i)%*%i)%*%t(i))

#residual matrix Of total X
MX<-I-(X%*%solve(t(X)%*%X)%*%t(X))

#total residual of y on X1 and X2
e<-MX%*%y

#SSE
s<-t(e)%*%e

#SST calculation
ym0y<-t(y)%*%M0%*%y

#R2 Calculation
R2<-1-(s/ym0y)

#computing R-square after removing the constant variable from X1

X_1<-X[,-1]

MX_1<-I-(X_1%*%solve(t(X_1)%*%X_1)%*%t(X_1))
e_1<-MX_1%*%y
s_1<-t(e_1)%*%e_1

R2_1<-1-(s_1/ym0y)


#computing R2 for constant term only 
#the centering matrix is the same residual matrix made by constants only 

e_c<-M0%*%y
s_c<-t(e_c)%*%e_c

R2_c<-1-(s_c/ym0y) #approximately zero

#regressing y on X1 and X2 after controling effect of X1 on it (X2start)
X2star<-MX1%*%X2
Xstar<-cbind(X1,X2star)


Coef_s<-solve(t(Xstar)%*%Xstar)%*%t(Xstar)%*%y
print(Coef_s)


#residual matrix for star data 

Mstart<-I-((Xstar%*%solve(t(Xstar)%*%Xstar)%*%t(Xstar)))

estar<-Mstart%*%y

s_star<-t(estar)%*%estar

R2star<-1-(s_star/ym0y)


#fitting partitioned regression for X2 

coef_p<-solve(t(X2)%*%MX1%*%X2)%*%(t(X2)%*%MX1%*%y)



#getting Coef of regressing y on X1 controlling X2

coef_P2<-solve(t(X1)%*%X1)%*%t(X1)%*%(y-(X2%*%coef_p))
