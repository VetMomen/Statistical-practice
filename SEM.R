#SEM practicing


library(lavaan)
library(semPlot)
library(tidyverse)


#path Analysis 

n<-3094
sd<-c(.71,.75,.90,.69,1.84,1.37)
lowercorr<-c(1.00,
.28,1.00,
.19,.21,1.00,
.15,.15,.23,1.00,
.35,.09,.20,.20,1.00,
.08,.11,.09,.11,.23,1.00)

fullcorr<-lav_matrix_lower2full(lowercorr)

covmat<-cor2cov(R = fullcorr,sds = sd)

colnames(covmat)<-rownames(covmat)<-c("ability","acheive","deg_asp","hi_deg","selectiv","income")

model<-'
income~hi_deg+selectiv
hi_deg~ability+acheive+deg_asp
selectiv~ability+acheive+deg_asp
ability~~acheive
ability~~deg_asp
acheive~~deg_asp
'

fit<-sem(model = model,sample.cov = covmat,sample.nobs = n)
summary(fit,standardized=T,fit.measures=T,rsquare=T)

semPaths(fit,layout = "tree2",whatLabels = "std",color = "red")

lavResiduals(object = fit,zstat = T)

#the std.all in case of error variance is the total variance unexplained 


modificationindices(fit,sort. = T)

model2<-'
income~hi_deg+selectiv
hi_deg~ability+acheive+deg_asp
selectiv~ability+acheive+deg_asp+hi_deg
ability~~acheive
ability~~deg_asp
acheive~~deg_asp
'

fit2<-sem(model = model2,sample.cov = covmat,sample.nobs = n)
summary(fit2,standardized=T,fit.measures=T,rsquare=T)

semPaths(fit2,layout = "tree2",whatLabels = "std",color = "red")

lavResiduals(object = fit2,zstat = T)

modificationindices(fit2,sort. = T)

model3<-'
income~hi_deg+selectiv+acheive
hi_deg~ability+acheive+deg_asp
selectiv~ability+acheive+deg_asp+hi_deg
ability~~acheive
ability~~deg_asp
acheive~~deg_asp
'

fit3<-sem(model = model3,sample.cov = covmat,sample.nobs = n)
summary(fit3,standardized=T,fit.measures=T,rsquare=T)

semPaths(fit3,layout = "tree2",whatLabels = "std",color = "red")

lavResiduals(object = fit3,zstat = T)

modificationindices(fit3,sort. = T)


model4<-'
income~hi_deg+selectiv+acheive
hi_deg~ability+acheive+deg_asp
selectiv~ability+acheive+deg_asp+hi_deg
ability~income
ability~~acheive
ability~~deg_asp
acheive~~deg_asp
'

fit4<-sem(model = model4,sample.cov = covmat,sample.nobs = n)
summary(fit4,standardized=T,fit.measures=T,rsquare=T)

semPaths(fit4,layout = "tree2",whatLabels = "std",color = "red")

lavResiduals(object = fit4,zstat = T)

modificationindices(fit4,sort. = T)

anova(fit4,fit3,fit2,fit) #third and fourth modifications is not significant 



#CFA by lavaan
n<-318
covlow<-c(.7821,
.5602,.9299,
.5695,.6281,.9751,
.1969,.2599,.2362,.6352,
.2289,.2835,.3079,.4575,.7943,
.2609,.3670,.3575,.4327,.4151,.6783,
.0556,.0740,.0981,.2094,.2306,.2503,.6855,
.0025,.0279,.0798,.2047,.2270,.2257,.4224,.6952,
.0180,.0753,.0744,.1892,.2352,.2008,.4343,.4514,.6065,
.1617,.1919,.2892,.1376,.1744,.1845,.0645,.0731,.0921,.4068,
.2628,.3047,.4043,.1742,.2066,.2547,.1356,.1336,.1283,.1958,.7015,
.2966,.3040,.3919,.1942,.1864,.2402,.1073,.0988,.0599,.2233,.3033,.5786)


covfull<-lav_matrix_lower2full(covlow)

colnames(covfull)<-rownames(covfull)<-c(paste0("TEN",1:3),paste0("WOR",1:3),paste0("IRTHK",1:3),paste0("BODY",1:3))


model<-'
TENSION=~TEN1+TEN2+TEN3
WORRY=~WOR1+WOR2+WOR3
TESTIRRTHINKING=~IRTHK1+IRTHK2+IRTHK3
BODILYSUMP=~BODY1+BODY2+BODY3
TENSION~~WORRY
TENSION~~TESTIRRTHINKING
TENSION~~BODILYSUMP
WORRY~~TESTIRRTHINKING
WORRY~~BODILYSUMP
TESTIRRTHINKING~~BODILYSUMP
TENSION~~1*TENSION
WORRY~~1*WORRY
TESTIRRTHINKING~~1*TESTIRRTHINKING
BODILYSUMP~~1*BODILYSUMP
'
fit<-cfa(model = model,sample.cov = covfull,sample.nobs = n,std.lv=TRUE)
summary(fit,fit.measures=T,standardized=T,rsquare=T)

lavResiduals(object = fit,zstat = T)
modificationindices(fit,sort. = T)[1,]

semPaths(fit,whatLabels = "std",residuals = T,color = "green")

#modification 1

model2<-'
TENSION=~TEN1+TEN2+TEN3
WORRY=~WOR1+WOR2+WOR3
TESTIRRTHINKING=~IRTHK1+IRTHK2+IRTHK3
BODILYSUMP=~BODY1+BODY2+BODY3+TEN3
TENSION~~WORRY
TENSION~~TESTIRRTHINKING
TENSION~~BODILYSUMP
WORRY~~TESTIRRTHINKING
WORRY~~BODILYSUMP
TESTIRRTHINKING~~BODILYSUMP
TENSION~~1*TENSION
WORRY~~1*WORRY
TESTIRRTHINKING~~1*TESTIRRTHINKING
BODILYSUMP~~1*BODILYSUMP
'
fit2<-cfa(model = model2,sample.cov = covfull,sample.nobs = n,std.lv=TRUE)
summary(fit2,fit.measures=T,standardized=T,rsquare=T)

lavResiduals(object = fit2,zstat = T)$cov.z%>%data.frame()%>%apply(MARGIN = 2,function(x){
        ifelse(abs(x)>=1.96,x,0)
})%>%View
modificationindices(fit2,sort. = T)

semPaths(fit2,whatLabels = "std",residuals = T,color = "green")


#modification 12

model3<-'
TENSION=~TEN1+TEN2+TEN3
WORRY=~WOR1+WOR2+WOR3
TESTIRRTHINKING=~IRTHK1+IRTHK2+IRTHK3
BODILYSUMP=~BODY1+BODY2+BODY3+TEN3
TENSION~~WORRY
TENSION~~TESTIRRTHINKING
TENSION~~BODILYSUMP
WORRY~~TESTIRRTHINKING
WORRY~~BODILYSUMP
TESTIRRTHINKING~~BODILYSUMP
WOR2~~WOR3
TENSION~~1*TENSION
WORRY~~1*WORRY
TESTIRRTHINKING~~1*TESTIRRTHINKING
BODILYSUMP~~1*BODILYSUMP
'
fit3<-cfa(model = model3,sample.cov = covfull,sample.nobs = n,std.lv=TRUE)
summary(fit3,fit.measures=T,standardized=T,rsquare=T)

lavResiduals(object = fit3,zstat = T)$cov.z%>%data.frame()%>%apply(MARGIN = 2,function(x){
        ifelse(abs(x)>=1.96,x,0)
})%>%View
modificationindices(fit3,sort. = T)

semPaths(fit3,whatLabels = "std",residuals = T,color = "green")

anova(fit,fit2,fit3)


#################################3
#structural ( latent path analysis ) model

corlow<-c(1.000,
.812,1.000,
.819,.752,1.000,
.334,.344,.228,1.000,
.177,.094,.141,.363,1.000,
.363,.383,.387,.241,.273,1.000,
.239,.258,.275,.286,.389,.445,1.000,
.243,.293,.234,.116,.096,.222,.344,1.000,
.672,.616,.621,.277,.137,.458,.315,.246,1.000,
.464,.620,.514,.213,.173,.430,.387,.132,.680,1.000,
.612,.640,.719,.192,.090,.509,.336,.230,.819,.676,1.000,
.331,.391,.310,.435,.263,.409,.298,.256,.446,.395,.411,1.000,
.209,.214,.286,.319,.671,.423,.334,.246,.308,.268,.280,.573,1.000,
.298,.358,.361,.171,.232,.791,.286,.057,.433,.387,.477,.389,.445,1.000,
.309,.303,.381,.132,.307,.637,.459,.267,.468,.406,.458,.554,.514,.551,1.000,
.056,.086,.092,.090,.201,.123,.247,.403,.176,.076,.131,.318,.213,.056,.342,1.000)

corfull<-lav_matrix_lower2full(corlow)
colnames(corfull)<-rownames(corfull)<-c(paste("SS1",1:3,sep = "_"),
                                        paste("SE1",1:2,sep = "_"),
                                        paste("EB1",1:3,sep = "_"),
                                        paste("SS2",1:3,sep = "_"),
                                        paste("SE2",1:2,sep = "_"),
                                        paste("EB2",1:3,sep = "_"))
sds<-c(2.46,1.76,2.74,2.04,2.13,4.30,1.90,1.90,2.63,1.89,2.84,2.34,2.27,4.86,2.66,1.94)
n<-300
covmat<-cor2cov(corfull,sds = sds)

path1<-paste("EB1",paste(paste("EB1",1:3,sep = "_"),collapse = "+"),sep = "=~")
path2<-paste("EB2",paste(paste("EB2",1:3,sep = "_"),collapse = "+"),sep = "=~")
path3<-paste("SE1",paste(paste("SE1",1:2,sep = "_"),collapse = "+"),sep = "=~")
path4<-paste("SE2",paste(paste("SE2",1:2,sep = "_"),collapse = "+"),sep = "=~")
path5<-paste("SS1",paste(paste("SS1",1:3,sep = "_"),collapse = "+"),sep = "=~")
path6<-paste("SS2",paste(paste("SS2",1:3,sep = "_"),collapse = "+"),sep = "=~")

measuremodel<-paste(path1,path2,path3,path4,path5,path6,sep  = "\n")
structuralmodel<-"
SS2~SS1
SE2~SS2+SS1+SE1
EB2~SE1+SE2+EB1

EB1~~SE1
EB1~~SS1
SE1~~SS1
"
model<-paste(measuremodel,structuralmodel,collapse  = "\n")

fit<-sem(model = model,sample.cov = covmat,sample.nobs = n)
summary(fit,fit.measures=T,standardized=T,rsquare=T)

semPaths(fit,whatLabels = "std")

modificationindices(fit,sort. = T)[1:10,]

lavResiduals(object = fit,zstat = T)$cov.z%>%data.frame()%>%apply(MARGIN = 2,function(x){
        ifelse(abs(x)>=1.96,x,NA)
})%>%round(digits = 2)%>%View

#running a saturated path analysis 

structuralmodel2<-'

EB1~~EB2
EB1~~SE2
EB1~~SS2
EB1~~SE1
EB1~~SS1
SE1~~EB2
SE1~~SE2
SE1~~SS2
SE1~~SS1
SS1~~EB2
SS1~~SE2
SS1~~SS2
EB2~~SE2
EB2~~SS2
SE2~~SS2

'
model2<-paste(measuremodel,structuralmodel2,collapse  = "\n")
fit2<-sem(model = model2,sample.cov = covmat,sample.nobs = n)
summary(fit2,fit.measures=T,standardized=T,rsquare=T)

semPaths(fit2,whatLabels = "std")

modificationindices(fit2,sort. = T)[1:10,]
pathmi1<-"EB1_1 ~~ EB2_1"
pathmi2<-"SE1_2 ~~ SE2_2"
pathmi3<-"SE1_2 ~~ SE2_1"
pathmi4<-"SS1_3 ~~ SS2_3"
pathmi5<-"SS1_1 ~~ SS2_1"
pathmi6<-" EB2 =~ SE2_2"
pathmi7<-"EB1 =~ SE2_2"

measuremodel2<-paste(path1,path2,path3,path4,path5,path6,pathmi1,pathmi2,pathmi3,pathmi4,pathmi5,pathmi6,pathmi7,sep  = "\n")
model3<-paste(measuremodel2,structuralmodel2,collapse  = "\n")
fit3<-sem(model = model3,sample.cov = covmat,sample.nobs = n)
summary(fit3,fit.measures=T,standardized=T,rsquare=T)

semPaths(fit3,whatLabels = "std")
