library(tidyverse)
library(psych)
library(lavaan)

cor<-c(1.000,.659,.596,
.659,1.000,.628,
.596,.628,1.000)%>%matrix(,ncol = 3,byrow = T)

colnames(cor)<-rownames(cor)<-c("Stimulate","Challenge","Interest")


pc<-pca(cor,n.obs = 300)
pc$Structure
pc$communality%>%mean

##########################################################

lower<-c(1.000,
.659,1.000,
.596,.628,1.000,
.177,.111,.107,1.000,
.112,.109,.116,.701,1.000,
.140,.104,.096,.619,.673,1.000)

cor<-lav_matrix_lower2full(lower)

colnames(cor)<-rownames(cor)<-c("simulate","challenge","interest","recognize","appreciate","compancate")

print(cor)

para<-fa.parallel(cor,n.obs = 300,fa = "fa",fm = "pa")

para$pc.sim

efaorth<-fa(r = cor,n.obs = 300,rotate = "quartimax",fm = "pa",nfactors = 2)

factor.scores(cor,efaorth)

efaobl<-efaorth<-fa(r = cor,n.obs = 300,rotate = "quartimin",fm = "pa",nfactors = 2,oblique.scores = T,scores = "regression")

scree(efaobl)


efaorth$Structure

efaobl$R2.scores
efaobl$loadings

factor.scores(x = cor,f = efaobl)

efaobl$e.values
efaobl$values

fa.diagram(efaobl)
fa.diagram(efaorth)


#############################################################

corlow<-c(1.000,
.657,1.000,
.652,.660,1.000,
.279,.338,.300,1.000,
.290,.330,.350,.644,1.000,
.358,.462,.440,.659,.566,1.000,
.076,.093,.120,.317,.313,.367,1.000,
.003,.035,.097,.308,.305,.329,.612,1.000,
.026,.100,.097,.305,.339,.313,.674,.695,1.000,
.287,.312,.459,.271,.307,.351,.122,.137,.185,1.000,
.355,.377,.489,.261,.277,.369,.196,.191,.197,.367,1.000,
.441,.414,.522,.320,.275,.383,.170,.156,.101,.460,.476,1.000)

cor<-lav_matrix_lower2full(corlow)

colnames(cor)<-rownames(cor)<-c("ten1","ten2","ten3","wor1","wor2","wor3","tirt1","tirt2","tirt3","bod1","bod2","bod3")


apply(cor,1,function(x){
        ifelse(x>=.9&x<1,"error","OK")
})

KMO(cor)

cortest.bartlett(cor,n = 318)


fa.parallel(x = cor,n.obs = 318,fm = "pa",fa = "fa")

factor<-fa(r = cor,nfactors = 4,n.obs = 318,rotate = "quartimin",scores = "regression",oblique.scores = T)

loading<-factor$loadings%>%matrix()%>%apply(MARGIN = 2,function(x){
        ifelse(x<=.35,"",x)
})%>%matrix(ncol = 4,dimnames = list(c(names(cor)),c(paste("ML",1:4,sep = " "))))


factor$communality%>%mean()

factor$loadings

factor$uniquenesses%>%mean()

#################################################################


