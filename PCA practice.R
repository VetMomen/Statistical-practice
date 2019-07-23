#this is for practicing PCA as unsupervised machine learning 

#loading libraries

library(caret)
library(tidyverse)
library(readxl)
library(FactoMineR)

#loading the data

pcadat<-read_excel("./data sets/Food PCA.xlsx")

#looking at the structure

str(pcadat)

#extracting the numeric variables 

pcanum<-pcadat%>%select(-ID,-FoodGroup,-ShortDescrip,-Descrip,-CommonName,-MfgName,-ScientificName)


pca<-PCA(pcanum,scale.unit = T)

barplot(pca$eig[,1],pca$eig[,2])

summary(pca)

plot(pca)

dimdesc(pca)

scree(pca)
