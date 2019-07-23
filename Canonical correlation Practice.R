library(lavaan)
library(tidyverse)
library(CCA)
library(CCP)
library(candisc)
library(GPArotation)

#canonical correlation problem practice

mm <- read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", 
                  "Science", "Sex")
summary(mm)

psych <- mm[, 1:3]
acad <- mm[, 4:8]

matcor(psych,acad)


cc<-candisc::cancor(psych,acad,standardize=T)
summary(cc)

cc$structure$X.xscores


redundancy(cc)

coef(cc)

plot(cc)

#rotated canonical correlation
Varimax(cc$structure$X.xscores)
Varimax(cc$structure$Y.xscores)
Varimax(cc$structure$X.yscores)
Varimax(cc$structure$Y.yscores)


