library(tidyverse)
library(car)

ami_data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
names(ami_data) <- c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")

summary(ami_data)
pairs(ami_data)

mlm1 <- lm(cbind(TOT, AMI) ~ GEN, data = ami_data)
summary(mlm1)

coef(mlm1)

vcov(mlm1)%>%View

Manova(mlm1,type="III")

mlm2<-update(mlm1,.~.- PR - DIAP - QRS)

anova(mlm1,mlm2)

lh.out <- linearHypothesis(mlm1, hypothesis.matrix = c("GEN=0"))

lh.out
