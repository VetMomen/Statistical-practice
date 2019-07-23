#practice helmert contrast



library(contrast)
hsb2 = read.table('https://stats.idre.ucla.edu/stat/data/hsb2.csv', header=T, sep=",")

#creating the factor variable race.f
hsb2$race.f = factor(hsb2$race, labels=c("Hispanic", "Asian", "African-Am", "Caucasian"))

tapply(hsb2$write, hsb2$race.f, mean)

helmat<-c(1,rep(-1/3,3),0,1,rep(-1/2,2),0,0,1,-1)%>%matrix(ncol = 3)

contrasts(hsb2$race.f)<-helmat

