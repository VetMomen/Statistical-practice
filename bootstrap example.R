loans_income<-sample(100:200,50,replace = T)
library(boot)

stat_fun<-function(x,idx) mean(x[idx])
boot_obj<-boot(loans_income,R = 1000,statistic = stat_fun)

print(boot_obj)


