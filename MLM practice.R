library(lme4)
library(tidyverse)

mat<-c(1,1,23,33,0,
       2,1,22,33,0,
       3,1,20,27,0,
       4,1,19,25,0,
       5,2,16,22,0,
       6,2,17,21,0,
       7,2,18,28,0,
       8,2,19,31,0,
       9,3,25,28,0,
       10,3,28,38,0,
       11,3,29,35,0,
       12,3,31,34,0,
       13,4,27,38,0,
       14,4,23,27,0,
       15,4,22,28,0,
       16,4,21,25,0,
       17,5,32,28,0,
       18,5,31,37,0,
       19,5,28,33,0,
       20,5,26,30,0,
       21,6,13,27,1,
       22,6,12,22,1,
       23,6,14,34,1,
       24,6,15,28,1,
       25,7,16,30,1,
       26,7,17,37,1,
       27,7,14,27,1,
       28,7,12,25,1,
       29,8,11,28,1,
       30,8,10,23,1,
       31,8,20,34,1,
       32,8,15,33,1,
       33,9,21,29,1,
       34,9,18,31,1,
       35,9,19,30,1,
       36,9,23,39,1,
       37,10,18,27,1,
       38,10,17,36,1,
       39,10,16,36,1,
       40,10,23,32,1)%>%matrix(ncol = 5,byrow = T)





colnames(mat)<-c("cleintid","cluid","empathy","contentment","method")

data<-mat%>%data.frame()

clcontenmean<-data%>%group_by(cluid)%>%summarize(meancontent=mean(contentment))
data<-data%>%left_join(clcontenmean,by="cluid")


data<-data%>%group_by(cluid)%>%mutate(groupcontent=contentment-meancontent,
                                      meanempathy=mean(empathy))



#Empathy_ij ~ B_0j+r_ij --> unconditional ant first level

#B_0j ~ Y_00+ Y_01(method_ij) + u_01

#Finally : Empathy_ij ~ Y_00 + y_01(method_ij) + u_0j + r_ij

uncmod<-lmer(empathy~method+(1|cluid),data,REML = T)
summary(uncmod)

#Variance partitioning factor

VPF<-function(tau,sigma){
        vpf<-tau/(tau+sigma)
        return(vpf)
}

VPF(tau = 14.967,sigma = 6.758)

predict(uncmod)%>%View


#random intercept model

#Empathy_ij ~ B_0j + B_1j(contentment) + r_ij
#B_0j ~ Y00 + Y_01(method) + Y_02(meancontent) + u_0j
#B_1j ~ Y_10

#Finally: Empathy_ij~ Y00 + Y_01(method) + Y_02(meancontent) +Y_10(groupcontent) + u_0j + r_ij

mod<-lmer(empathy~method+meancontent+groupcontent+(1|cluid),data)

summary(mod)
VarCorr(mod)
deviance(mod)
formula(mod)
model.frame(mod)
profile(mod)

