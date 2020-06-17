#Gautchi method

L<-function(xbar,t,var){
        L<-(xbar-t)^2+var
        return(L)
}


x<-c(13.688,13.788,14.173,14.557,
13.925,14.545,13.797,14.778)

xbar<-mean(x)
S<-var(x)


L(xbar = xbar,t = 14.5,var = S)


######################################

run1<-c(14.158,14.754,14.412,14.065,13.802,14.424,14.898,14.187)
run2<-c(13.676,14.177,14.201,14.557,13.827,14.514,13.897,14.278)
run3<-c(13.868,13.898,14.773,13.597,13.628,14.655,14.597,14.978)
run4<-c(13.668,13.788,14.173,14.557,13.925,14.545,13.797,14.778)

runs<-list(run1,run2,run3,run4)

robust<-function(xbar,SS){
        R<-log(xbar^2/SS)
        return(R)
}

sapply(runs,function(x){
        xbar<-mean(x)
        SS<-var(x)
        R<-robust(xbar = xbar,SS = SS)
})
