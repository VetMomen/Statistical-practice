perm_test<-function(x,n1,n2){
        n<-n1+n2
        idx_n1<-sample(1:n,n1,replace = F)
        idx_n2<-setdiff(1:n,idx_n1)
        mean_diff<-mean(x[idx_n1])-mean(x[idx_n2])
        return(mean_diff)
}

table_table<-matrix(c(200,23593,128,22406),byrow = F,nrow = 2,ncol = 2)

dimnames(table_table)<-list(c('+','-'),c('A','B'))

data<-c(rep(1,45945),rep(0,382))

perm_diffr<-rep(0,1000)

for(i in 1:1000){
        perm_diffr[i]<-perm_test(x = data,n1 = 23593,n2 = 22406)
}
obs_pct_diff<-(200/23739 - 182/22588)
hist(perm_diffr)
abline(v = obs_pct_diff)
