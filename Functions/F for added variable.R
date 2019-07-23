#F value for added variable incremental validity 


Fincr<-function(R2AB,R2A,kA,kB,n){
  f<-((R2AB-R2A)/kB)/((1-R2AB)/(n-kA-kB-1))
  df<-c(kB,(n-kA-kB-1))
  
  return(list(f=f,df=df))
}

Fincr(R2AB = .682,R2A = .555,kA = 1,kB = 2,n = 57)




