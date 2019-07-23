#function to calculate F from R2

fcalc<-function(R2,k,n){
  return((R2/k)/((1-R2)/(n-k-1)))
}


fcalc(R2 = .346,k = 1,n = 68)



