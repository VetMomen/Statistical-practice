correction<-function(N,n,p,t){
        num<-(N-2)-(2*(n-1)*p)
        denum<-(N-2)*(1+((n-1)*p))
        ration<-num/denum
        correctF<-sqrt(ration)
        corrected_type_I=correctF*t
        probt<-pt(q = t,df = denum)
        return(list(CorrectF=correctF,
                    probt))
}

correction(N = 50,n=10,p = .2,t = 2.72)
