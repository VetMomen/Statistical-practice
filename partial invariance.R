
mod<-paste0(c(names(load[,1])),"~",1)

diff<-list()

test1<-cfa(model = esemmodel,
           data = big[,-c(1,ncol(big))],
           group = "Sex",
           group.equal = c("loadings","intercepts"))

for(i in mod){
        test2<-cfa(model = esemmodel,
                   data = big[,-c(1,ncol(big))],
                   group = "Sex",
                   group.equal = c("loadings","intercepts"),
                   group.partial = i)
        diff[[i]]<-fitmeasures(test1,fit.measures = "chisq")-fitmeasures(test2,fit.measures = "chisq")
}

diff<-data.frame(diff)%>%t()
View(diff)
