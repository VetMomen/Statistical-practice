library(readxl)

all<-1:(6*8)

set.seed(776)

memo<-function(out,all,n){
        set.seed(342)
        say<-(sample(all,n,replace = F))
        quart<-say%%8
        jus2<-((say-quart)/8)+1
        return(list(say=say,J=jus2,Q=quart))
}


say<-memo(out = ,all = all,n = 48)
print(say)

memo<-bind_rows(say)
write_csv(memo,"/home/debian/Downloads/memo.csv")
