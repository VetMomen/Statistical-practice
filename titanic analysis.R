tanic<-read_csv(file = 'D:/mo2men/DATA SCIENCE/R/data sets/titnic ML/train.csv')

head(tanic)

str(tanic)

uniques<-apply(tanic,2,unique)
tanic<-tanic%>%mutate(Survived=factor(Survived),
                      PassengerId=as.character(PassengerId),
                      Pclass=factor(Pclass)
                      )


#ploting proportion of survived in each class
nro<-nrow(tanic)
tanic%>%group_by(Pclass)%>%summarize(`prop of survived`=sum(as.numeric(Survived),na.rm = T)/nro)%>%
                                             ggplot(aes(x = Pclass,y = `prop of survived`,fill=Pclass))+
                                             geom_col(width = .5)

#the same plotting related devided by other factor "sex"
nro<-nrow(tanic)
tanic%>%group_by(Pclass,Sex)%>%summarize(`prop of survived`=sum(as.numeric(Survived),na.rm = T)/nro)%>%
        ggplot(aes(x = Pclass,y = `prop of survived`,fill=Pclass))+
        geom_col(width = .5)+
        facet_wrap(.~Sex)
#the same plot adding sibling factor
tanic%>%group_by(Pclass,SibSp,Sex)%>%summarize(`prop of survived`=sum(as.numeric(Survived),na.rm = T)/nro)%>%
        ggplot(aes(x = Pclass,y = `prop of survived`,fill=Pclass))+
        geom_col(width = .5)+
        facet_grid(SibSp~Sex)

#ploting boxplot age
mean<-tanic%>%group_by(Pclass,Sex)%>%summarize(mean=mean(Age,na.rm=T))
tanic%>%ggplot(aes(x='',y = Age))+
        geom_boxplot(outlier.colour = 'red')+
        facet_grid(Sex~Pclass)+
        geom_abline(data=mean,aes(intercept = mean,slope = 0),lty='dashed',color='blue',lwd=.7)


tanic%>%ggplot(aes(Age))+
        geom_density(aes(fill=Pclass))+
        facet_wrap(Sex~.)
