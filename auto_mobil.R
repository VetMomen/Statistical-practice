auto_sympol<-read_csv(file = 'D:/mo2men/DATA SCIENCE/R/data sets/otomobil.csv')

#Naming the data 

names(auto_sympol)<-c('symboling','normalized-losses','make','fuel-type'
                       ,'aspiration','num-of-doors','body-style','drive-wheels'
                       ,'engine-location','wheel-base','length','width','height'
                       ,'curb-weight','engine-type','num-of-cylinders','engine-size'
                       ,'fuel-system','bore','stroke','compression-ratio','horsepower'
                       ,'peak-rpm','city-mpg','highway-mpg','price')



str(auto_sympol)

#some classes modification

auto_sympol<-auto_sympol%>%mutate(`normalized-losses`=as.numeric(`normalized-losses`),
                                  bore=as.numeric(bore),
                                  stroke=as.numeric(stroke),
                                  horsepower=as.numeric(horsepower),
                                  `peak-rpm`=as.numeric(`peak-rpm`),
                                  price=as.numeric(price))


auto_sympol<-apply(X = auto_sympol,2,FUN = function(x){
        str_remove_all(string = x,pattern = '\\?')
})

auto_sympol<-data.frame(auto_sympol)

uniques<-apply(auto_sympol,MARGIN = 2,FUN = unique)

#plotting 
xtab<-table(auto_sympol$make,auto_sympol$body.style,auto_sympol$fuel.type)
xtab<-as.data.frame(xtab)

ggplot(data = xtab)+geom_mosaic(aes(weight=Freq,x=product(Var1,Var2)))
