loans_income<-sample(200:500,100,replace = T)


Sample_data<-data.frame(income=sample(loans_income,1000,replace = T),
                        type='data_dist')

mean_05<-data.frame(income=tapply(sample(loans_income,1000*5,replace = T),
                                  rep(1:1000,rep(5,1000)),mean),
                    type='mean of 5')


mean_20<-data.frame(income=tapply(sample(loans_income,1000*20,replace = T),
                                  rep(1:1000,rep(20,1000)),
                                  mean),
                    type='mean of 20')


income<-bind_rows(Sample_data,mean_05,mean_20)

income<-income%>%mutate(type=factor(type,levels = c("data_dist","mean of 5","mean of 20")))

str(income)

income%>%ggplot(aes(income))+
        geom_histogram(bins = 40)+
        facet_grid(.~type)
