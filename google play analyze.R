if(!file.exists('data sets')){
        dir.create('data sets')
}
download.file(url = 'https://www.kaggle.com/lava18/google-play-store-apps/downloads/google-play-store-apps.zip/5',destfile = './data sets/play_store.zip')


#reading the data set
play_store<-read_csv(file = './data sets/googleplaystore.csv')


#function for removing M from size col.
num_extract<-function(x){
        ss<-str_extract_all(x,'([0-9]{1,3}\\.[0-9]{1,2})|[0-9]{1,3}',simplify = T)
        ss<-c(ss)
        return(ss)
}

#function for removing k from size col.

num_extract2<-function(x){
        ss<-str_extract_all(x,'([0-9]{1,4}\\.[0-9]{1,4})|[0-9]{1,5}',simplify = T)
        c(ss)
        return(ss)
}

#Function to apply the two function on the entire col 
size<-function(x){
        z<-c()
        y<-c()
        for(i in 1:length(x)){
                if(str_count(x[i],'M')>0){
                        z[i]<-num_extract(x[i])
                        z[i]<-as.numeric(z[i])
                }else if(x[i]=='Varies with device'){
                        x[i]<-NA
                }else if(x[i]=="1,000+"){
                        x[i]<-NA
                }else{
                        z[i]<-num_extract2(x[i])
                        z[i]<-as.numeric(z[i])/1000
                }
        }
        print(z)
}

#the data sets without char. in col Size
play_store<-play_store%>%mutate(Size=size(Size))

head(play_store)

#the data set with out + in the installs col
play_store<-play_store%>%mutate(Installs=str_remove_all(Installs,'\\+'))

#converting the last update to date class 
play_store<-play_store%>%mutate(`Last Updated`=mdy(`Last Updated`))

#catigory frequancy
play_store%>%
        select(Category)%>%group_by(Category)%>%summarize(n=n())%>%
        ggplot(aes(x = reorder(Category,-n),y = n))+
        geom_col()+
        theme(axis.text.x = element_text(angle = 45,vjust = c(.6)))

#relation between frequancy and No of downloads 
install.packages('ggmosaic')
library(ggmosaic)
ggplot(data = play_store) +
        geom_mosaic(aes(x = product(Category, Installs), fill=Category))+
        scale_fill_hue(na.value = 'black')+
        scale_y_discrete(label=NULL)+
        theme(axis.text = element_text(angle = 45))

cat_freq<-play_store%>%group_by(Category)%>%summarize(n=n())
inst_freq<-play_store%>%group_by(Installs)%>%summarize(n=n())

