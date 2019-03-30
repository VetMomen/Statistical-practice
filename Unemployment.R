#loading the libraries

library(tidyverse)
library(readxl)

#loading the data 

data<-read_excel("./data sets/Cleaned Data.xlsx")


#need to rename the data 

long_names<-names(data)

names(data)<-c("emp","m_illness","edu","owen_comp","hosp","d_hosp",
               "disabled","net_access","parents","gap","gap_length",
               "income","unem","read","welfare","food_stamp","sec8","hos_time",
               "lake_conc","anx","dep","obsess_think","mood","panic","comp_beh",
               "tire","age","gendr","hous_inc","region","device")

#seeing the class of each column

str(data)

#converting some variables to factors

data<-data%>%mutate(emp=factor(emp),
                    m_illness=factor(m_illness),
                    edu=factor(edu),
                    owen_comp=factor(owen_comp),
                    hosp=factor(hosp),
                    disabled=factor(disabled),
                    net_access=factor(net_access),
                    parents=factor(parents),
                    gap=factor(gap),
                    unem=factor(unem),
                    read=factor(read),
                    lake_conc=factor(lake_conc),
                    anx=factor(anx),
                    dep=factor(dep),
                    obsess_think=factor(obsess_think),
                    mood=factor(mood),
                    panic=factor(panic),
                    comp_beh=factor(comp_beh),
                    tire=factor(tire),
                    age=factor(age),
                    gendr=factor(gendr),
                    hous_inc=factor(hous_inc),
                    region=factor(region),
                    device=factor(device),
                    food_stamp=factor(food_stamp),
                    sec8=factor(sec8))

#Looking again at the structure 

str(data)

#we see here that most of data is categorical and factors 

#Lets wisualize our data for inspection and releasing the patterns in it 

library(ggplot2)

#the most important variable here is the employability 
#so lets look at it as bar plot

data%>%ggplot(aes(x = emp,color=emp))+geom_bar(fill="white",width = .4)+
        xlab("Employability")

#here we can see the the most of our sample is employed 
#but we need to ask some questions about the relationship between this variable and others

#lets look for example to the mental Illness 

data%>%group_by(emp,m_illness)%>%summarize(percent=round((n()/nrow(data))*100,2))%>%
        ggplot(aes(x=emp,y = percent,color=emp))+geom_col(fill="white",width = .4)+
        xlab("Employability")+
        facet_grid(emp~m_illness)+
        geom_label(aes(label=percent),show.legend = F)

#here we can see that 52% of our sampls are employed and they are not assigned as mentally illed 
#only 14% employed and they assigned as mentally illed 
#note here that there are a high percent of unemployed persons and they didn't have any issue 
#the high percent in unemployed and no issue 
#and also employed with issue open a new investigation in front of us to explore 

#we will focus on those to see their characteristics 

#lets separate the employed with mental illness 

emp_ill<-data%>%filter(emp==1,m_illness==1)

# ithink that the most variables may affect on the employment is the Education , living with parents , regions

#lets do inspection with education 

emp_ill%>%ggplot(aes(x = edu,fill=edu))+
        geom_bar(width = .4)

#No wander to find some people with master or finished undergraduate levels with high rate of 
#employability , but the surprising part to find those who with some undergraduate degree and high employability 
#so lits test some other variables like the type of illness 


emp_ill%>%ggplot(aes(x = edu,fill=edu))+
        geom_bar(width = .4)+
        facet_grid(edu~lake_conc)+theme(axis.text.x = element_text(angle = 45,vjust = .5))

