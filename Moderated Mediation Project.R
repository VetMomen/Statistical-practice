
#This is a tutorial for moderated mediation - Full illustration

#plz feel free for any recommendation and advice to contact me at : vet.m.mohamed@gmail.com

#i will use these following packages 

library(lavaan)
library(tidyverse)
library(semPlot)


#Now lets load the data jobs 

data(jobs)
head(jobs)%>%View

#splitting the variables which included in our analysis

data<-jobs%>%dplyr::select(sex,econ_hard,age,job_seek,treat,depress2)

View(data)

#This data is about the effect of job seeking 
#training on the job seeking and mental health of unembloyed people 

#we suppose here that the job seeking variable mediate the relationship 
#between the treatment and depression2

#we will use lavaan package to test this mediation 

#bit first of all i want to convert all catigorical variables to number factor and center all numeric variables 

#So lets look at the structure and run the modifications

str(data)

data<-data%>%mutate(sex=factor(sex,levels = c(0,1),labels = c(0,1)),
                    treat=factor(treat,levels = c(0,1),labels = c(0,1)),
                    econ_hard=scale(econ_hard,center = T,scale = F),
                    age=scale(age,center = T,scale = F),
                    depress2=scale(depress2,center = T,scale = F))

#Then we will use lavaan package to build the model and build it 

model<-"

#Regressions

depress2 ~ a1*treat + M*job_seek + a2*econ_hard + a3*sex + a4*age
job_seek ~ b1*treat + b2*econ_hard + b3*sex + b4*age

#indirect effect
ind:=M*b1

#total effect
total:=a1+(M*b1)

#propapility

prop_med:=ind/total

"

