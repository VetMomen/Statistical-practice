model1<-data%>%with(
        gls(latency~1,data = data,method = "ML",na.action = na.omit)
)
summary(model1) #fixed intercept

model2<-data%>%with(
        lme(latency~1,random = ~1|partno,method = "ML",na.action = na.omit)
)
summary(model2)#random intercept

anova(model1,model2) #model2 is better that model1

model3<-data%>%with(
        lme(latency ~1,random = list(~1|partno,~1|trialcode),
            data = data,method = "ML",na.action = na.omit))#random intercept with different groups 


anova(model1,model2,model3)

#model3 is useless

model4<-data%>%with(
        lme(latency~response,
            data = data,
            random = ~1|partno,
            method = "ML",na.action = "na.omit")
) #adding predictor and letting intercept to vary with partno

summary(model4) 

anova(model1,model2,model4)

#the fourth model is significantly differ from model 3

#this mean that random intercept in presence of predictor is significant 

#model5 random slope 

model5<-data%>%with({
        lme(latency~response,
            data=data,random = ~response|partno,method = "ML",na.action = "na.omit")
})

summary(model5)

anova(model1,model2,model4,model5) #random slope with fixed intercept is better model with fixed random intercept and fixed slope 


#model6 #random intercept randome slope

model6<-data%>%with(
        lme(latency~response,data=data,method = "ML",na.action = "na.omit",random = list(~1|partno,~response|partno))
)

summary(model6)

anova(model1,model2,model4,model5,model6)
#randome slop and randome intercept doesn't differ from model 5

#so, we will keep model 5 --> random slop only 
