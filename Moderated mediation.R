

#This is a tutorial for Moderation analysis 

#For any advice or recommendation plz feel free to contact me at : vet.m.mohamed@gmail.com

#creating the data sets

set.seed(42) #This makes sure that everyone gets the same numbers generated through rnorm function

a1 = -.59 #Set the path a1 strength (effect of X on M)

a2 = -.17 #Set path a2 strength (effect of Z on M)

a3 = .29 #Set path a3 strength (interaction between X and Z on M)

b = .59 #Set path b strength (effect of M on Y)

cdash1 = .27 #Set path c'1 strength (effect of X on Y)

cdash2 = .01 #Set path c'2 strength (effect of Z on Y)

cdash3 = -.01 #Set path c'3 strength (interaction betwee X and Z on Y)
#Here we are creating the values of our variables for each subject

n <- 200 #Set sample size

X <- rnorm(n, 7, 1) #IV: Time spent in grad school (M = 7, SD = 1)

Z <- rnorm(n, 5, 1) #Moderator: Time spent (hours per week) with Professor Demos in class or in office hours (M = 5, SD = 1)

M <- a1*X + a2*Z + a3*X*Z + rnorm(n, 0, .1) #Mediator: Number of publications in grad school
#The mediator variable is created as a function of the IV, moderator, and their interaction with some random noise thrown in the mix

Y <- cdash1*X + cdash2*Z + cdash3*X*Z + b*M + rnorm(n, 0, .1) #DV: Number of job offers
#Similar to the mediator, the DV is a function of the IV, moderator, their interaction, and the mediator with some random noise thrown in the mix
#Now we put it all together and make our data frame

Success.ModMed <- data.frame(jobs = Y, time = X, pubs = M, alex = Z) #Build our data frame and give it recognizable variable names


#scaling the data 

scaled<-apply(Success.ModMed,2,function(x){
        scale(x,center = T,scale = F)
})%>%data.frame()

library(lavaan)

model<-"
#regressions
jobs~a1*time+a2*pubs+a3*alex+int1*time:alex

pubs~b1*time+b2*alex+int2*time:alex

#mean of alex
alex~mean*1

#variance of alex
alex~~var*alex

#indirect effect
ind_high:=(b1+int2*(mean+sqrt(var)))*a2

ind_low:=(b1+int2*(mean-sqrt(var)))*a2

#direct effect
dir_high:=a1+int1*(mean+sqrt(var))
dir_low:=a1+int1*(mean-sqrt(var))


#total effect

total_high:=ind_high+dir_high
total_low:=ind_low+dir_low

#propability med effect

prop_med_high:=ind_high/total_high
prop_med_low:=ind_low/total_low

#index of moderated mediation

ind_nod_med:=a2*int2

"

modmed<-cfa(model,scaled,se="bootstrap",bootstrap=50)

summary(modmed,standardized=T)

