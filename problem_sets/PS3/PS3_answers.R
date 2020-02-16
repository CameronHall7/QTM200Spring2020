#load data 
incumbent<-read.csv("C:/Users/camer/OneDrive/Documents/GitHub/QTM200Spring2020/problem_sets/PS3/incumbents_subset.csv")
incumbent


#Question 1 

#Run regression
regresults1<-lm(voteshare~difflog,data=incumbent)
summary(regresults1) 

#Make scatterplot with regression line 
plot(incumbent$difflog,incumbent$voteshare) 
abline(regresults1,col="red",lwd=3)

#save residuals 
residuals1<-resid(regresults1)
residuals1

#prediction equation 
#y=.579 + .042x 


#Question 2 

#run regression 
regresults2<-lm(presvote~difflog,data=incumbent) 
summary(regresults2) 

#make scatterplot with regression line
plot(incumbent$difflog,incumbent$presvote) 
abline(regresults2,col="red",lwd=3) 

#save residuals 
residuals2<-resid(regresults2) 
residuals2 

#prediction equation 
#y=.508 + .024x 


#Question 3 

#run regression 
regresults3<-lm(voteshare~presvote,data=incumbent) 
summary(regresults3) 

#make scatterplot with regression line
plot(incumbent$presvote,incumbent$voteshare) 
abline(regresults3,col="red",lwd=3) 

#prediction equation 
#y=.441 + .388x 


#Question 4 

#run regression
regresults4<-lm(residuals1~residuals2) 
summary(regresults4) 

#make scatterplot with regression line 
plot(residuals2,residuals1) 
abline(regresults4,col="red",lwd=3) 

#prediction equation 
#y=-4.860x10^-18 + .257x


#Question 5 

#run regression 
regresults5<-lm(voteshare~difflog + presvote,data=incumbent)
summary(regresults5)

#write prediction equation 
#y=.449 + .036x1 + .257x2 (where x1 is difflot and x2 is presvote) 

#What is it in this output that is identical to the output in Question 4? Why do you think this is the case? 
#The coefficient value for the residuals from the first model in Question 4 is the same as the coefficient value for presvote in this question. Since these residuals tell us how much variation in voteshare is not explained by difflog, it makes sense that this unexplained variation is coming from presvote, which is included in this model but not the earlier one that generated the residuals. 





