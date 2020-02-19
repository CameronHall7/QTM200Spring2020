#Question 1 
#install packages 

install.packages("car")
library(car)
data(Prestige)
help(Prestige) 

#1a: Create a new variable professional by recoding the variable type so that professionals are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse.)

library(dplyr)
Prestige1<-Prestige %>% mutate(type=ifelse(Prestige$type=="prof",1,0))

#1b: Run a linear model with prestige as an outcome and income, professional, and the interaction of the two as predictors (Note: this is a continuous x dummy interaction.)

Model1<-lm(prestige~income + type + income:type,data=Prestige1)
summary(Model1)

#1c: Write the prediction equation based on the result. 

#prestige = 21.142 + .003(income) + 37.781(type) -.002(income)(type) 

#1d: Interpret the coefficient for income. 

#For every increase in average income of $1, the prestige score of one's occupation increases by .003 on average 

#1e: Interpret the coefficient for professional 

#Being a professional increases the prestige score of one's occupation by 37.781 on average in comparison to being white collar or blue collar

#1f: What is the effect of a $1,000 increase in income on prestige score for professional occupations? In other words, we are interested in the marginal effect of income when the variable professional takes the value of 1. Calculate the change in ^y associated with a $1,000 increase in income based on your answer for (c). 

prestige1f<-21.142 + .003*(1000) + 37.781 - .002*(1000)
prestige1f
#A $1000 increase in income for professional occupations results in an average increase in prestige score of 59.923 

#1g: (g) What is the effect of changing one's occupations from non-professional to professional when her income is $6,000? We are interested in the marginal effect of professional jobs when the variable income takes the value of 6; 000. Calculate the change in ^y based on your answer for (c).

prestige1g<-(21.142 + .003*(6000) + 37.781 -.002*(6000)) - (21.142 + .003*(6000))
prestige1g
#Changing one's occupation from non-professional to professional at an income of $6000 results in an average increase in prestige score of 25.781



#Question 2 

#2a: (a) Use the results to determine whether having these yard signs in a precinct affects vote share (e.g., conduct a hypothesis test with alpha = :05).
test.stat2a<-.042/.016 
p2a<-2*pt(test.stat2a,127,lower.tail=F) 
p2a # = .010

#Since the p value for having yard signs is less than .05, we have suffient evidence to reject the null hypothesis. Therefore we can reasonably infer that yard signs do affect vote share. 

#2b: (b) Use the results to determine whether being next to precincts with these yard signs affects vote share (e.g., conduct a hypothesis test with alpha = :05).
test.stat2b<-.042/.013 
p2b<-2*pt(test.stat2b,127,lower.tail=F)
p2b #.002 

#Since the p value for having yard signs in an adjacent precinct is less than .05, we have sufficient evidence to reject the null hypothesis. Therefore we can reasonably infer that year signs in adjacent precincts do affect vote share. 

#2c: Interpret the coefficient for the constant term substantively. 

#In precincts that neither had yard signs themselves nor were adjacent to precincts with yard signs, the average vote share for Cuccinelli was .302. 

#2d: Evaluate the model fit for this regression. What does this tell us about the importance of yard signs versus other factors that are not modeled?

#Since the r^2 value is only .094, this model only explains 9.4% of the variation in vote share. This means that there are a lot of other variables that influence vote share, which likely have a much larger effect.