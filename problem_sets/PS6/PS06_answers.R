#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest) 
library(gam) 
library(nnet)
library(MASS)



# set working directory
setwd("C:/Users/camer/OneDrive/Documents/GitHub/QTM200Spring2020/problem_sets/PS6")

#####################
# Problem 1
#####################

cholesterol <- read.csv("cholesterol.csv", stringsAsFactors = F, header=T) 

#Question 1 
#Part a:Fit an additive model. Provide the summary output, the global null hypothesis, and p-value. Please describe the results and provide a conclusion.

#H0: the amount of fat consumed and one's sex have no effect on whether someone has cholesterol, Ha: the amount of fat consumed and one's sex do have an effect on whether or not someone has cholesterol

model1<-glm(cholCat~fat + sex, data=cholesterol)
summary(model1) 

#Since both variables are significant with p-values less than .05 (and less than .01), we reject H0 in favor of Ha. It appears that both the amount of fat consumed and one's gender have an effect on whether someone has cholesterol

#Question 2 
#Part a: For women, how does increasing their fat intake by 1 gram per day change their odds on being in the high cholesterol group? (Interpretation of a coefficient)

#For women, increasing their fat intake by 1 gram per day increases their odds of being in the high cholesterol group by about .8% 

#part b: For men, how does increasing their fat intake by 1 gram per day change their odds on being in the high cholesterol group? (Interpretation of a coefficient)

#For men, increasing their fat intake by 1 gram per day changes their odds on being in the high cholesterol group by about 19.7%

#part c: What is the estimated probability of a woman with a fat intake of 100 grams per day being in the high cholesterol group?

#The estimated probability is about 80% 

#part d: Would the answers to 2a and 2b potentially change if we included the interaction term in this model? Why?
model2<-glm(cholCat~fat + sex + fat:sex, data=cholesterol)
summary(model2) 
#No, because the interaction term is not significant

#####################
# Problem 2
#####################

gdpChange <- read.csv("gdpChange.csv", stringsAsFactors = F, header=T) 

#Question 1: Construct and interpret an unordered multinomial logit with GDPWdiff as the output and "no change" as the reference category, including the estimated cutoff points and coefficients.
#create reference category and fix variable types 
gdpChange$EDT2<-as.numeric(gdpChange$EDT)
gdpChange$GDPWdiff2<-as.factor(gdpChange$GDPWdiff)
gdpChange$GDPWdiff3 <- relevel(gdpChange$GDPWdiff2, ref = "no change")
#run regression
model3<-multinom(GDPWdiff3~REG + OIL + EDT2,data=gdpChange)
summary(model3)
#Interpretation: Not being a democracy increased the log odds of having negative growth vs. no change, as did exporting a lot of oil and having a more educated labor force. Meanwhile being a democracy increased the log odds of a country experiencing positive growth vs. no change, as did exporting a lot of oil (although not as much as with negative growth) and having a more educated workforce (more so this time). Since being a democracy was the only variable that is a different sign between positive and negative gdp growth, it is the only variable from this model that can be said to conclusively affect GDP growth.  

#Question 2: Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including the estimated cutoff points and coefficients.
model4<-polr(GDPWdiff3~REG + OIL + EDT2,data=gdpChange, Hess=T)
summary(model4)
#Interpretation: here we see that being a democracy, not being a major oil exporter, and having a more educated workforce all increase the odds of having positive GDP growth