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

library(lme4) 
library(plm) 
library(car)

# set working directory
setwd("C:/Users/camer/OneDrive/Documents/GitHub/QTM200Spring2020/problem_sets/PS7")

#####################
# Problem 1
#####################


mexico_elections <- read.csv("MexicoMuniData.csv", stringsAsFactors = F, header=T) 

#Part a: Run a Poisson regression because the outcome is a count variable. Is there evidence that PAN presidential candidates visit swing districts more? Provide a test statistic and p-value.

model1<-glm(PAN.visits.06~competitive.district + marginality.06 + PAN.governor.06, family="poisson",data=mexico_elections)
summary(model1)
#There is little evidence that PAN candidates visit swing distrcits more. The p value of .161 would not be significant at any customary level of siginificance. 

#Part b: Interpret the marginality.06 and PAN.governor.06 coefficients 

#On average, an increase of 1 in terms of poverty and having a PAN governor decreased the number of visits to a district by 2.1 and .2 respectively. However the PAN governor variable was not significant 

#Part c: Provide the estimated mean number of visits from the winning PAN presidential candidate for a hypothetical district that was competitive (competitive.district=1), had an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).

#This district would have a mean number of visits of -3.93 -.46 -.21 = -4.6
#####################
# Problem 2
#####################

sleepstudy <- sleepstudy

#Part 1: Create a "pooled" linear model where you regress Days on the outcome Reaction. Make sure to run regression diagnostics to check if the variance around the regression line is equal for every year.
model2<-plm(Reaction~Days,data=sleepstudy,index="Subject",model="pooling") 
summary(model2) 

#Part 2: Fit an "un-pooled" regression model with varying intercepts for patient (include an additive factor for patient) and save the fitted values. 
model3<-lm(Reaction~Days + Subject,data=sleepstudy)
summary(model3)

#Part 3: Fit a "un-pooled" regression model with varying slopes of time (days) for patients (include only the interaction Days:Subject) and save the fitted values.
model4<-lm(Reaction~Days:Subject,data=sleepstudy)
summary(model4)

#Part 4: Fit an "un-pooled" regression model with varying intercepts for patients with varying slopes of time (days) by patient (include the interaction and constituent terms of Days and Subject, Days + Subject + Days:Subject) and save the fitted values.
model5<-lm(Reaction~Days + Subject + Days:Subject,data=sleepstudy)
summary(model5)

#Part 5: Fit a "semi-pooled" multi-level model with varying intercept for subject and varying slope of day by subject. Is it worthwhile for us to run a multi-level model with varying effects of time by subject? Why? Compare your model from part 5 to the other completely "pooled" or "un-pooled models". 
model6<-lm(Reaction~Days + Subject + Subject:Days,data=sleepstudy)
summary(model6)
#It is worthwhile to examine varying effects of time by subject because other factors that are not included in the model could make it so that time affects some subjects more than others. Time could also just affect subjects' sleep differently based on their biology. This is evident from the fact that in this model the various subjects are mostly not significant while the interaction is really what is significant. 

