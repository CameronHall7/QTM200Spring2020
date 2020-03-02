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

lapply(c("faraway"),  pkgTest) 
library(car)


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble) 

#Problem 1a 

plot(model1) 
#The variance is more or less constant, as evidenced by the fact that the residuals average to about 0 at each fitted value. However, it is not perfect. There also appear to be three notable outliers. 

#Problem 1b 

plot(model1)
#The data appears to be generally normally distributed at each value of x, but the same three outliers appear again. 

#Problem 1c 

plot(hatvalues(model1)) 
abline(h=2*5/47) 
abline(h=3*5/47)

#There are four points with high hat values that have high leverage and thus potential to influence the model

#Problem 1d 

outlierTest(model1) 
#Given the very small Bonferroni p value (1.9289*10^-5), it we reject the null hypothesis that there are no outliers, because the probability of getting these results if there were no outliers is extremely low

#Problem 1e 

plot(hatvalues(model1),rstudent(model1),type="n")
cook<-sqrt(cooks.distance(model1))
points(hatvalues(model1),rstudent(model1),cex=10*cook/max(cook)) 
abline(h=c(-2,0,2))
abline(v=c(2,3)*3/45)
#There is point with a very large large Cook's distance and studentized residual, indicating that despite its relatively unremarkable hat value it is quite influential. Otherwise, there are several other points that have either a large studentized residual or a large hat value but never both (and usually a fairly reasonable Cook's distance), indicating that none of these outliers is highly influential. 
