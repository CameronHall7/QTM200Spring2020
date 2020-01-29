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

# set working directory
setwd("~C:/Users/camer/OneDrive/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98) 

#Find 90% confidence interval assuming normality 
#Find z score
z90 <- qnorm((1-.9)/2, lower.tail = FALSE)

#Find sample mean
sample_mean<-mean(y)

#Find sample standard deviation 
sample_sd<-sd(y)

#Find upper and lower bounds of confidence interval 
upper<-sample_mean + (z90*(sample_sd/sqrt(25))) 
lower<-sample_mean - (z90*(sample_sd/sqrt(25))) 

#Final confidence interval 
CI90<-c(upper, lower)
CI90

#Confidence Interval: (94.133, 102.747) 
#Interpretation: when taking random samples of the population, the mean IQ scores of these samples will fall between 94.133 and 102.747 90% of the time.


#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Assumptions met: continuous scale, random sampling (albeit with small issues), population normally distributed, n not quite over 30 but close

#H0: mu<=100, Ha: mu>100 

# one sided t test

t.test(y, mu=100, alternative="greater",conf.level=.95) 

#With a p value of .7215 and an alpha value of .05, we fail to reject H0. It is entirely possible that we could have obtained this sample mean given a random sample of the entire population, indicating that this school's students likely do not have higher IQs on average.


#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)

expenditure <- read.table("expenditure.txt", header=T) 

#Load data 
expenditure<-read.table("C:/Users/camer/OneDrive/Documents/Emory/Emory Classes/QTM 200/expenditure.txt", header=T)

#Plot X1, X2, and X3 against Y
plot(expenditure$X1, expenditure$Y) 
#Correlation: strong positive almost linear (.9)
plot(expenditure$X2,expenditure$Y)
#Correlation: none (0) 
plot(expenditure$X3, expenditure$Y)
#Correlation: very weak positive linear (.1) 

#Plot relationship between Y and region 
plot(expenditure$Region, expenditure$Y)
#The West has the highest per capita expenditure on education on average 

#Plot relationship between X1 and Y with different colors and shapes for different regions 
plot(expenditure$X1,expenditure$Y,col=expenditure$Region,pch=expenditure$Region) 
legend("topleft",legend=paste(c("Northeast","North Central","South","West")),col=1:4,pch=1:4)
#Overall, the relationship has a strong, positive, linear correlation, but on the regional level this is really only true for the South and West. In the Northeast the relationship is much weaker and in the North Central region there is no correlation at all.                                                                                           

