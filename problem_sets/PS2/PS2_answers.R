#Question 1 

#Part a

#calculate expected values 
(27*21)/42 #=13.5 
(27*13)/42 #=8.357 
(27*8)/42 #=5.143 
(15*21)/42 #=7.5 
(15*13)/42 #=4.643 
(15*8)/42 #=2.857 

#calculate test statistic 
chi_squared<-(14-13.5)^2/13.5 + (6-8.357)^2/8.357 + (7-5.143)^2/5.143 + (7-7.5)^2/7.5 + (7-4.643)^2/4.643 + (1-2.857)^2/2.857
chi_squared #=3.791 

#Part b 
#calculate p value 
pchisq(3.791,df=2,lower.tail=FALSE) #p=.150 

#If alpha =.1, we fail to reject that null hypothesis that a driver's class has no bearing on whether or not they are asked for a bribe 

#Part c 
(14-13.5)/sqrt(13.5*(1-27/42)*(1-21/42)) #=.322 
(6-8.357)/sqrt(8.357*(1-27/42)*(1-13/42)) #=-1.642 
(7-5.143)/sqrt(5.143*(1-27/42)*(1-8/42)) #=1.523 
(7-7.5)/sqrt(7.5*(1-15/42)*(1-21/42)) #=-.322 
(7-4.643)/sqrt(4.643*(1-15/42)*(1-13/42)) #=1.642 
(1-2.857)/sqrt(2.857*(1-15/42)*(1-8/42)) #=-1.523 

#create table 

table1results<-matrix(c(.322,-1.642,1.523,-.322,1.642,-1.523),ncol=3,byrow=TRUE) 
rownames(table1results)<-c("Upper Class","Lower Class")
colnames(table1results)<-c("Not Stopped","Bribe Requested","Stopped/Given Warning")
table1<-as.table(table1results)
View(table1)

#Part d 
#They provide a sense of how far each estimate is from the center of the chi squared distribution. Since none of them are that large, we can infer that they are fairly close to this center and thus these results are perfectly reasonable assuming H0 is true.

#Question 2 

#Load data 
library(readr)
women <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(women)

#Part a 

#H0: There is no association between the variables reserved and water 
#HA: There is an association between the variables reserved and water 

#Part b 
meanx<-mean(women$reserved)
meany<-mean(women$water)
sumx<-sum(women$reserved)
sumy<-sum(women$water)

#Estimate beta
beta.numerator<-sum((women$water-meany)*(women$reserved-meanx)) 
beta.denominator<-sum((women$reserved-meanx)^2)
beta.hat<-beta.numerator/beta.denominator 
#beta=9.252

#Solve for alpha 
#meany=a + beta(meanx) 
alpha<-meany-beta*meanx
#alpha = 14.738


#Part c 
#Reserving GP for women leaders on average results in an increase of 9.252 new or repaired drinking water facilities in the village 

#Question 3 

#part a
#import data
fruitfly<-read.csv("GitHub/QTM200Spring2020/problem_sets/PS2/fruitfly.csv") 
summary(fruitfly$lifespan)

#part b
plot(fruitfly$lifespan,fruitfly$thorax)
cor(fruitfly$lifespan,fruitfly$thorax) # r=.636 -- there is a relatively weak positive linear relationship 

#part c 
model1<-lm(fruitfly$thorax~fruitfly$lifespan)
model1
#intercept = .660, #coefficient=.003 
#for every increase in lifespan of 1 day, there is on average an increase in the length of the thorax by .003 mm 
alpha1<-.660 
beta1<-.003


#part d
#find standard deviation
sd_estimate<-sigma(model1)

#calculate standard error
beta_se<-sd_estimate/sqrt(sum((fruitfly$lifespan-mean(fruitfly$lifespan))^2))
beta_se
alpha_se<-sd_estimate*sqrt((1/dim(fruitfly)[1])+(mean(fruitfly$lifespan)^2/sum((fruitfly$lifespan-mean(fruitfly$lifespan))^2)))
alpha_se 

#find p values 
2*pt((beta-0)/beta_se,dim(fruitfly)[1]-2,lower.tail=FALSE) #=~0 
2*pt((alpha-0)/alpha_se,dim(fruitfly)[1]-2,lower.tail=FALSE) #=~0

#check 
summary(model1) #same results 

#Due to the small size of the p values, we can assume that they are significant at any possible level. This means that we reject the null hypothesis that there is no relationship in favor of the alternative hypothesis that there is one

#part e
confint(model1,level=.9)
#in repeated sampling, 90% of the sample slopes will fall between .002 and .003 

#part f 

#fitted model
Fit<-predict(lm(fruitfly$lifespan~fruitfly$thorax),newdata=fruitfly,se.fit=TRUE) 
Fit
#the model predicts a lifespan of 48.641 days 

#confidence interval 
CI<-predict(lm(fruitfly$lifespan~fruitfly$thorax),newdata=fruitfly,interval="confidence",level=.95) 
CI
#the confidence interval is (45.572,51.711) 

#prediction interval
PI<-predict(lm(fruitfly$lifespan~fruitfly$thorax),newdata=fruitfly,interval="prediction",level=.95)  
PI
#the prediction interval is (21.543,75.740) 

#part g
plot(Fit$fit) 
lines(CI)
lines(PI)

 

