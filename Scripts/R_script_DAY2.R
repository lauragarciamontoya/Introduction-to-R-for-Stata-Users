
#Libraries
#install packages if necessary. 
library(ggplot2)
library(datasets)
library(help = "datasets")
library(cowplot)
library(dplyr)
library(broom)
library(margins) 
library(AER)
library(stargazer) #Cool package to format regression output

fahrenheit_to_celsius <- function(temp_F){
  temp_C <- ((temp_F - 32) * (5 / 9)) 
  return(temp_C)
}

fahrenheit_to_kelvin <- function(temp_F){
  temp_K <-(temp_F - 32) * (5 / 9) + 273.15 
  return(temp_K)
}

celsiustokevin <- function(temp_C){
  temp_K <- temp_C - 273.15
  return(temp_K)
}


fahrenheit_to_celsius(40)
fahrenheit_to_kelvin(40)
celsiustokevin(40)

#Or with vectors:
temperatures_F<-c(42,40,35,34,39,34,35, NA)

fahrenheit_to_celsius(temperatures_F)

##Confidence Intervals
confidence_z<-function(estimate,alpha,se)
{
  lower_bound<-estimate-se*qnorm(1-(alpha/2))
  upper_bound<-estimate+se*qnorm(1-(alpha/2))
  return(c(lower_bound, upper_bound))
}


confidence_n<-function(estimate,conf_level,n,se){
  if (n>30) {
    lower_bound<-estimate-se*qnorm(conf_level)
    upper_bound<-estimate+se*qnorm(conf_level)
  } else if (n<=30) {
    lower_bound<-estimate-se*qt(conf_level,n-1)
    upper_bound<-estimate+se*qt(conf_level,n-1)
  }
  return(c(lower_bound,upper_bound))
}


#########################################################################
######################Basic Plots########################################
#########################################################################

#Box Plots
boxplot(data_example$Gini_coef)

boxplot(data_example$aSECOMDURS)

boxplot(data_example$Gini_coef, data_example$aSECOMDURS)

#Histogram
par(mfrow=c(2,2)) #Allows to have plots 
#1
hist(cars$speed) 

#2
hist(cars$speed, breaks=10) 

#3
hist(cars$speed, breaks=10 , xlab = "Speed", main = "Car Speed Distribution")

#4
hist(cars$speed, breaks=10 , xlab = "Speed", main = "Car Speed Distribution", col="lightblue")

#Scatter Plots
par(mfrow=c(2,2)) 

plot(cars$speed~cars$dist,col="blue", xlab="Speed", ylab="Distance")

plot(cars$speed,cars$dist,col="blue", xlab="Speed", ylab="Distance")
lines(cars$dist~cars$speed, col="blue")

plot(cars$speed,cars$dist,col="blue", xlab="Speed", ylab="Distance")
lines(lowess(cars$dist~cars$speed), col="red")

plot(cars$speed,cars$dist,col="black", xlab="Speed", ylab="Distance")
lines(lowess(cars$dist~cars$speed), col="red")
text(18,82, "This is ok", col="Dark Green", pos=3)
text(8,17, "BEST", col="Blue", pos=3)
text(24,119.5, "This is probably an outlier -> ", col="Dark red", pos=2)

#Distribution plots

d_speed<-density(cars$speed)
cars$treatment<-rbinom(50,1,0.4)
d_speed_t<-density(cars$speed[cars$treatment==1])
d_speed_c<-density(cars$speed[cars$treatment==0])
par(mfrow=c(3,1)) 
plot(d_speed , main="Kernel Density of Car Speed", xlab="Car Speed")
polygon(d_speed, col="light blue", border="blue")
plot(d_speed_t , main="Only Treated", xlab="Car Speed")
polygon(d_speed_t, col="light green", border="dark green")
plot(d_speed_c , main="Only Control", xlab="Car Speed")
polygon(d_speed_c, col="light gray", border="dark gray")

#########################################################################
#############GGPLOT######################################################
#########################################################################

#Example 1: using ggplot2
ggplot(diamonds, aes(carat, price, colour = color, shape = cut)) +
  geom_point()

#Example 2: Scatter Plot
ggplot(ChickWeight, aes(weight, Time, colour = "color")) + geom_point()

#Example 3: Chicken Weights
#The function names will allow you to see what variables you have. 
names(ChickWeight)

#Plot
ggplot(ChickWeight, aes(weight)) + geom_histogram() 

#Change colors
ggplot(ChickWeight, aes(weight)) + geom_histogram(color="darkblue", fill="lightblue") 

#Example 4
names(USArrests)

#Create plot
ggplot(USArrests, aes(UrbanPop, Murder, colour="color")) + geom_point()  + geom_text(label=rownames(USArrests), size=3)

#Change color and size
ggplot(USArrests, aes(UrbanPop, Murder, colour="blue", size=2)) 
+ geom_point(colour="blue")  
+ geom_text(label=rownames(USArrests), color="black", size=3)

#Example 5 
ggplot(data=Orange,
       aes(x=age, y=circumference, colour=Tree)) +
  geom_line()


#Example 6 - Density functions
ggplot(diamonds, aes(carat, fill = cut)) +   geom_density(position = "stack")



#########################################################################
#############Linear Models###############################################
#########################################################################


#Linear Model 
linear_model1 <- lm(mpg ~ hp, data=mtcars)  # build linear regression model on full data
print(linear_model1)

#Summary
summary(linear_model1)

#Names:
names(linear_model1)

#Display coefficients
linear_model1$coefficients

#Creates object with coefficients
coeficcients_m1<-linear_model1$coefficients
coeficcients_m1

#Just the intercept
coeficcients_m1[1]

#Just the hp coefficient
coeficcients_m1[2]

#Residuals 
summary(linear_model1)[3]
summary(linear_model1)[[3]][5]

summary(linear_model1)[[3]]["Datsun 710"]


summary(linear_model1)$residuals

  
#R2
summary(linear_model1)$r.squared
summary(linear_model1)$adj.r.squared

#Display all estimates from model
coef(summary(linear_model1))

#Creates an object with all estimates from model
coef_ds<-coef(summary(linear_model1)) #index this object as dataframe. 

#Just the standard errors
coef_se<-coef(summary(linear_model1))[,2] 
coef_se

##5.3 Robust Standard errors
#To estimate robust standard errors you need to use the sandwich package and the lmtest package.

library(lmtest) #Install package before.
library(sandwich)  #Install package before.

coeff_rse<-coeftest(linear_model1, vcov = vcovHC(linear_model1, type="HC1")) #This is already a coefficient ds object. Like the one we constructed above. HC1 is the equivalent to stata's.

#The default type in R is HCW3
coeff_rse[,2]


#Simple Scatter Plot
p1<-ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point()

#Add linear model
p2<-ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point() +geom_smooth(method="lm", se=FALSE) #try adding se by changing to TRUE. 

#Add linear model
p3<-ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point() +geom_smooth(method="lm", se=TRUE) 

p4<-ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point() +geom_smooth(method="lm", se=TRUE) + labs(title="Regression line", y="Miles per Gallon", x="Horse Power") 

plot_grid(p1,p2,p3,p4)


#########

mod1<-lm(mpg~disp, data=mtcars)
print(mod1)
summary(mod1)

#And plot the fitted lines
lm1_plota<-ggplot(augment(mod1), aes(x=disp, y=mpg))+geom_point()+geom_line(aes(y=.fitted), size=1) + theme_bw()

#Using the broom output, you can do something like this for confidence intervals: 
lm1_plotb<-ggplot(augment(mod1), aes(x=disp, y=mpg)) + 
  geom_ribbon(aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2) +
  geom_point(aes(colour = mpg)) + 
  geom_line(aes(disp, .fitted, colour = .fitted)) +
  theme_bw() +theme(legend.position = "none")

plot_grid(lm1_plota, lm1_plotb)


#Subsetting the data frame to estimate model: 
mod1_cyl6<-lm(mpg~disp, data=mtcars[mtcars$cyl==6,])
print(mod1_cyl6)
summary(mod1_cyl6)


mod1_cyl8<-lm(mpg~disp, data=mtcars[mtcars$cyl==8,])
print(mod1_cyl8)
summary(mod1_cyl8)

#
stargazer(mod1_cyl6,mod1_cyl8 )


#And add interaction terms: 
mtcars$am<-factor(mtcars$am)
mtcars$am<-dplyr::recode(mtcars$am, "0"="Automatic", "1"="Manual")
mod2<-lm(mpg~disp+am, data = mtcars)
mod3<-lm(mpg~disp+am+disp:am, data = mtcars)
mod3<-lm(mpg~disp*am, data = mtcars)


#Plots
lm3a<-ggplot(augment(mod2), aes(x=disp, y=mpg, color=am)) + 
  geom_ribbon(aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2, linetype=0) +
  geom_point() + geom_line(aes(y=.fitted)) +
  theme_bw() +theme(legend.position = "none") + annotate("text", x=300, y=30, label= "mpg = b0 + b1*Disp + b3*Autom + e")


lm3b<-ggplot(augment(mod3), aes(x=disp, y=mpg, color=am)) + 
  geom_ribbon(aes(ymin=.fitted-1.96*.se.fit, ymax=.fitted+1.96*.se.fit), alpha=0.2, linetype=0) +
  geom_point() + geom_line(aes(y=.fitted)) + theme_bw() +theme(legend.position = "none") + annotate("text", x=300, y=30, label= "mpg = b0 + b1*D + b3*A + b4*D*A+ e")

plot_combined<-plot_grid(lm3a, lm3b)


############PRACTICE
data("PSID1982")
names(PSID1982)


PSID1982$married_n<-ifelse(PSID1982$married=="yes",1,0)
model_logit<-glm(married_n ~ education + experience , data=PSID1982)
margins_logit<-margins(model_logit, atmeans=c(education))
plot(margins_logit)

