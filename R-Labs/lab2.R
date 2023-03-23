#if a library is not available, it needs to be installed first
# with install.packages("")
# for instance install.packages("ISLR2")
#library(MASS)
library(ISLR2)
# look at the dataset and descriptive statistics
names(Boston)
?Boston
head(Boston)
summary(Boston)
#
#
# Simple Linear Regression
# model: medv=β0 + β1*lstat + ε
fit1=lm(medv~lstat,data=Boston)
# fit1 does not give a lot of information
# summary(fit1) gives much more information
fit1
# check the regression output
summary(fit1)
#
names(fit1)
# 95% ci for each betahat
confint(fit1)
# check the function predict()
# predict can give fitted values
fv1<-predict(fit1)
fv2<-fitted(fit1)
head(cbind(fv1, fv2)) # same values 
# prediction for some new x-values
newd<-data.frame(lstat=c(5,10,15))
predict(fit1, newdata=newd)
# predictions and 95% ci for the average y
predict(fit1,newdata=newd,interval="confidence")
# predictions and 95% ci for the y-values: wider intervals
predict(fit1,newdata=newd,interval="prediction")
# evidence of non-linearity
plot(medv~lstat,Boston)
abline(fit1,col="red")
#four diagnostic plots
par(mfrow=c(2,2))
plot(fit1)  # gives diagnostic plots
par(mfrow=c(1,1))
#
#
# Multiple linear regression
# model: medv=β0 + β1*lstat + β2*age + ε
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
# use all 12 predictors
fit3=lm(medv~.,Boston)
summary(fit3)
# regress on all predictors except age and indus
fit4=lm(medv~.-age-indus, data=Boston)
summary(fit4)
#
#
# Nonlinear terms and Interactions
#
# model: medv=β0 + β1*lstat + β2*age + β3*lstat*age + ε
# interaction term lstat*age 
#to model synergy of the two predictors 
fit5=lm(medv~lstat+age+lstat:age,Boston)
summary(fit5)
# same as
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
#
# quadratic term: non-linear effects
# model: medv=β0 + β1*lstat + β2*lstat^2 + ε
fit6=lm(medv~lstat +I(lstat^2),Boston) 
summary(fit6)
# cubed term: non-linear effects
# model: medv=β0 + β1*lstat + β2*lstat^2 + β3*lstat^3 + ε
fit7=lm(medv~lstat +I(lstat^2) + I(lstat^3),Boston)
summary(fit7)
#use points() to plot fitted values on the same graph
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
points(lstat,fitted(fit7),col="blue",pch=20)
points(lstat,fitted(fit1),col="green",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)
#
#Qualitative predictors -- dummy variables
#
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
# consider factor variable with g levels
# (g-1) dummy variables are generated
contrasts(Carseats$ShelveLoc)
# check design matrix X
fit1.X<-model.matrix(fit1)
head(fit1.X)
#
###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)




