require(ISLR2)
# Dataset Smarket
names(Smarket)
?Smarket
dim(Smarket)
tail(Smarket)
head(Smarket)
# Descriptive analysis
summary(Smarket)
pairs(Smarket,col=Smarket$Direction)
cor(Smarket)
cor(Smarket[,-9])
plot(Smarket$Lag1, Smarket$Today, col=Smarket$Direction)
plot(Smarket$Year, Smarket$Volume, col=Smarket$Direction)
#
## Logistic regression
class(Smarket$Direction)
contrasts(Smarket$Direction)
glm.fit5=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
# extract elements from "fit" and "summary" objects
names(glm.fit5)
coef(glm.fit5)
fitted(glm.fit5)[1:5]
summary(glm.fit5)$coef
# prediction of probabilities
glm.probs=predict(glm.fit5,type="response") 
glm.probs[1:5]
# class-labels prediction with cutoff point 0.5
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
data.frame(Smarket$Direction,glm.probs, glm.pred)[1:5,]
attach(Smarket)
# confusion matrix
table(glm.pred,Direction)
(145+507)/1250
mean(glm.pred==Direction)
# logical or Boolean vectors with values TRUE or FALSE
re<-(glm.pred==Direction)
re[1:5]
re[1:5]*1
sum(glm.pred==Direction)
# error rate
(141+457)/1250
# Make training and test set
train = Year<2005
# fit using training subset
glm.fit5=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)
# predict Direction for Year 2005 test data
glm.probs=predict(glm.fit5,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
# confusion matrix: prediction performance on test set 
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#Fit smaller model
glm.fit2=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit2,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
# smaller model has better predictions, smaller test MSE 
mean(glm.pred==Direction.2005)
# much better performance when prediction is "Up"
106/(76+106)
# not so good when prediction is "Down"
35/(35+35)
# predictions
newd=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8))
predict(glm.fit2, newdata=newd, type="response")
#
## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
train.X<-Xlag[train,]
test.X<-Xlag[!train,]
train.Direction<-Direction[train]
test.Direction<-Direction[!train]
# K=1 :overly flexible
set.seed(1)  # for reproducibility
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)
# K=3 :less flexible
set.seed(1)  # for reproducibility
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)
set.seed(1)  # for reproducibility
knn.pred=knn(train.X,test.X,train.Direction,k=5)
table(knn.pred,test.Direction)
mean(knn.pred==test.Direction)

