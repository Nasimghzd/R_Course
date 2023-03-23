
# Lab: Linear Models and Regularization Methods


## Subset Selection Methods


### Best Subset Selection

###
library(ISLR2)
?Hitters
View(Hitters)
names(Hitters)
dim(Hitters)
#is.na() returns TRUE if obs is missing or FALSE otherwise
sum(is.na(Hitters$Salary))
###
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
#
library(leaps)
# help
?regsubsets
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)
###
regfit.full <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary
###
names(reg.summary)
###
reg.summary$rsq
###
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
    ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
    ylab = "Adjusted RSq", type = "l")
###
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, 
    pch = 20)
###
plot(reg.summary$cp, xlab = "Number of Variables",
    ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2,
    pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables",
    ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2,
    pch = 20)
###
# return to c(1,1) due to scale
par(mfrow = c(1, 1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
###
# with bic optical model includes 6 predictors
# extract coef using id=6
coef(regfit.full, 6)

### Forward and Backward Stepwise Selection

###
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19, method = "backward")
summary(regfit.bwd)
###
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

### Choosing Among Models Using the Validation-Set Approach and Cross-Validation
### commands needed later
# %*% matrix multiplication (matrix times vector)
x<-c(1, 3, 5)
y<-c(0, 1, 0)
x*y #element by element
x%*%y  #inner product
x <- matrix(data = c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
x
x%*%y  #x=2x3 matrix, y=vector
x[1,]%*%y  #inner product, compare with x[1,]*y
x[2,]%*%y
# model.matrix -- creates design matrix
fit1<-lm(Salary~AtBat+Hits,data=Hitters)
fit1.mat<-model.matrix(fit1)
fit1.mat[1:10,]
# coef extractor, variable names, fitted values extractor
fit1.coef<-coef(fit1)
fit1.coef
names(fit1.coef)
names(fit1.coef)[2]
fit1.coef["AtBat"]
# fitted values yhat=Xb manually
fitted1<-fit1.mat%*%fit1.coef
#compare with fitted values extractor
cbind(fitted1, fitted(fit1))[1:5,]
#extract the formula of the fit1 object
names(fit1)
fit1$call
fit1$call[[1]]
fit1$call[[2]]    # we will use it
fit1$call[[3]]
###
# apply ... to avoid for loop
x <- matrix(data = c(1:9), nrow = 3, ncol = 3)
x
cmeans<-apply(x, 2, mean)
cmeans
rmeans<-apply(x, 1, mean)
rmeans
###
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
    replace = TRUE)
test <- (!train)
###
regfit.best <- regsubsets(Salary ~ .,
    data = Hitters[train, ], nvmax = 19)
###
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
###
# predict() method for regsubsets() is NOT available
# we do the test manually with a for() loop
val.errors <- rep(NA, 19)
for (i in 1:19) {
 coefi <- coef(regfit.best, id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
###
val.errors
which.min(val.errors)
coef(regfit.best, 7)
###
 predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
 }
###
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(regfit.best, 7)
###
# 10-folds cv
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
    dimnames = list(NULL, paste(1:19)))
###
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
       data = Hitters[folds != j, ],
       nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
         mean((Hitters$Salary[folds == j] - pred)^2)
   }
 }
###
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
###
reg.best <- regsubsets(Salary ~ ., data = Hitters,
    nvmax = 19)
coef(reg.best, 10)

## Ridge Regression and the Lasso

###
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

### Ridge Regression

###
library(glmnet)
grid <- 10^seq(10, -2, length = 100)
?glmnet
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge.mod, xvar="lambda", label=TRUE)
###
dim(coef(ridge.mod))
###
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2))
###
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2))
###
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
###
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
###
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
    lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
mean((mean(y[train]) - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
    exact = T, x = x[train, ], y = y[train])
## above try: ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ])
## ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact=T)
## then see that, when exact=T, the correct command is above.
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
    x = x[train, ], y = y[train])[1:20, ]
###
# use CV to choose best lambda.
# set seed so we can replicate, since CV is random splitting
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
###
ridge.pred <- predict(ridge.mod, s = bestlam,
    newx = x[test, ])
mean((ridge.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20, ]

### The Lasso

###
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
    lambda = grid)
plot(lasso.mod)
plot(lasso.mod, xvar="norm", label=TRUE)
plot(lasso.mod, xvar="lambda", label=TRUE)

###
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam,
    newx = x[test, ])
mean((lasso.pred - y.test)^2)
###
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients",
    s = bestlam)[1:20, ]
lasso.coef
lasso.coef[lasso.coef != 0]

