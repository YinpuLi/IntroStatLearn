library(leaps)

X = rnorm(100)
e = rnorm(100)

beta0 = .5
beta1 = 1
beta2 = 2
beta3 = 3

Y = beta0 + (beta1*X) + (beta2*(X^2)) + (beta3*(X^3)) + e

data = data.frame("X"=X,"Y"=Y)
f = as.formula(Y~poly(X,10,raw=TRUE))

# Best subsets regression
best.fit = regsubsets(f,data=data,nvmax=10)
best.summary=summary(best.fit)
names(best.summary)

par(mfrow=c(2,2))

best.summary$cp
plot(best.summary$cp,type="l")
points(best.summary$cp,pch=20)
which.min(best.summary$cp)
#standardErrorRule = lowestSDIdx(best.summary$cp)
points(6, best.summary$cp[6], pch = 4, col = "red", lwd = 7)

best.summary$bic
which.min(best.summary$bic)
plot(best.summary$bic,type="l")
points(best.summary$bic,pch=20)
points(3, best.summary$bic[3], pch = 4, col = "red", lwd = 7)

best.summary$adjr2
which.max(best.summary$adjr2)
plot(best.summary$adjr2,type="l")
points(best.summary$adjr2,pch=20)
points(6, best.summary$adjr2[6], pch = 4, col = "red", lwd = 7)

coef(best.fit,3)
coef(best.fit,6)

# Foreward stepwise regression
fit.fwd = regsubsets(f,data=data,nvmax=10,method="forward")
plotRegSubSet(fit.fwd)
coef(fit.fwd,6)
coef(fit.fwd,3)

fit.bwd = regsubsets(f,data=data,nvmax=10,method="backward")
plotRegSubSet(fit.bwd)

# Lasso Regression
library(glmnet)
Xm = model.matrix(f,data=data)[,-1]
y = data$Y
grid=10^seq(10,-2,length=100)
cv.lasso = cv.glmnet(Xm,y,alpha=1)
plot(cv.lasso)
bestlam=cv.lasso$lambda.min
fit.lasso = glmnet(Xm,y,alpha=1,lambda=grid)
lasso.coef = predict(fit.lasso,type="coefficients",s=bestlam)
lasso.coef

# This doesn't work, because the CV method's lambda grid is not the same as 
# the grid I supplied. Above, it must interpolate at the given value of s, 
# rather than storing it. 
coef(fit.lasso)[,which.min(cv.lasso$lambda)]
cv.lasso$lambda.min
minLambdaIdx = which(cv.lasso$lambda == cv.lasso$lambda.min)
dim(coef(fit.lasso))
coef(fit.lasso)[,52]


# Part D
beta7 = .25
Y = beta0 + beta7*(X^7) + e 
data2 = data.frame(Y=Y,X=X)

# Best subsets. Bayesian IC finds it. Amazing.
best.fit = regsubsets(f,data=data2,nvmax=10)
best.fit
best.summary = summary(best.fit)
best.summary
which.min(best.summary$bic)
coef(best.fit,1)
coef(best.fit,4)
coef(best.fit,6)
par(mfrow=c(1,1))
plot(best.summary$bic,type = "l")
points(best.summary$bic,pch=20)
points(1,best.summary$bic[1],pch=4,col="red",lwd=7)
plotRegSubSet(best.fit)

# Lasso.
set.seed(1)
y=data2$Y
Xm = model.matrix(f,data2)[,-1]
cv.lasso = cv.glmnet(Xm,y)
bestlam = cv.lasso$lambda.min
bestlam
lasso.fit = glmnet(Xm,y,lambda=grid)
lasso.coef = predict(lasso.fit,type="coefficients",s = bestlam)
lasso.coef
