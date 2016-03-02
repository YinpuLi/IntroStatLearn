library(leaps)
source("~/github/local/IntroStatLearn/Utils.R")

n = 1000
p = 20

set.seed(1)
x = matrix(rnorm(20*1000),nrow = 1000, ncol = 20)
colnames(x) = paste("X",seq(1:20),sep="")
beta = rnorm(20)
names(beta) = paste("X",seq(1:20),sep="")
beta[sample(c(TRUE,FALSE),size=20,replace=TRUE,prob=c(.25,.75))]=0
eps = rnorm(1000)
y = as.vector(x%*%beta) + eps

train = sample(1000,size=100)
test = -train

trainX = x[train,]
trainY = y[train]

testX = x[test,]
testY = y[test]

trainFrm = data.frame(trainX,y=trainY)
testFrm = data.frame(testX,y=testY)

best.fit = regsubsets(y~., data=trainFrm,nvmax = 20)
best.sum = summary(best.fit)
names(best.sum)

bestTrainMSE = rep(0,20)
for(i in 1:20){
   best.pred = predict.regsubsets(best.fit,newdata=trainFrm,id=i)
   bestTrainMSE[i] = mean((best.pred-trainY)^2)
}
plot(bestTrainMSE)


bestTestMSE = rep(0,20)
for(i in 1:20){
   best.pred = predict.regsubsets(best.fit,newdata=testFrm,id=i)
   bestTestMSE[i] = mean((best.pred-testY)^2)
}
points(bestTestMSE,col="red")
points(x=which.min(bestTestMSE),y=bestTestMSE[which.min(bestTestMSE)],
       pch=4,col="red",lwd=4)

coef(best.fit,id=15)
beta

coef(best.fit,id=1)[-1]

rootSumSqCoefDiff = rep(0,20)


for(i in 1:20){
   coefi = coef(best.fit,id=i)[-1]
   namesi = names(coefi)
   rootSumSqCoefDiff[i] = sqrt(sum(
                                    (coefi[namesi] - beta[namesi])^2
                               ) 
                               + 
                                 # Add all the squared beta - 0 for those not in model with i coefs
                               sum(
                                    beta[!(names(beta) %in% namesi)]^2
                               )
                           )
}
plot(rootSumSqCoefDiff)
