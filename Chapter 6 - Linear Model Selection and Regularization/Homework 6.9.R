library(leaps)
library(glmnet)
library(pls)

data("College")
View(College)
dim(College)

n = nrow(College)
p = ncol(College)-1


# Check for missing data. There is none.
sum(is.na(College))


# Set up train and test logical vectors:
set.seed(1)
trainL=sample(c(TRUE,FALSE), nrow(College),rep=TRUE)
testL = (!trainL)

# Could also set them up this way using numerical vectors:
set.seed(1)
trainN=sample(1:n,n/2)
testN=(-trainN)

# We'll go with logical for this exercise:
train = trainL
test = testL
rm(trainN);rm(testN);rm(trainL);rm(testL)

# Get the Y into its own variable for easy access
y = College[,"Apps"]
trainY = y[train]
testY = y[test]


# Predict Apps using other features using least squares
lm.fit = lm(Apps~.,College,subset=train)
summary(lm.fit)
lm.pred = predict(lm.fit,newdata=College[test,])
lm.MSE = mean((lm.pred-testY)^2)

# Predict with null model
null.MSE = mean((mean(trainY)-testY)^2)

# Predict the number of applications received vs. other variables using
# Ridge regression.
grid = 10^seq(10,-2,length=100)
trainX = model.matrix(Apps~.,College[train,])[,-1]
testX = model.matrix(Apps~.,College[test,])[,-1]

# Cross validation to find best lambda. Using just training set.
cv.ridge = cv.glmnet(trainX, trainY, alpha=0)
plot(cv.ridge)
bestlam.ridge=cv.ridge$lambda.min

# Train ridge model on training set
ridge.mod = glmnet(trainX,trainY,alpha=0,lambda=grid)
ridge.pred = predict(ridge.mod,s=bestlam.ridge,newx=testX)

ridge.MSE = mean((ridge.pred-testY)^2)

# Lasso
cv.lasso=cv.glmnet(trainX,trainY,alpha=1)
plot(cv.lasso)
bestlam.lasso=cv.lasso$lambda.min

# Train the lasso model on the training set
lasso.mod = glmnet(trainX,trainY,alpha=1,lambda=grid)
lasso.pred = predict(lasso.mod,s=bestlam.lasso,newx=testX)
lasso.coef = predict(lasso.mod,s=bestlam.lasso,type = "coefficients")

lasso.MSE = mean((lasso.pred-testY)^2)
lasso.coef


# Principal Componenets Regression
pcr.fit = pcr(Apps~.,data=College[train,],scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")
# Lowest RMSE is 17, the full set, same as least sqaures
pcr.pred = predict(pcr.fit,testX,ncomp=17)
pcr.MSE=mean((pcr.pred-testY)^2)


# Partial Least Squares
pls.fit = plsr(Apps~.,data=College[train,],scale=TRUE,validation="CV")
summary(pls.fit)
# Lowest RMSE is 1090, which occurs at 10 and 13 comps. Will choose 10.
pls.pred = predict(pls.fit,testX,ncomp=10)
pls.MSE=mean((pls.pred-testY)^2)
pls.MSE
