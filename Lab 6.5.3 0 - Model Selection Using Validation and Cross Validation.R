library(ISLR)
library(leaps)
View(Hitters)
names(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# Command to create an "X" matrix. Does not include response, Y.
# Includes intercept term.
test.mat=model.matrix(Salary~.,data=Hitters[test,])
sum(test==TRUE)

val.errors=rep(NA,19)
for(i in 1:19){
   coefi=coef(regfit.best,id=i)
   # Matrix multiplication nxi * ix1 --> 1, end up
   # with one row per single prediction column. 
   pred=test.mat[,names(coefi)]%*%coefi
   val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets = function(object,newdata,id,...){
   form=as.formula(object$call[[2]])
   mat=model.matrix(form,newdata)
   coefi=coef(object,id=id)
   xvars=names(coefi)
   mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)


# Cross validation
# We must perform best subset selection within each of the k training sets.