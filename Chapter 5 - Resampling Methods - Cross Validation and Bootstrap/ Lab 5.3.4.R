library(ISLR)
library(boot)


# This function returns, or outputs, an estimate for α based on applying (5.7) to 
# the observations indexed by the argument index
alpha.fn = function(data,index){
   X=data$X[index]
   Y=data$Y[index]
   return((var(Y)-cov(X,Y))/(var(Y)+var(X)-2*cov(X,Y)))
}

# The following command tells R to estimate α using all 100 observations.
alpha.fn(Portfolio,1:100)

# The next command uses the sample() function to randomly select 100 
# observations from the range 1 to 100, with replacement. This is equivalent to 
# constructing a new bootstrap data set and recomputing αˆ based on the new 
# data set.
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

# We can implement a bootstrap analysis by performing this command many times, 
# recording all of the corresponding estimates for α, and computing the resulting 
# standard deviation.
boot(Portfolio,alpha.fn,R=1000)


boot.fn = function(data, index)
   return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))


boot(Auto,boot.fn,R=1000)
summary(lm(mpg~horsepower ,data=Auto))$coef


boot.fn = function(data,index)
   return(coef(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index)))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
