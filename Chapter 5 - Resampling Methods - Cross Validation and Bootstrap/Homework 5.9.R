library(MASS)
library(boot)

data(Boston)
attach(Boston)
set.seed(1)

muHat = mean(medv)
se = sd(medv)/sqrt(nrow(Boston))
se*1000

boot.fn = function(data,index){
   return(mean(data[index]))
}

boot(medv,boot.fn,1000)

bse = 0.4119374
muHat + c(-1,1)*2*bse
t.test(medv)

medv.med = median(medv)

boot.fn = function(data,index){
   return(median(data[index]))
}

bst = boot(medv,boot.fn,1000)
bst

boot.fn = function(data,index){return(quantile(data[index],.1))}
bst = boot(medv,boot.fn,1000)
bst
