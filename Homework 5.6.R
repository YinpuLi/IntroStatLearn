library(ISLR)
library(boot)
data(Default)
attach(Default)

set.seed(1)

glm.fit = glm(default~income+balance,family=binomial)
coef(summary(glm.fit))[2:3,1:2]
coef(glm.fit)

boot.fn = function(data, index){
   return(
         coef(glm(default~income+balance
                      ,family=binomial
                      ,data=data
                      ,subset=index))
   )
}

boot(Default,boot.fn,1000)
