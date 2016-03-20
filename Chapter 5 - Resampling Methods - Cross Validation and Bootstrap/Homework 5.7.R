library(ISLR)
library(boot)
data(Weekly)
attach(Weekly)
summary(Weekly)

glm.fit = glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.fit)

glm.fit = glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial)
summary(glm.fit)
glm.prob = predict(glm.fit,newdata = Weekly[1,],type="response")
glm.pred = ifelse(glm.prob>.5,"Up","Down")
(glm.pred == Weekly[1,"Direction"])
Weekly[1,"Direction"]

n = dim(Weekly)[1]
cvErr = rep(0,n)
for(i in 1:n){
   glm.fit = glm(Direction~Lag1+Lag2
                 ,data=Weekly
                 ,family=binomial
                 ,subset=-i)
   cvErr[i] = (ifelse(predict(glm.fit,Weekly[i,],type="response")>.5
                     ,"Up"
                     ,"Down") 
               != Weekly[i,"Direction"])
}

mean(cvErr)