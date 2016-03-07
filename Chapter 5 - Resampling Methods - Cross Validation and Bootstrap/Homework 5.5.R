library(ISLR)
View(Default)
data(Default)
attach(Default)
n = dim(Default)[1]
p = 2
set.seed(1)

test = rep(0.0,4)

# Sample 5,000 out of a vector of 10,000
train = sample(1:n, n/2)
glm.fit = glm(default~balance+income,family=binomial,subset=train)
glm.fit
glm.probs = predict(glm.fit,newdata = Default[-train,],type="response")
glm.preds = ifelse(glm.probs>0.5,"Yes","No")
mean(glm.preds=="Yes")
mean(glm.preds != Default[-train,"default"])
test[1] = mean(glm.preds != Default[-train,"default"])

train2 = sample(1:n, n/2)
glm.fit2 = glm(default~balance+income,family=binomial,subset=train2)
glm.fit2
glm.probs2 = predict(glm.fit2,newdata = Default[-train2,],type="response")
glm.preds2 = ifelse(glm.probs2>0.5,"Yes","No")
mean(glm.preds2=="Yes")
mean(glm.preds2 != Default[-train2,"default"])
test[2] = mean(glm.preds2 != Default[-train2,"default"])

train3 = sample(1:n, n/2)
glm.fit3 = glm(default~balance+income,family=binomial,subset=train3)
glm.fit3
glm.probs3 = predict(glm.fit3,newdata = Default[-train3,],type="response")
glm.preds3 = ifelse(glm.probs3>0.5,"Yes","No")
mean(glm.preds3=="Yes")
mean(glm.preds3 != Default[-train3,"default"])
test[3] = mean(glm.preds3 != Default[-train3,"default"])

train4 = sample(1:n, n/2)
glm.fit4 = glm(default~balance+income,family=binomial,subset=train4)
glm.fit4
glm.probs4 = predict(glm.fit4,newdata = Default[-train4,],type="response")
glm.preds4 = ifelse(glm.probs4>0.5,"Yes","No")
mean(glm.preds4=="Yes")
mean(glm.preds4 != Default[-train4,"default"])
test[4] = mean(glm.preds4 != Default[-train4,"default"])
mean(test)


# train = sample(dim(Default)[1], dim(Default)[1]/2)
# glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
#               subset = train)
# glm.pred = rep("No", dim(Default)[1]/2)
# glm.probs = predict(glm.fit, Default[-train, ], type = "response")
# glm.pred[glm.probs > 0.5] = "Yes"
# mean(glm.pred != Default[-train, ]$default)

test2 = rep(0,4)
for(i in 1:4){
   train = sample(n, n/2)
   glm.fit = glm(default~.,data=Default,family=binomial,subset=train)
   glm.probs = predict(glm.fit, newdata = Default[-train,],type="response")
   glm.preds = ifelse(glm.probs>.5,"Yes","No")
   test2[i] = mean(glm.preds != Default[-train,"default"])
}
test2
mean(test2)
