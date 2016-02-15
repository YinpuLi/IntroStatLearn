library(ISLR)
library(MASS)
library(class)

train = (Weekly$Year < 2009)
Weekly.Lag2.test = data.frame(Lag2=Weekly[!train,"Lag2"])
Weekly.Direction = Weekly[!train, "Direction"]

glm.fit2 = glm(Direction~Lag2, data=Weekly, family=binomial, subset = train)
summary(glm.fit2)
glm.fit2.probs = predict(glm.fit2,newdata = Weekly.Lag2.test, type="response")
glm.fit2.preds = rep("Down",104)
glm.fit2.preds[glm.fit2.probs>.5] = "Up"
table(glm.fit2.preds, Weekly.Direction)
glm.tn = 9/(9+5)
glm.fn = 1-glm.tn
glm.tp = 56/(56+34)
glm.fp = 1-glm.tp
mean(glm.fit2.preds == Weekly.Direction)


lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
summary(lda.fit)
lda.fit
lda.preds = predict(lda.fit,Weekly.Lag2.test)
lda.class = lda.preds$class
table(lda.class, Weekly.Direction)
lda.tn = 9/(9+5)
lda.fn = 1-lda.tn
lda.tp = 56/(56+34)
lda.fp = 1-lda.tp
mean(lda.class == Weekly.Direction)

qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.fit
qda.preds = predict(qda.fit,Weekly.Lag2.test)
qda.class = qda.preds$class
table(qda.class, Weekly.Direction)
qda.tn = 0
qda.fn = 0
qda.tp = 61/(61+43)
qda.fp = 1-qda.tp

knn.train.x.v = Weekly$Lag2[train]
knn.train.x = matrix(Weekly[train,"Lag2"])
knn.test.x = matrix(Weekly[!train,"Lag2"])
knn.train.y = Weekly[train,"Direction"]
set.seed(1)
knn.pred = knn(knn.train.x.v, knn.test.x, knn.train.y, k=1)
table(knn.pred, Weekly.Direction)
knn.tn = 21/(21+30)
knn.fn = 1-knn.tn
knn.fp = 22/(22+31)
knn.tp = 1-knn.fp
mean(knn.pred == Weekly.Direction)


glm.fit = glm(Direction~.-Today-Year, data=Weekly, family=binomial, subset = train)
glm.fit.probs = predict(glm.fit,Weekly[!train,],type="response")
glm.fit.preds = rep("Down", 104)
glm.fit.preds[glm.fit.probs>.5]="Up"
table(glm.fit.preds,Weekly.Direction)
mean(glm.fit.preds == Weekly.Direction)
glm.tn = 31/(31+44)
glm.fn = 1-glm.tn
glm.fp = 12/(12+17)
glm.tp = 1-glm.fp


glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family=binomial, subset = train)
summary(glm.fit)
glm.fit.probs = predict(glm.fit,Weekly[!train,c("Lag1","Lag2")],type="response")
glm.fit.preds = rep("Down", 104)
glm.fit.preds[glm.fit.probs>.5]="Up"
mean(glm.fit.preds == Weekly.Direction)
table(glm.fit.preds, Weekly.Direction)
36/(36+53)


knn.train.x = matrix(Weekly[train,c("Lag1")])
knn.test.x = matrix(Weekly[!train,c("Lag1")])
knn.train.y = Weekly[train,"Direction"]
set.seed(1)
knn.pred = knn(knn.train.x, knn.test.x, knn.train.y, k=3)
table(knn.pred, Weekly.Direction)
mean(knn.pred == Weekly.Direction)
