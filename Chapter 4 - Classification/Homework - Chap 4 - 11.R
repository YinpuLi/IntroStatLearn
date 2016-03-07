library(MASS)
library(class)
library(ISLR)

View(Auto)

Auto2 <- Auto
Auto2$mpg01 = 0
Auto2[Auto$mpg > median(Auto$mpg),"mpg01"] = 1

plot(Auto2)
# Looks like low displacement, low horsepower, low weight, high acceleration are associated with high gas milage,
# cylinders and year as well when you look at boxplot.
attach(Auto2)
Auto2$mpg01 = as.factor(mpg01)
boxplot(displacement~mpg01)
boxplot(horsepower~mpg01)
boxplot(weight~mpg01)
boxplot(acceleration~mpg01)
boxplot(cylinders~mpg01)
boxplot(origin~mpg01)
boxplot(year~mpg01)

# Sample 80% for train set
Auto2.train.idx = sample(nrow(Auto2),size=nrow(Auto2)*.8)
Auto2.train = Auto2[Auto2.train.idx, ]
Auto2.test = Auto2[-Auto2.train.idx, ]
Auto2.test.Y = Auto2.test[,"mpg01"]

lda.fit = lda(mpg01~.-name-mpg,data=Auto2.train)
lda.fit
lda.pred = predict(lda.fit, newdata = Auto2.test)
lda.class = lda.pred$class
mean(lda.class == Auto2.test.Y)
mean(lda.class != Auto2.test.Y)
mean("1" == Auto2.test.Y)
table(lda.class, Auto2.test.Y)


qda.fit = qda(mpg01~.-name-mpg,data=Auto2.train)
qda.fit
qda.class = predict(qda.fit,Auto2.test)$class
mean(qda.class != Auto2.test.Y)
mean(qda.class == Auto2.test.Y)


glm.fit = glm(mpg01~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto2.train,family=binomial)
glm.probs = predict(glm.fit,Auto2.test[,2:8],type="response")
glm.preds = rep("0",nrow(Auto2.test))
glm.preds[glm.probs>.5]="1"
glm.preds = as.factor(glm.preds)
mean(glm.preds != Auto2.test.Y)
table(glm.preds, Auto2.test.Y)


knn.train.X = as.matrix(Auto2.train[,2:8])
knn.train.Y = as.matrix(Auto2.train[,"mpg01"])
knn.test.X = as.matrix(Auto2.test[,2:8])
knn.preds.k1 = knn(knn.train.X, knn.test.X, knn.train.Y, k=1)
knn.preds.k3 = knn(knn.train.X, knn.test.X, knn.train.Y, k=3)
knn.preds.k5 = knn(knn.train.X, knn.test.X, knn.train.Y, k=5)
knn.preds.k10 = knn(knn.train.X, knn.test.X, knn.train.Y, k=10)

result = rbind(
      c("k=1", mean(knn.preds.k1 == Auto2.test.Y)),
      c("k=3", mean(knn.preds.k3 == Auto2.test.Y)),
      c("k=5", mean(knn.preds.k5 == Auto2.test.Y)),
      c("k=10", mean(knn.preds.k10 == Auto2.test.Y))
        )