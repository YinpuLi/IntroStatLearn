library(boot)
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x,y)

iData = data.frame(x=x,y=y)

f1 = y~x
f2 = y~x+I(x^2)
f3 = y~x+I(x^2)+I(x^3)
f4 = y~x+I(x^2)+I(x^3)+I(x^4)

glm.fit1 = glm(f1,data=iData)
glm.fit2 = glm(f2,data=iData)
glm.fit3 = glm(f3,data=iData)
glm.fit4 = glm(f4,data=iData)

cvErr = rep(0,4)
cvErr[1] = cv.glm(iData,glm.fit1,K=dim(iData)[1])$delta[1]
cvErr[2] = cv.glm(iData,glm.fit2,K=dim(iData)[1])$delta[1]
cvErr[3] = cv.glm(iData,glm.fit3,K=dim(iData)[1])$delta[1]
cvErr[4] = cv.glm(iData,glm.fit4,K=dim(iData)[1])$delta[1]


set.seed(5)
glm.fit1 = glm(f1,data=iData)
glm.fit2 = glm(f2,data=iData)
glm.fit3 = glm(f3,data=iData)
glm.fit4 = glm(f4,data=iData)

cvErr = rep(0,4)
cvErr[1] = cv.glm(iData,glm.fit1,K=dim(iData)[1])$delta[1]
cvErr[2] = cv.glm(iData,glm.fit2,K=dim(iData)[1])$delta[1]
cvErr[3] = cv.glm(iData,glm.fit3,K=dim(iData)[1])$delta[1]
cvErr[4] = cv.glm(iData,glm.fit4,K=dim(iData)[1])$delta[1]
