library(ISLR)
library(boot)

attach(Auto)

glm.fit = glm(mpg~horsepower,data=Auto)
cv.err = cv.glm(Auto,glm.fit)
cv.err$delta

cv.error = rep(0,5)
# Takes a few secs to run
for(i in 1:5){
   glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
   cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}