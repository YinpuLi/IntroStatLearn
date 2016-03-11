# Function to take a vector and return
# the lowest index of the value that is 
# within one standard deviation of the lowest.
lowestSDIdx = function(v){
   sdv = sd(v)/sqrt(length(v))
   print(sdv)
   minv = min(v)
   high = sdv + minv
   lowestIdx = v[(v < high)][1]
   if(length(lowestIdx) == 0){
      return(which.min(v))
   }
   else{
      return(lowestIdx)
   }
}

plotRegSubSet = function(reg.fit){
   par(mfrow=c(2,2))
   reg.sum = summary(reg.fit)
   
   reg.sum$cp
   bestIdx = which.min(reg.sum$cp)
   plot(reg.sum$cp,type="l");points(reg.sum$cp,pch=20)
   points(bestIdx,reg.sum$cp[bestIdx],pch=4,col="red",lwd=7)
   
   reg.sum$bic
   bestIdx = which.min(reg.sum$bic)
   plot(reg.sum$bic,type="l");points(reg.sum$bic,pch=20)
   points(bestIdx,reg.sum$bic[bestIdx],pch=4,col="red",lwd=7)
   
   reg.sum$adjr2
   bestIdx = which.max(reg.sum$adjr2)
   plot(reg.sum$adjr2,type="l");points(reg.sum$adjr2,pch=20)
   points(bestIdx,reg.sum$adjr2[bestIdx],pch=4,col="red",lwd=7)
}

predict.regsubsets = function(object,newdata,id,...){
   form=as.formula(object$call[[2]])
   mat=model.matrix(form,newdata)
   coefi=coef(object,id=id)
   xvars=names(coefi)
   mat[,xvars]%*%coefi
}

findCuts = function(v,n) {
   if(n<2) return(NA)
   minVal = range(v)[1] - .1
   maxVal = range(v)[2] + .1
   internalCutIdx = seq(1:(n-1)) * floor(length(v)/n)
   return(c(minVal, v[order(v)][internalCutIdx], maxVal))
}

probFromLogit = function(z){
   exp(z)/(1+exp(z))
}