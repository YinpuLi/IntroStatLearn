x = seq(1,10,by=.25)
t = 4


ya = function(x,t){
   return(1 + x + ((x-t)*I(x > t)))
}

yb = function(x,t){
   return(1 + x + ((x-t)*I(x <= t)))
}

yc = function(x,t){
   return(1*I(x > t) + 1*I(x <= t) + ((x-t)*I(x > t)))
}

yd = function(x,t){
   return(1 + (x-t)*I(x<=t) + (x-t)*I(x>t))
}


par(mfrow=c(2,2))
plot(x,ya(x,t),type="l")
abline(v = t,col="red",lty="dashed")

plot(x,yb(x,t),type="l")
abline(v = t,col="red",lty="dashed")

plot(x,yc(x,t),type="l")
abline(v = t,col="red",lty="dashed")

plot(x,yd(x,t),type="l")
abline(v = t,col="red",lty="dashed")