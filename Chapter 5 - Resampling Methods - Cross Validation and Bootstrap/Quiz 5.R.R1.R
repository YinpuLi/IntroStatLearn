load("/Users/bpafoshizle/Downloads/5.R.RData")
fit = glm(data=Xy,formula = y~X1+X2)
summary(fit)
matplot(Xy,type="l")
daily.pnl <- Xy
nn <- ncol(daily.pnl)
legend("top", colnames(daily.pnl),col=seq_len(nn),cex=0.8,fill=seq_len(nn))
summary(Xy)

bootFn = function(data){
   coef(glm(y~X1+X2,data=data))
}


newXy = 

require(boot)
bootR = boot(Xy,bootFnTs,1000)
bootR

tsBootR = tsboot(Xy,bootFn,sim = "fixed",l=100,R=1000)
tsBootR

index = 1
index = index:(index+100)
