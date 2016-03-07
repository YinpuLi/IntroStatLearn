pm1 = seq(0,1,by=.01)
pm2 = 1-pm1
cer = ifelse(pm1<pm2,pm1,pm2)
gini = pm1*(1-pm1) + pm2*(1-pm2)
cent = -1* (pm1*log(pm1) + pm2*log(pm2))

plot(1,xlim=c(0,1),ylim=c(0,1), type="n",xlab="",ylab="")
lines(pm1,cer)
lines(pm1,gini,col="pink",lwd=3)
lines(pm1,cent,col="green",lwd=2,lty="dashed")
legend("topright",legend=c("Classification Error", "Gini", "Cross Entropy"), col=c("black","pink","green"),lty = c(1,1,2))
