set.seed(1)
m = matrix(rnorm(60,mean=c(15,1,10000,18,42,587)),ncol=6,byrow=T)

set.seed(1)
km.unscaled.out = kmeans(m,4,nstart=20)

mscale = scale(m)
set.seed(1)
km.scaled.out = kmeans(mscale,4,nstart=20)

km.unscaled.out$cluster
km.scaled.out$cluster
