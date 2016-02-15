store = rep(NA, 10000)
for(i in 1:10000){
   # Sample 100 samples from a set of 1-100. If 4th is in there at all
   # then we count as 1 success. 
   store[i] = sum(sample(1:100, rep=TRUE)==4)>0
}
mean(store)
