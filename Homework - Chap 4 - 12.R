Power <- function(x,a){
   print(x^a)
}

Power3 <- function(x,a){
   result = x^a
   return(result)
}

PlotPower <- function(x,a) {
   y = Power3(x,a)
   plot(x,y
        ,ylab = paste("x^",a,sep = "")
        ,xlab="x"
        #,log = "y"
        )
}