f = function(b){
   # For p=1, y=1, lambda=1
   # Solving (y-b)^2 + lambda*b^2:
   2*(b^2) - 2*b + 1
}
plot(f,-10,11)
f2 = function(b){
   (b^2) - (2*b) + 1 + abs(b)
}
plot(f2,-2,2)
