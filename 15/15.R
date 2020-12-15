#star 1 
input <- rev(c(2,0,1,7,4,14,18))

van.eck <- function(n){
  i <- which.first(n[2:length(n)]==n[1])
  if(length(i) == 0){
    n <- c(0,n)
  }else{
    n <- c(i,n)
  }
  return(n)
}

n <- input
while(length(n)<2020){
  n <- van.eck(n)
}
print(n[1])



# star 2
input <- as.integer(c(2,0,1,7,4,14,18))

last.said <- rep(0, 3*10**7)
input[1:(length(input)-1)]+1

last.said[input[1:(length(input)-1)]+1] <- 1:(length(input)-1)

x <- tail(input, 1)
step<-length(input)

while(step <= 30000000){
if(last.said[x+1] == 0){
  last.said[x+1] <- step
  x <- 0
}else{
  rem.index <- x+1
  x <- step - last.said[x+1]
  last.said[rem.index] <- step
  
}
step <- step + 1
}
print(which(last.said == 30000000)-1)
