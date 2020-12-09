setwd("~/AoC/2020/09")
input <- unlist(read.table("./input09.txt"))

is.valid <- function(input, pos, prev){
  numbers <- unique(input[(pos-prev):(pos-1)])
  for( i in 1:(length(numbers)-1)){
    for(j in (i+1):length(numbers)){
      if(numbers[i] + numbers[j] == input[pos]){
        return(TRUE)
        break
      }
    }
  }
  return(FALSE)
}


#star 1
k <- 26
while(is.valid(input, pos = k, prev = 25)){
  k <- k+1
}
print(input[k])

#star 2
target <- input[k]
part.sum<-function(input, start, target){
  part.sum<-input[start]
  index <- start + 1
  while(part.sum<target){
    part.sum <- part.sum + input[index]
    index <- index + 1
  }
  if(part.sum == target){
    print(index-1)
    print(sum(range(input[start :(index-1)])))
    return(TRUE)
  }
  if(part.sum > target){
    return(FALSE)
}
}


test <- TRUE
start <- 1
while(test){
  print(start)
  test <- !(part.sum(input, start=start, target = target ))
  start <- start+1
}


