setwd("~/AoC/2020/14")

input <- readLines("./input14.txt")
#input <- readLines("./input14small.txt")

library("stringr")
library("binaryLogic")
library("gmp")
library("bit64")

apply.mask <- function(number, mask){
  mask <- unlist(strsplit(mask, split = ""))
  number <- as.binary(number, n = 36, logic = F)
  for(i in 1:36){
    if(mask[i] == "X"){
      #do nothing
    }else{
      number[i]<-as.numeric(mask[i])
    }
  }
  x <- (sum(as.numeric(as.character(number)) * 2**(35:0)))
  return(x)
}

# returns all position in memory according to mask and number
apply.memory.mask <- function(number, mask){
  memories <- c()
  mask <- unlist(strsplit(mask, split = ""))
  number <- as.character(as.binary(number, n = 36, logic = F))
  for(i in 1:36){
    if(mask[i] == "X"){
      number[i] <- "X"
    }else if (mask[i] == "1"){
      number[i]<-"1"
    }
  }
  floats <- which(number == "X")
  floats.n <- 2**length(floats)
  for(i in 0:(floats.n-1)){
    n <- number
    n[floats] <- 0
    n[floats[as.binary(i, n=length(floats))]] <- 1
    x <- sum(as.numeric(as.character(n)) * 2**(35:0))
    memories <- c(memories, x)
  }
  return(memories)
}

#star 1
memory <- c()
for ( line in input){
if (substr(line, 1, 4) == "mask") {
  mask <- substr(line, 8, nchar(line))
} else{
  start.split <- which(unlist(strsplit(line, split = "")) == "[") + 1
  end.split <- which(unlist(strsplit(line, split = "")) == "]") - 1
  mem <- as.numeric(substr(line, start = start.split, stop = end.split))
  value <- as.numeric(unlist(strsplit(line, split = "="))[2])
  x <- apply.mask(value, mask)
  memory[mem] <- x
}
}

print(as.character(sum(memory, na.rm = T)))

#star2
# run 1: how many memory points do we need?
memes <- c()
for ( line in input){
  if (substr(line, 1, 4) == "mask") {
    mask <- substr(line, 8, nchar(line))
  } else{
    start.split <- which(unlist(strsplit(line, split = "")) == "[") + 1
    end.split <- which(unlist(strsplit(line, split = "")) == "]") - 1
    mem <- as.numeric(substr(line, start = start.split, stop = end.split))
    value <- as.numeric(unlist(strsplit(line, split = "="))[2])
    x <- apply.memory.mask(mem, mask)
    memes<-unique(c(memes, x))
  }
}

#init memory
memory <- cbind(sort(memes), rep(0, length(memes)))

#2nd run; fill memory
for ( line in input){
  if (substr(line, 1, 4) == "mask") {
    mask <- substr(line, 8, nchar(line))
  } else{
    start.split <- which(unlist(strsplit(line, split = "")) == "[") + 1
    end.split <- which(unlist(strsplit(line, split = "")) == "]") - 1
    mem <- as.numeric(substr(line, start = start.split, stop = end.split))
    value <- as.numeric(unlist(strsplit(line, split = "="))[2])
    mems <- apply.memory.mask(mem, mask)
    for(m in mems){
      memory[which(memory[,1] == m),2] <- value
    }
  }
}
print(as.character(sum(memory[,2])))
